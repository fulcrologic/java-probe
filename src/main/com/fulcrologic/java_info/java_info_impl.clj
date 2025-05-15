(ns com.fulcrologic.java-info.java-info-impl
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [taoensso.encore :as enc])
  (:import
    (com.github.javaparser StaticJavaParser)
    (com.github.javaparser.ast.body ClassOrInterfaceDeclaration MethodDeclaration)
    (com.github.javaparser.javadoc Javadoc JavadocBlockTag)
    (com.github.javaparser.javadoc.description JavadocDescription JavadocDescriptionElement)
    (io.github.classgraph ClassGraph ClassInfo)
    (java.io File InputStream)
    (java.nio.file Paths)
    (java.util Optional)
    (java.util.jar JarFile)
    (java.util.regex Pattern)
    (org.jsoup Jsoup)))

(defn is-jdk-class? [class-name]
  "Determines if a class is part of the standard JDK.

   Parameters:
   - class-name: A string containing the fully-qualified class name.

   Returns:
   Boolean - true if the class is part of the standard JDK, false otherwise."
  (let [prefixes ["java." "javax." "sun." "com.sun." "jdk." "org.w3c." "org.xml."]]
    (some #(.startsWith class-name %) prefixes)))

(defn jar-path [^String class-name]
  "Retrieves the JAR file path for a given class.

   Parameters:
   - class-name: A string containing the fully-qualified class name.

   Returns:
   File object representing the JAR file containing the class, or nil if the class cannot be found
   or is not in a JAR (e.g., it might be in a directory or built-in to the JVM)."
  (try
    (-> (Class/forName class-name)
      .getProtectionDomain
      .getCodeSource
      .getLocation
      .toURI
      Paths/get
      .toFile)
    (catch Exception _ nil)))

(defn parse-maven-coords-from-path [^File jar-file]
  "Parses Maven coordinates (group-id, artifact-id, version) from a JAR file path.

   Parameters:
   - jar-file: A File object representing the JAR file path in a Maven repository.

   Returns:
   A map containing :group-id, :artifact-id, and :version keys if successful,
   or nil if parsing fails. The path is expected to follow Maven repository structure
   (group/id/paths/artifact-id/version/artifact-id-version.jar)."
  (let [path-str (str/replace (str jar-file) #"^.*/repository/" "")
        parts    (vec (str/split path-str (re-pattern (Pattern/quote "/"))))]
    (try
      (let [len         (count parts)
            _           (when (< len 4)
                          (throw (ex-info "Insufficient path parts"
                                   {:path   path-str
                                    :parts  parts
                                    :length len})))
            version     (nth parts (- len 2))
            artifact-id (nth parts (- len 3))
            group-parts (subvec parts 0 (- len 3))
            group-id    (str/join "." group-parts)]
        {:group-id    group-id
         :artifact-id artifact-id
         :version     version})
      (catch Exception e
        (println "Error parsing Maven coordinates:"
          {:path  path-str
           :parts parts
           :error (.getMessage e)})
        nil))))

(defn download-sources [{:keys [group-id artifact-id version] :as coords}]
  "Downloads source JAR for a Maven artifact using the mvn dependency:get command.

   Parameters:
   - coords: A map containing Maven coordinates with keys :group-id, :artifact-id, and :version.

   Returns:
   nil if successful.

   Throws:
   Exception if the download fails for any reason."
  (let [artifact-str (format "%s:%s:%s:jar:sources" group-id artifact-id version)]
    (try
      (let [{:keys [exit out err]} (sh "mvn" "dependency:get"
                                     (str "-Dartifact=" artifact-str))]
        (when-not (zero? exit)
          (throw (ex-info (str "Failed to download sources: " out) {}))))
      (catch Exception e
        (throw (ex-info "Error downloading sources"
                 {:coords coords} e))))))

(defn class-to-path [^String class-name]
  "Converts a fully-qualified class name to a file path format by replacing dots with slashes.

   Parameters:
   - class-name: A string containing the fully-qualified class name (e.g., 'java.util.List')

   Returns:
   A string with the class name converted to a path format (e.g., 'java/util/List')"
  (str/replace class-name "." "/"))

(defn find-source-jar-path ^File [{:keys [group-id artifact-id version]}]
  "Finds the path to a source JAR file in the local Maven repository.

   Parameters:
   - A map containing Maven coordinates with keys :group-id, :artifact-id, and :version.

   Returns:
   A File object representing the source JAR file if it exists, nil otherwise.
   The function constructs the expected path based on Maven repository conventions."
  (let [base-path (str (System/getProperty "user.home")
                    "/.m2/repository/"
                    (str/replace group-id "." "/") "/"
                    artifact-id "/" version "/"
                    artifact-id "-" version "-sources.jar")]
    (let [f (io/file base-path)]
      (when (.exists f) f))))

(defn ensure-sources ^File [{:keys [group-id artifact-id version] :as coords}]
  "Ensures source JAR is available locally, downloading it if necessary.

   Parameters:
   - coords: A map containing Maven coordinates with keys :group-id, :artifact-id, and :version.

   Returns:
   A File object representing the path to the source JAR file.

   If the source JAR doesn't exist locally, it will attempt to download it
   using Maven and then return the path to the downloaded file."
  (or (find-source-jar-path coords)
    (do
      (download-sources coords)
      (find-source-jar-path coords))))

(defn strip-html [^String s]
  "Removes HTML markup from a string, returning only the text content.

   Parameters:
   - s: A string that may contain HTML markup.

   Returns:
   A string with all HTML tags removed, or nil if the input is nil."
  (when s
    (.text (Jsoup/parse s))))

(defn format-description [^JavadocDescription description]
  "Formats a JavaParser JavadocDescription object into a clean string.

   Parameters:
   - description: A JavadocDescription object from the JavaParser library.

   Returns:
   A formatted string with all of the description elements joined together
   with spaces, and with any HTML tags removed from each element."
  (->> description
    .getElements
    (map (fn [^JavadocDescriptionElement e] (strip-html (.toText e))))
    (str/join " ")))

(defn oo [^Optional v default]
  "Safely unwraps a Java Optional, returning the contained value or a default.

   Parameters:
   - v: A Java Optional object
   - default: The value to return if the Optional is empty

   Returns:
   The contained value if the Optional is present, otherwise the default value."
  (if (.isEmpty v)
    default
    (.get v)))

(defn tag-name [^JavadocBlockTag t]
  "Gets the name of a JavadocBlockTag.

   Parameters:
   - t: A JavadocBlockTag object from the JavaParser library

   Returns:
   The tag name as a string (e.g., 'param', 'return', etc.)"
  (.getTagName t))

(defn find-method-source [^ClassOrInterfaceDeclaration clazz ^String method-name]
  "Finds and returns the source code for methods with the specified name in a class.

   Parameters:
   - clazz: A ClassOrInterfaceDeclaration object from the JavaParser library
   - method-name: A string containing the name of the method to find

   Returns:
   A string containing the source code of all methods matching the given name,
   with each method separated by a newline. Returns nil if the class is nil or
   no matching methods are found."
  (when clazz
    (let [methods (.getMethods clazz)]
      (str/join "\n"
        (into []
          (comp
            (filter #(= method-name (.getNameAsString %)))
            (map str))
          methods)))))

(defn extract-method-docs
  "Extracts documentation for a Java method from its Javadoc comments.

   Parameters:
   - method: A MethodDeclaration object from the JavaParser library

   Returns:
   A map containing the following keys:
   - :name - The name of the method
   - :declaration - The full method declaration as a string
   - :description - The main description from the Javadoc, or empty string if none
   - :params - A map from parameter names to their descriptions, or empty map if none
   - :return - The return value description, or nil if none

   If the method has no Javadoc, default empty values are provided for the documentation fields."
  [^MethodDeclaration method]
  (let [javadoc  (.getJavadoc method)
        decl     (.getDeclarationAsString method)
        doc-data (if (.isPresent javadoc)
                   (let [^Javadoc doc (.get javadoc)
                         tags         (.getBlockTags doc)
                         param-tags   (filter #(= (tag-name %) "param") tags)
                         return-tag   (first (filter #(= (tag-name %) "return") tags))]
                     (cond-> {:description (format-description (.getDescription doc))}
                       (seq param-tags) (assoc :params (into {}
                                                         (map (fn [^JavadocBlockTag tag]
                                                                [(oo (.getName tag) "") (format-description (.getContent tag))])
                                                           param-tags)))
                       return-tag (assoc :return (format-description (.getContent return-tag)))))
                   ;; Set empty values if no javadoc
                   {:description ""
                    :params      {}
                    :return      nil})]
    (merge doc-data {:name        (.getNameAsString method)
                     :declaration decl})))

(defn source-jar ^File [class-name]
  "Locates the source JAR file for a given class, downloading it if necessary.

   Parameters:
   - class-name: A string containing the fully-qualified class name.

   Returns:
   A File object representing the source JAR file, or nil if:
   - The class couldn't be found
   - The class's JAR location couldn't be determined
   - Maven coordinates couldn't be parsed from the JAR path
   - The source JAR couldn't be found or downloaded

   This function attempts to find the JAR containing the class, parse its Maven
   coordinates, and then ensure the corresponding source JAR is available."
  (enc/when-let [jar-file (jar-path class-name)
                 coords   (parse-maven-coords-from-path jar-file)
                 src-jar  (ensure-sources coords)]
    src-jar))

(defn find-entry-in-jar [^JarFile jar ^String relative-path]
  "Tries to find an entry in a JAR file, handling different Java module path structures.

   Parameters:
   - jar: A JarFile object representing the JAR file to search
   - relative-path: A string containing the relative path to the file within the JAR

   Returns:
   A JarEntry object if the file is found, nil otherwise.

   This function attempts several strategies to find the entry:
   1. First tries the direct path
   2. Then tries with 'java.base/' module prefix for core classes
   3. Finally, searches through all entries for ones ending with the relative path"
  (or
    ;; Try direct path
    (.getEntry jar relative-path)
    ;; Try with java.base module prefix for core classes
    (.getEntry jar (str "java.base/" relative-path))
    ;; Try all entries ending with our path (for more complex module layouts)
    (let [entries (enumeration-seq (.entries jar))
          matches (filter #(.endsWith (.getName %) relative-path) entries)]
      (first matches))))

(defn configure-java-parser []
  "Configures the JavaParser library to support the latest Java version (Java 21).

   Parameters:
   None

   Returns:
   Nil

   Side effects:
   Sets the JavaParser configuration to use Java 21 language level, enabling
   parsing of the latest Java syntax features."
  (let [config (.setLanguageLevel (com.github.javaparser.ParserConfiguration.)
                 com.github.javaparser.ParserConfiguration$LanguageLevel/JAVA_21)]
    (StaticJavaParser/setConfiguration config)))

;; Configure the parser once
(configure-java-parser)

(defn find-jdk-source-path []
  "Attempts to find the JDK src.zip file containing Java standard library source code.

   Parameters:
   None

   Returns:
   A File object pointing to the JDK src.zip file if it exists, nil otherwise.

   The function tries several common locations for the src.zip file based on the
   value of java.home system property, handling different JDK distributions and
   operating systems (Linux, Windows, macOS)."
  (let [jdk-home       (System/getProperty "java.home")
        possible-paths [(str jdk-home "/../lib/src.zip")    ; OpenJDK on Linux/Windows
                        (str jdk-home "/lib/src.zip")       ; Some JDK distributions
                        (str jdk-home "/src.zip")           ; Some JDK distributions
                        (str jdk-home "/../src.zip")]       ; macOS layout possibility
        existing-path  (first (filter #(.exists (io/file %)) possible-paths))]
    (when existing-path
      ;; Found JDK sources
      (io/file existing-path))))

(defn declaration ^ClassOrInterfaceDeclaration [^String class-name]
  "Retrieves the class or interface declaration from source code for a given class.

   Parameters:
   - class-name: A string containing the fully-qualified class name

   Returns:
   A ClassOrInterfaceDeclaration object representing the parsed class declaration,
   or nil if the source code couldn't be found or parsed.

   This function handles two different cases:
   1. For JDK classes: Looks for the source in the JDK's src.zip file
   2. For other classes: Looks for the source in Maven source JARs

   In both cases, it opens the JAR file, finds the .java file, parses it, and
   extracts the relevant class declaration."
  (let [java-path (str (class-to-path class-name) ".java")]
    (if (is-jdk-class? class-name)
      ;; For JDK classes, try to find source in the JDK src.zip
      (when-let [src-file (find-jdk-source-path)]
        (try
          (with-open [jar (JarFile. src-file)]
            (when-let [entry (find-entry-in-jar jar java-path)]
              ;; Found JDK source file
              (with-open [^InputStream stream (.getInputStream jar entry)]
                (let [cu                (StaticJavaParser/parse stream)
                      class-name-simple (last (str/split class-name #"\."))]
                  (try
                    (.getFirst (.getLocalDeclarationFromClassname cu class-name-simple))
                    (catch Exception e1
                      (println "Error extracting declaration for" class-name ":" (.getMessage e1))
                      nil))))))
          (catch Exception e
            (println "Error reading JDK source for" class-name ":" (.getMessage e))
            nil)))

      ;; For other classes, use the previous approach with Maven source jars
      (enc/when-let [jar   (some-> (source-jar class-name) (JarFile.))
                     entry (.getEntry jar java-path)]
        (with-open [^InputStream stream (.getInputStream jar entry)]
          (let [cu                (StaticJavaParser/parse stream)
                class-name-simple (last (str/split class-name #"\."))]
            (try
              (.getFirst (.getLocalDeclarationFromClassname cu class-name-simple))
              (catch Exception e
                (println "Error extracting declaration for non-JDK class" class-name ":" (.getMessage e))
                nil))))))))

(defn get-parent-classes [^String class-name]
  "Gets a list of parent class and interface names for a given class.

   Parameters:
   - class-name: A string containing the fully-qualified class name

   Returns:
   A vector of strings containing the fully-qualified names of all direct parent classes
   and interfaces of the given class. The vector excludes java.lang.Object as a parent.
   Returns an empty vector if the class cannot be found or if an error occurs.

   This function uses Java reflection to identify both the superclass and
   implemented interfaces of the specified class."
  (try
    (let [cls          (Class/forName class-name)
          superclass   (.getSuperclass cls)
          interfaces   (seq (.getInterfaces cls))
          parent-names (filterv identity
                         (concat
                           (when (and superclass (not= (.getName superclass) "java.lang.Object"))
                             [(.getName superclass)])
                           (when interfaces
                             (mapv #(.getName %) interfaces))))]
      parent-names)
    (catch Exception e
      (println "Error getting parent classes for" class-name ":" (.getMessage e))
      [])))

;; Forward declaration to avoid circular dependency
(declare extract-class-info)

(defn find-method-docs-in-parent
  "Finds documentation for a specific method in a parent class.

   Parameters:
   - parent-class: A string containing the fully-qualified name of a parent class/interface
   - method-name: A string containing the name of the method to find documentation for

   Returns:
   A map containing the documentation for the method if found, with keys:
   - :description - The main description from the Javadoc
   - :params - A map from parameter names to their descriptions
   - :return - The return value description

   Returns nil if the parent class cannot be found, if it doesn't contain the method,
   or if the method doesn't have any documentation."
  [parent-class method-name]
  (try
    ;; Get parent class info and look for the method
    (let [parent-info (extract-class-info parent-class)]
      (when parent-info
        (let [parent-method (first (filter #(= (:name %) method-name) (:methods parent-info)))]
          (when (and parent-method
                  (or (not-empty (:description parent-method))
                    (not-empty (:params parent-method))
                    (:return parent-method)))
            ;; Found docs in parent class
            (select-keys parent-method [:description :params :return])))))
    (catch Exception e
      (println "Error getting docs from parent" parent-class ":" (.getMessage e))
      nil)))

(defn get-method-docs-from-parents [method-name parent-classes]
  "Searches for method documentation in a list of parent classes/interfaces.

   Parameters:
   - method-name: A string containing the name of the method to find documentation for
   - parent-classes: A vector of strings containing fully-qualified parent class/interface names

   Returns:
   A map containing the first found documentation for the method, with keys:
   - :description - The main description from the Javadoc
   - :params - A map from parameter names to their descriptions
   - :return - The return value description

   Returns nil if no documentation is found in any parent class.

   This function iterates through the parent classes in order, stopping at the first
   one that contains sufficient documentation for the method."
  (try
    (loop [remaining-parents parent-classes
           result nil]
      (if (or result (empty? remaining-parents))
        result
        (let [parent-class (first remaining-parents)
              parent-info (extract-class-info parent-class)
              parent-methods (:methods parent-info)
              matching-method (when parent-methods
                               (first (filter #(= (:name %) method-name) parent-methods)))
              docs (when matching-method
                    (select-keys matching-method [:description :params :return]))]
          (recur (rest remaining-parents)
                (when (and docs
                          (or (not-empty (:description docs))
                              (not-empty (:params docs))
                              (:return docs)))
                  docs)))))
    (catch Exception e
      (println "Error getting method docs from parents:" (.getMessage e))
      nil)))

(defn enrich-method-docs-from-parents
  "Enriches method documentation by looking for missing information in parent classes and interfaces.

   Parameters:
   - class-name: A string containing the fully-qualified class name
   - methods-docs: A vector of maps containing method documentation

   Returns:
   A vector of method documentation maps with missing documentation fields filled in
   from parent classes and interfaces when available.

   This function looks at each method in the provided methods-docs and checks if it has
   complete documentation (description, parameters, and return value). If any information
   is missing, it searches parent classes for the same method to find and incorporate
   the missing documentation.

   The function automatically downloads source JARs for parent classes when needed."
  [^String class-name methods-docs]
  (try
    (let [parent-classes (get-parent-classes class-name)]
      (if (empty? parent-classes)
        methods-docs
        ;; For each method, try to enrich with docs from parent classes
        (mapv
          (fn [method-doc]
            (let [method-name (:name method-doc)
                  has-full-docs (and (not-empty (:description method-doc))
                                    (not-empty (:params method-doc))
                                    (:return method-doc))]
              (if has-full-docs
                ;; Already has complete docs
                method-doc
                ;; Try to find docs in parent classes
                (let [parent-docs (get-method-docs-from-parents method-name parent-classes)]
                  (if parent-docs
                    (do
                      ;; Found docs in parent classes
                      (merge method-doc parent-docs))
                    method-doc)))))
          methods-docs)))
    (catch Exception e
      (println "Error in enrich-method-docs-from-parents:" (.getMessage e))
      methods-docs)))

(declare extract-class-info-with-parents)

(defn extract-class-info
  "Extracts basic information about a Java class from its source code.

   Parameters:
   - class-name: A string containing the fully-qualified class name

   Returns:
   A map containing the following keys:
   - :class-name - The fully-qualified class name
   - :docstring - The class-level Javadoc description, or nil if none exists
   - :methods - A vector of maps containing documentation for each public method

   Returns nil if the class declaration cannot be found or parsed.

   This function extracts only the information available directly in the class's
   source code, without enriching method documentation from parent classes."
  [^String class-name]
  (let [clazz (declaration class-name)]
    (if clazz
      (let [^Javadoc javadoc (oo (.getJavadoc clazz) nil)
            doc              (if javadoc
                               (format-description (.getDescription javadoc))
                               nil)
            methods          (filterv
                               (fn [^MethodDeclaration m] (.isPublic m))
                               (.getMethods clazz))
            method-docs      (mapv extract-method-docs methods)]
        {:class-name class-name
         :docstring  doc
         :methods    method-docs})
      nil)))

(defn extract-class-info-with-parents
  "Extracts comprehensive information about a Java class, including documentation from parent classes.

   Parameters:
   - class-name: A string containing the fully-qualified class name

   Returns:
   A map containing the following keys:
   - :class-name - The fully-qualified class name
   - :docstring - The class-level Javadoc description, or nil if none exists
   - :methods - A vector of maps containing enriched documentation for each public method

   Returns nil if the class declaration cannot be found or parsed.

   This function first extracts the basic class information and then enriches any
   methods with incomplete documentation by searching for that documentation in
   parent classes and interfaces. This provides more complete documentation even
   when a class doesn't fully document inherited methods."
  [^String class-name]
  (let [basic-info (extract-class-info class-name)]
    (if basic-info
      (let [parent-classes (get-parent-classes class-name)]
        (if (empty? parent-classes)
          basic-info
          (try
            ;; Update each method that needs documentation
            (update basic-info :methods
              (fn [methods]
                (mapv
                  (fn [method]
                    (let [method-name (:name method)
                          missing-docs (or (empty? (:description method))
                                          (empty? (:params method))
                                          (nil? (:return method)))]
                      (if missing-docs
                        (if-let [parent-docs (get-method-docs-from-parents method-name parent-classes)]
                          (do
                            ;; Found docs in parent class
                            (merge method parent-docs))
                          method)
                        method)))
                  methods)))
            (catch Exception e
              (println "Error enriching methods from parents:" (.getMessage e))
              basic-info))))
      nil)))

(defn describe-class
  "Gets comprehensive information about a Java class, method, or parameter.

   This function has three arities:

   Arity 1: (describe-class class-name)
   Parameters:
   - class-name: A string containing the fully-qualified name of the class

   Returns:
   A map containing detailed information about the class, including its docstring
   and documentation for all public methods, enriched with documentation from
   parent classes where available.

   Arity 2: (describe-class class-name method-name)
   Parameters:
   - class-name: A string containing the fully-qualified name of the class
   - method-name: A string containing the name of a method to find

   Returns:
   A string containing the source code of all methods with the given name in the class,
   including all overloaded versions, separated by newlines.

   Arity 3: (describe-class class-name method-name param-name)
   Parameters:
   - class-name: A string containing the fully-qualified name of the class
   - method-name: A string containing the name of a method
   - param-name: A string containing the name of a parameter in the method

   Returns:
   A string containing the Javadoc description of the specified parameter, or nil
   if the parameter or its documentation doesn't exist.

   Note: Use find-classes-by-simple-name to find fully-qualified class names when you
   only know the simple name of a class."
  ([^String class-name]
   (extract-class-info-with-parents class-name))
  ([^String class-name ^String methodName]
   (find-method-source (declaration class-name) methodName))
  ([^String class-name ^String methodName ^String paramName]
   (let [{:keys [methods]} (describe-class class-name)]
     (some-> (filterv #(and (contains? (:params %) paramName) (= methodName (:name %))) methods)
       (first)
       (get-in [:params paramName])))))

(let [cached-result (volatile! {})]
  (defn find-classes-by-simple-name
    "Finds all fully-qualified class names on the classpath matching a simple class name.

     Parameters:
     - simple-name: A string containing the simple (unqualified) class name to search for

     Returns:
     A vector of strings containing all fully-qualified class names on the current
     classpath that have the given simple name, or nil if none are found.

     The function caches results for better performance in subsequent calls.
     Inner classes are skipped and not included in the results.

     This function is useful when you know a class's simple name (e.g., 'ArrayList')
     but need to find its fully-qualified name (e.g., 'java.util.ArrayList')."
    [simple-name]
    (locking cached-result
      (if (seq @cached-result)
        (get @cached-result simple-name)
        (let [scan-result (-> (ClassGraph.)
                            (.enableClassInfo)
                            (.enableSystemJarsAndModules)
                            (.scan))
              all-classes (.getAllClasses scan-result)
              nm->nms     (reduce
                            (fn [acc ^ClassInfo c]
                              (if (.isInnerClass c)
                                acc
                                (update acc (.getSimpleName c) (fnil conj []) (.getName c))))
                            {}
                            all-classes)]
          (vreset! cached-result nm->nms)
          (nm->nms simple-name))))))

(defn wildcard-to-regex
  "Converts a wildcard pattern (e.g., 'get*', '*Name*') to a regex pattern."
  [pattern]
  (if pattern
    (-> pattern
        (str/replace #"\*" ".*")
        (str/replace #"\?" ".")
        (as-> p (str "^" p "$")))
    nil))

(defn filter-methods
  "Filters method information based on a pattern.

   Parameters:
   - methods: A sequence of method info maps
   - pattern: A wildcard pattern to match method names (can contain * and ?)

   Returns:
   A filtered sequence of method info maps whose names match the pattern."
  [methods pattern]
  (if pattern
    (let [regex-pattern (re-pattern (wildcard-to-regex pattern))]
      (filter #(re-matches regex-pattern (:name %)) methods))
    methods))

(defn methods-of
  "Gets a formatted string listing public method declarations for a class.

   Parameters:
   - class-name: A string containing the fully-qualified class name
   - pattern: (Optional) A wildcard pattern to filter method names (e.g., 'get*', '*Name*')

   Returns:
   A string containing the declarations of matching public methods in the class,
   with each method declaration on a separate line. Returns an empty string
   if the class cannot be found or has no matching methods.

   This function provides a convenient way to see available methods on a class
   at once, which is useful for API exploration and discovery."
  ([^String class-name]
   (methods-of class-name nil))
  ([^String class-name pattern]
   (let [class-info (describe-class class-name)]
     (if class-info
       (let [methods (:methods class-info)
             filtered-methods (filter-methods methods pattern)]
         (->> filtered-methods
              (map :declaration)
              (str/join "\n")))
       ""))))

;; Strip javadoc comments function for javasrc
(defn strip-javadoc-comments [source]
  "Removes Javadoc comments from Java source code.

   Parameters:
   - source: A string containing Java source code

   Returns:
   A string with all Javadoc comments (starting with /** and ending with */) removed,
   while preserving regular comments and code structure."
  (if source
    (str/replace source #"/\*\*[\s\S]*?\*/" "")
    ""))
