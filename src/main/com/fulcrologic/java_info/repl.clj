(ns com.fulcrologic.java-info.repl
  "Utilities for extracting Java API information for display at the REPL.

   This namespace provides functions to retrieve javadoc documentation and
   source code for Java classes and methods, making it easier to work with
   Java libraries from Clojure."
  (:require [clojure.string :as str]
            [com.fulcrologic.java-info.java-info-impl :as impl]))

(defn- clean-markup
  "Cleans up JavaDoc markup for more readable output."
  [text]
  (when text
    (-> text
      (str/replace #"\{@code ([^}]+)\}" "$1")
      (str/replace #"\{@link ([^}]+)\}" "$1"))))

(defn- truncate-if-needed
  "Truncates text to the specified length if it exceeds that length."
  [text max-length]
  (if (and text (> (count text) max-length))
    (str (subs text 0 (- max-length 3)) "...")
    text))

(def ^:private max-class-doc-length 1000)
(def ^:private max-method-doc-length 600)
(def ^:private max-param-doc-length 200)
(def ^:private max-class-src-length 2000)
(def ^:private max-method-src-length 1000)

(defn javadoc
  "Get Javadoc documentation for a Java class or method.

   When called with just a class name, returns the class-level documentation.
   When called with a class name and method name, returns documentation for all
   overloaded versions of that method.

   Parameters:
   - class-name: A string containing the fully-qualified class name
   - method-name: (Optional) A string containing the method name to get docs for
   - options: (Optional) A map containing configuration options:
     - :max-class-length - Maximum length for class documentation (default: 1000)
     - :max-method-length - Maximum length for method documentation (default: 600)
     - :max-param-length - Maximum length for parameter documentation (default: 200)

   Returns:
   For a class: A string containing the class documentation (truncated if too long)
   For a method: A string with all overloaded method signatures and their
                 respective documentation, clearly separated and truncated if needed"
  ([class-name]
   (javadoc class-name nil {}))
  ([class-name method-name]
   (javadoc class-name method-name {}))
  ([class-name method-name options]
   (let [max-class-len  (or (:max-class-length options) max-class-doc-length)
         max-method-len (or (:max-method-length options) max-method-doc-length)
         max-param-len  (or (:max-param-length options) max-param-doc-length)]
     (if method-name
       ;; Method documentation
       (let [class-info (impl/describe-class class-name)
             methods    (when class-info
                          (filterv #(= (:name %) method-name) (:methods class-info)))]
         (if (seq methods)
           (->> methods
             (map (fn [method]
                    (let [declaration (clean-markup (:declaration method))
                          description (truncate-if-needed
                                        (clean-markup (:description method))
                                        max-method-len)
                          params      (when (seq (:params method))
                                        (str/join "\n"
                                          (map (fn [[param-name param-desc]]
                                                 (str param-name ": "
                                                   (truncate-if-needed
                                                     (clean-markup param-desc)
                                                     max-param-len)))
                                            (:params method))))
                          return-desc (truncate-if-needed
                                        (clean-markup (:return method))
                                        max-param-len)]
                      (str declaration "\n"
                        (when description
                          (str description "\n"))
                        (when params
                          (str params "\n"))
                        (when return-desc
                          (str "Returns: " return-desc))))))
             (str/join "\n\n"))
           (str "No methods named '" method-name "' found in class " class-name)))
       ;; Class documentation
       (let [class-info (impl/describe-class class-name)]
         (if-let [doc (:docstring class-info)]
           (truncate-if-needed (clean-markup doc) max-class-len)
           (str "No documentation found for class " class-name)))))))

(defn javasrc
  "Get the source code for a Java class or method.

   When called with just a class name, returns the complete class source.
   When called with a class name and method name, returns the source for all
   overloaded versions of that method.

   Parameters:
   - class-name: A string containing the fully-qualified class name
   - method-name: (Optional) A string containing the method name to get source for
   - options: (Optional) A map containing configuration options:
     - :max-class-length - Maximum length for class source (default: 2000)
     - :max-method-length - Maximum length for method source (default: 1000)

   Returns:
   For a class: A string containing the complete class source with javadoc comments removed
                (truncated if too long)
   For a method: A string with all overloaded method implementations with javadoc comments removed
                 (truncated if too long)"
  ([class-name]
   (javasrc class-name nil {}))
  ([class-name method-name]
   (javasrc class-name method-name {}))
  ([class-name method-name options]
   (let [max-class-len  (or (:max-class-length options) max-class-src-length)
         max-method-len (or (:max-method-length options) max-method-src-length)]
     (if method-name
       ;; Method source
       (let [method-source (impl/describe-class class-name method-name)]
         (if method-source
           (-> method-source
             impl/strip-javadoc-comments
             (str/replace #"\\n" "\n")
             (truncate-if-needed max-method-len))
           (str "No method named '" method-name "' found in class " class-name)))
       ;; Class source
       (let [class-decl (impl/declaration class-name)]
         (if class-decl
           (-> (str class-decl)
             impl/strip-javadoc-comments
             (str/replace #"\\n" "\n")
             (truncate-if-needed max-class-len))
           (str "Source not found for class " class-name)))))))

(defn find-classes
  "Find all fully-qualified class names on the classpath matching a simple class name.

   Parameters:
   - simple-name: A string containing the simple (unqualified) class name to search for

   Returns:
   A vector of strings containing all fully-qualified class names on the current
   classpath that have the given simple name, or nil if none are found.

   This function is useful when you know a class's simple name (e.g., 'ArrayList')
   but need to find its fully-qualified name (e.g., 'java.util.ArrayList').

   Example:
   ```clojure
   (find-classes \"File\") ; => [\"java.io.File\" ...]
   ```"
  [simple-name]
  (impl/find-classes-by-simple-name simple-name))

(defn methods-of
  "List all public method signatures available on a Java class.

   Parameters:
   - class-name: A string containing the fully-qualified class name
   - options: (Optional) A map containing configuration options:
     - :pattern - A wildcard pattern to filter method names (e.g., \"get*\", \"*Name*\")
     - :max-length - Maximum length for the returned string (default: 3000)

   Returns:
   A string containing the declarations of all public methods in the class,
   with each method declaration on a separate line. If a pattern is provided in
   the options map, only methods whose names match the pattern will be included.

   Examples:
   ```clojure
   ;; List all methods
   (methods \"java.util.ArrayList\")

   ;; List only methods matching a pattern
   (methods \"java.util.ArrayList\" {:pattern \"add*\"})

   ;; List methods with custom length limit
   (methods \"java.util.ArrayList\" {:max-length 1000})
   ```"
  ([class-name]
   (methods-of class-name {}))
  ([class-name options]
   (let [max-length (or (:max-length options) 3000)
         pattern    (:pattern options)
         method-str (impl/methods-of class-name pattern)]
     (if (empty? method-str)
       (if pattern
         (str "No methods matching pattern '" pattern "' found in class " class-name)
         (str "No methods found in class " class-name))
       (truncate-if-needed method-str max-length)))))
