# Clojure REPL Commands For Java Info

This is a small library with REPL tools that are intended to be used by nREPL-based MCP tools and AI. The idea is that when you are working with Java libraries in Clojure this will enable you to start an nREPL, and have an nREPL MCP be able to ask for javadoc or even the Java source code of classes and methods. The AI can then automatically build a much better (and compact) context window for very specific Java things.

The basic way this works is as follows:

The built-in classes have local source code in a `src.zip`. All of the other code on the classpath typically comes from a Maven-style repo, and those files are stored in your `~/.m2` folder.

The Java ecosystem has a standard naming for source JARs, which you can download from Maven.

So, we use the classpath to discover things (e.g. Java reflection and classpath scanning), and then this code will auto-download (and cache) the source jar files for anything on your classpath.

It then uses some GitHub libraries to parse the source code to locally extract and format the source code or docstrings from the source code!

So, now instead of having to point your AI at huge HTML javadoc pages online, it can use the nREPL to get targeted info straight from the literal source code of those libraries.

Of course, the Clojure REPL already has this support for clojure code, so this library just balances it out.

See https://clojuredocs.org/clojure.repl/source.

## General Usage

1. Install an AI tool that supports MCP use, like Roo Code or Claude Code
2. Add an nREPL MCP server
3. Run an nREPL

The `finding-java-doc-and-source.md` is a document you can point your AI at to tell it how to use this library, and it is readable by you as well, of course.

You'll also want to give your AI a document to tell it how to find your nREPL port.

## Status

Just starting to play with it. Contributions welcome. Tested against Java 21. Other versions may have parsing issues.

## License

Copyright 2025, Fulcrologic, LLC.

MIT License.
