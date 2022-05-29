# hlisp

A lisp interpreter, written in Haskell

This project uses cabal. To run, just cd into this directory and run `cabal run`.

Currently it provides a repl that allows the user to evaluate a small subet of lisp-style expressions.

TO DO:
- [ ] Parse operators (+, -, etc)
- [ ] Allow users to define lisp functions
- [ ] Parse whole files instead of just using a repl
- [ ] Create a [Language Server](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide) implementation for the language
- [ ] Implement some basic library functions according to [r7rs](https://small.r7rs.org/) or some other minimalist lisp standard
