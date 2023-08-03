# Little Ada Compiler using OCamllex and OCamlyacc

This project focuses on constructing a compiler for the Little Ada programming language using OCaml, specifically with the tools OCamllex and OCamlyacc. Little Ada is a scaled-down version of the Ada programming language, predominantly utilized in systems where high levels of safety and reliability are required.

## Overview

The Little Ada Compiler is designed to translate source code written in Little Ada into executable code. This process involves various stages, including lexical analysis, syntax analysis and semantic analysis, each powered by OCaml, OCamllex, and OCamlyacc.

## Features

* **Lexical Analysis (with OCamllex):** This is the initial stage of the compilation process. The source code is scanned and transformed into a stream of tokens, with each token representing a logically cohesive sequence of characters (identifiers, keywords, operators, etc.). In this project, OCamllex is used to automatically generate the lexer from a set of regular expressions with associated semantic actions.

* **Syntax Analysis (with OCamlyacc):** The compiler, using the syntax rules of the Little Ada language, checks the stream of tokens for grammatical accuracy. OCamlyacc, a parser generator, is used in this stage to generate the parser code from a context-free grammar of Little Ada language.

* **Semantic Analysis:** This phase involves examining whether the expressions and statements in the source code make sense within the language's context.

## Prerequisites

To use the Little Ada Compiler, you will need a functional installation of OCaml, which is a robust functional programming language suited to high-level and succinct programming. Furthermore, familiarity with OCamllex and OCamlyacc will be beneficial.

## Usage

### Compile
    make test_parser

### Execute
    ./test_parser

### Script bash
    run_all_file.sh <path>
        where path is the path to the folder containing the OK and KO folders


## Disclaimer

This project is intended as an educational resource to comprehend the process of compilation. It is not designed to replace comprehensive Ada compilers employed in production environments.
Please resort to established Ada compilers for any serious or production-grade Ada development. Little Ada and this compiler offer only a subset of the functionality of the full Ada language and its compilers.

## Note
    The project compiles and has been tested on two different versions of ocaml.
    We also encountered problems with invisible characters on the test files supplied, which we understand to be a problem between linux and windows.
