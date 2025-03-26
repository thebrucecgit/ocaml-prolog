# OCaml Prolog Interpreter

This project is a simplified interpreter for the Prolog language, written in OCaml. Given a program and a query, the program effectively returns a lazy list of possible solutions for the given query. 

Supports
* Rules, facts, symbols
* Lazy evaluation
* Basic arithmetic operations
* Lists

## Usage

```
dune exec ./frontend.exe [filename.pl]
```