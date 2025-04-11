# OCaml Prolog Interpreter

This project is a simplified interpreter for the Prolog language, written in OCaml. Given a program and a query, the program uses OCaml effects to pause and return a solution, and resume the computation on user input. 

Supports
* Rules, facts, symbols
* Lazy evaluation
* Basic arithmetic operations
* Lists
* Cut operator (`!`)

## Usage

```
dune exec ./frontend.exe [filename.pl]
```