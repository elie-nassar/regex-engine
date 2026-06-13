# Regex Engine

A regular expression engine implementation in OCaml that converts regex patterns into finite automata for pattern matching.

## Building

```bash
dune build
```

## Running

```bash
dune exec regex_engine
```

## Project structure

```bash
.
├── bin
│   ├── dune
│   └── main.ml
├── dune-project
├── lib
│   ├── ast.ml
│   ├── dfa.ml
│   ├── dune
│   └── nfa.ml
├── Readme.md
└── regex_engine.opam
```