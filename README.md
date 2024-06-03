# Safe pattern generation for multi-stage programming

## Introduction

This project is a library, built on top of MetaOCaml, that implements the type-safe generation of arbitrary pattern-matching expressions. The dissertation abstract of this work is provided below, as an introduction:

> Multi-stage programming is a metaprogramming paradigm, allowing for the generation of programs guaranteed to be well-typed and well-scoped. This generation may be used to produce optimised versions of algorithms, such as performing unrolling of a list-mapping function. This function, like many in functional programming languages, makes use of pattern-matching expressions, however, current multi-stage programming languages do not support the type-safe generation of arbitrary pattern-matching expressions.
>
> This work introduces a domain-specific language (dsl) for the compositional description and generation of pattern-matching expressions, an associated type system to restrict generation to valid, type-safe code, and combinators for the construction of statically unknown patterns using this dsl. This work is implemented as a library built on top of the MetaOCaml extension of OCaml.
>
> The pattern system presented provides mechanisms to generate a broad class of pattern-matching expressions, is extensible to support many other pattern types, and provides a simple, ergonomic interface for use, with concise, informative error messages. The approach taken is also sufficiently general to be applicable to alternative multi-stage programming systems.

## File Structure

The file structure of this repository is detailed below, with insignificant files omitted.

```bash
.
├── README.md                           # This README file
├── haskell_patterns                
│   ├── rhiger_full.hs                  # Haskell implementation of Rhiger's pattern
│   ├── rhiger_simple.hs                # A simpler version of Rhiger's patterns
│   └── untyped_sum.hs                  # Untyped Template Haskell generator for sum_n
├── pattern_generation                  # The main pattern generation work
│   ├── bench                
│   │   └── unrolling.ml                # Benchmarks for generated unrolled maps (See diagram in paper)
│   ├── bin                             
│   │   └── main.ml                     # Main sandpit for generator use
│   ├── lib
│   │   ├── generators             
│   │   │   ├── fcp_generators.ml       # Generators using the pattern DSL introduced in this work
│   │   │   ├── generators.ml           # Generators using MetaOCaml's current pattern generation
│   │   │   └── report_examples.ml      # Code snippets featured in the dissertation report
│   │   ├── implementations
│   │   │   ├── hand_unrolled.ml        # Hand-unrolled map functions for powers of two
│   │   │   ├── standard.ml             # Standard map function implementations
│   │   │   ├── unrolled_ds.ml          # Map functions operating on unrolled data structures
│   │   │   └── unrolled_list.ml        # Map functions with unrolled recursion
│   │   └── utilities   
│   │       ├── fun_rebind              # A now unused, unsafe library, once used for function application
│   │       ├── pat_playground          # A playground with usage examples of the pattern DSL
│   │       └── patterns
│   │           ├── common.ml           # Shared definitions between unsafe and safe modules
│   │           ├── pat.ml              # Pattern DSL and code generation combinator implementation
│   │           └── unsafe.ml           # Unsafe utility functions for convenient prototyping
│   └── test
│       └── test_pattern_generation.ml  # Pattern generation tests
├── playground                          # Testing ground for code not written by @ethanrange
└── power_example
    └── power.ml                        # Various forms of power function specialisation, from dissertation background
```

## Usage

For OCaml, BER MetaOCaml N114 is supported:

```bash
❯ ocaml --version
The OCaml toplevel, version 4.14.1
❯ opam switch    
#  switch      compiler                   description
→  4.14.1+BER  ocaml-variants.4.14.1+BER  4.14.1+BER
   default     ocaml.5.1.0                default
```

For Haskell, GHC 9.2.8 is supported:

```bash
❯ ghc --version                            
The Glorious Glasgow Haskell Compilation System, version 9.2.8
```

### Pattern generation library

Navigate to the `pattern_generation` directory. Then:

- To run `nmap`, `sum` or `tail` generation, uncomment the desired function in `bin/main.ml` and run:

```bash
dune exec ./bin/main.exe
```

- To benchmark the list unrollings, run:

```bash
dune build @unrolling
```

- To run the examples from the dissertation, run:

```bash
dune exec ./lib/generators/report_examples.exe
```

- To run the pattern DSL examples, run:

```bash
dune exec ./lib/utilities/pat_playground/pat_playground.exe
```


### Haskell Patterns

Navigate to the `haskell_patterns` directory. Then:

- To use the full Rhiger patterns:

```bash
ghci rhiger_full.hs
```
- To use the simple Rhiger patterns:

```bash
ghci rhiger_simple.hs
```

- To use the untyped `sum_n` generator:

```bash
runghc untyped_sum.hs
```

### Power specialisation example

Navigate to the `power_example` directory. Then run:

```bash
dune exec ./power.exe
```
