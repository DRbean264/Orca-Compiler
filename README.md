# Orca Compiler

This is a Tiger Compiler written in SML/NJ for ECE 553 Spring 2023.

## Getting started

```
sml sources.cm
```

At the sml prompt, you can play with the compiler by calling

```
Main.compile "testcases/sample-tiger-programs/[filename].tig"
```

This will generate a single assembly file which contains everything required for it to be run in SPIM simulator.
The path of the generated assembly file is "testcases/sample-tiger-programs/[filename].tig.s"

Load the assembly file in SPIM, and you can run it!

## Running Tests

```
sml sources.cm test_lexer.sml
```

## Authors and acknowledgment
This is a group project by Gan Shun Lim (gl161), Jeremy Ding (yd160), and Qiheng
Gao (qg45)

