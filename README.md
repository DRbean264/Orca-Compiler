# Orca Compiler

This is a Tiger compiler written in SML/NJ for ECE 553 Spring 2023.

## Getting started

```
sml sources.cm
```

At the sml prompt, you can play with the compiler by calling (support till instruction selection)

```
Main.compile "testcases/sample-tiger-programs/xxx.tig"
```

### Extra Note
In funcion "procEntryExit2", I replace the space with 2 newline characters so that I can clearly see the boundary between different functions.

## Running Tests

```
sml sources.cm test_lexer.sml
```

## Authors and acknowledgment
This is a group project by Gan Shun Lim (gl161), Jeremy Ding (yd160), and Qiheng
Gao (qg45)

