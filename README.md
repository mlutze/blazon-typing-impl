# Blazon Typing

This project contains an SBT project implementing a Blazon Typechecker.

## Running

The project can be compiled and run with the following command:

```sh
sbt run
```

This launches a REPL where you can submit blazons to be typechecked.

```
> Gules a rock or.
OK
> Gules a rock sable.
               ^^^^^
Type error: rule of tincture violation
```
