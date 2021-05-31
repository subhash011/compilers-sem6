- This lab has some changes to the file structure and the binary produced.

## Binary file


- The final output is in the root directory of the project.
```
1. Use './tc --help' for help on how to run the binary.
2. Using './tc' without any flags defaults to printing the IR.
3. Using './tc' without a file name takes the input from stdin.
```

## Makefile

The targets 'test' and 'tests' work differently as follows:
1. test: This target pretty prints the tests/custom.tig file along with syntax highlighting.
2. tests: This target does a two way check. It first converts the source code to AST, then it converts the AST to pretty printed source code and it converts the resulting source code to AST again (this helps in veryfying that the AST -> source and source -> AST conversions are valid.) This target outputs the resulting source code (without syntax highlighting) to the tig_test.out file which also contains the errors if any.
