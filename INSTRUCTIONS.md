# This file contains instructions for each week's assignment.

## Week 4 - Parser for tiger language.

- All the source files are under the tiger/ directory
- If you face any errors, please make sure the user owns the directory and the tig_test.sh has executable permissions (although the Makefile takes care of this, there might be errors if the project is not in a user owned location.)

`
Useful Make options
`

```
1. all: Brings the parser and lexer together and writes the output of the resulting binary to tiger/tiger.

2. test: This target runs the custom.tig file in the tests/ directory and prints the output to stdout.

3. tests: This target runs all the test files in the tests/ directory except the custom.tig and writes the output ast to tig_test.out with each ast below its corresponding file name.

4. run: This target can be used to pass input to tiger from the terminal.

5. clean: Cleans all the generated files.
```

## Week 5 - Pretty Printer

- This lab has some changes to the file structure and the binary produced.

`Binary file`


- The final output is in the root directory of the project.
```
1. Use './tc -h' for help on how to run the binary.
2. To print the AST use './tc -a filename'
3. To pretty print the source code use './tc -p filename'
4. To pretty print the source code along with syntax highlighting, use './tc -pc filename'
```

`Makefile`

The targets 'test' and 'tests' work differently as follows:
1. test: This target pretty prints the tests/custom.tig file along with syntax highlighting.
2. tests: This target does a two way check. It first converts the source code to AST, then it converts the AST to pretty printed source code and it converts the resulting source code to AST again (this helps in veryfying that the AST -> source and source -> AST conversions are valid.) This target outputs the resulting source code (without syntax highlighting) to the tig_test.out file which also contains the errors if any.
