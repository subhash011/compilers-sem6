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