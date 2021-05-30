# Changelog

## Git commit hashes
```
Lab 6:  e847f8edfd3b018038c1bae3aeed167e2bdbcbd2  
        Date: 21-05-2021  
        Command:   
            1. Use `make` to generate the binary `tc`  
            2. `./tc --can file` or `make && ./tc --can file` - To get canonized IR  
            3. `./tc --ir file`  or `make && ./tc --ir file` - To get the IR (for lab5)
```
```
Lab 5:  5b3c2fd0b76ae55b2baaacf0bac56fa103d14425  
        Date: 28-04-2021  
        Command:  
            1. Use `make` to generate the binary `tc`  
            2. `./tc --help` - Help regarding command line args.  
            3. Since this lab could not be tested without lab6 code, there is no flag to print IR.
            4. But the canon.sml was up to date during this commit and only some optimizations were
               added to the traceSchedule after this commit.
```
```
Lab 4:  db9ec061dff475798f312874df9d4c3c08612519
        Date: 30-03-2021
```
```
Lab 3:  ff7e944fdae54ff0c9d32877d207f701544cd458
        Date: 22-03-2021
```

### 21-05-2021
- Used frames to implement functions, now supporting recursion
- Fixed errors in converting conditional expressions.
- Exception handling and removing unwanted EXP's from trace.

### 16-05-2021
- Complete translation from tiger to tree
- Added printtree.sml to print treees
- Command: 
    1. make && ./tc --can file - To print the canonized version of the tree.
    2. make && ./tc --ir file - To print the intermediate representation.
### 29-04-2021
- Added translate.sml
- Completed adding structures are conversion functions for translate.sml

### 28-04-2021
- Added structures for Tree IR.
- Added algorithm for canonisation.
- Reorganised file structure, moved symbol.sml and errormsg.sml to util/
- Add cm files for testing with the smlnj interpreter.

### 30-03-2021
- Pretty printer added.
- File structure changes, moved tiger/tiger.sml to tc.sml
- Command line flags introduced to choose the output of the compiler.
- Changes made to Makefile to test validity of converstion from source -> AST and AST to source.
- Removed the tig_test.sh script, instead moved to Makefile which also gives immediated results of tests.

### 16-03-2021
- Completed parser for tiger language, all source files reside in the "tiger/" directory
- added printast.sml to print the abstract syntax tree obtained from the parser.
- tiger.lex contains the lexer.
- tiger.grm contains the grammar.
- symbol.sml is created to store all the variables that will be encountered during compilation.
- tiger.mlb puts together are the files for compiling with mlton.
- tests/ directory contains all the testcases from the implementation of the appel book.
- removed src directory created in the first lab since it was causing confusions.

### 08-03-2021
- Added ASTs of source language Tiger and target language MIPS (SPIM).
- target/mips.sml contains the ast of mips.
- tiger/ast.sml contains the ast of tiger along with an example to derive the AST for a section of code in tiger language.

### 26-02-2021
- Added Division operator to the reverse polish compiler
- In this revision, the division has been given the highest preference compared to other operators.
- ast.sml has been modified to make Div as a BinOp
- DIV added to terminals, operator associativity and the rule EXP
- expr.lex has been modified to define what token represents DIV ("/")
- rp.lex modified to execute the division operation on seeing the operator.

### 01-03-2021
- Added parenthesis to evaluate expression
- expr.lex has been modified to define what tokens represent left (LPAREN) and right (RPAREN) parenthesis
- LPAREN and RPAREN added to the list of terminals in expr.grm
- New expression case with parenthesis added to the rule EXP
- modified test.expr to test new additions to the compiler.

### 19-02-2021
- Added Makefile, .gitignore, src folder and a basic Hello World example.
