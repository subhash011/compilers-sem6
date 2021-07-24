# Compilers Lab - IIT Palakkad (6th semester)

**Author**  - Subhash S (111801042)

This repository is a minimal implementation following the book `Modern compiler implementation in ML` by Andrew W. Appel.  
For specifications of the language, visit the [website](https://www.lrde.epita.fr/~tiger/tiger.html).  
This implementation does not support nested functions and object oriented programming techniques.

### Compiler status

1. The compiler can produce the intermediate representation of the tiger language. 
2. The program can also produce the pretty printed souce code of the tiger language similar to the `indent` program linux.
3. Pretty printing can be done with or without syntax highlighting.

### Error messages

The error messages are very generic and accurate message are displayed only for syntax errors. Other errors such as invalid types throw a generalised exception.

See [INSTRUCTIONS.md](INSTRUCTIONS.md) to use the tiger compiler.
