# glamc

Welcome to the GlamC github bois
Yeet


Makefile usage:

You can build the entire glamc compiler by running the command `make`.


You can also build specific components of the compiler (i.e scanner, parser) by running `make <name>`. For example, you can run `make scanner` to build the scanner.


To build custom files, you can run `make custom FILENAME=<filename>`. For example, if you have a file `test.ml` that tests the scanner or parser, you can run `make custom FILENAME=test` in order to successfully create the executable.


To clean the directory of unnecessary files, run `make clean`.
