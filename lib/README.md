fortran_LIB
===========

### Library of Fortran 95 functions and subroutines for many applications

* test_libs - 
This is where the libraries are tested. Compile with
`make test`
and then use
`./test module_name`
to test each module.


To use this library in your program use

-lmodules -L/usr/lib -I/home/joao/Programs/fortran/lib -L/home/joao/Programs/fortran/lib
