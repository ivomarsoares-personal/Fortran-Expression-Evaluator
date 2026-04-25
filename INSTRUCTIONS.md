# To Run this Interpreter

## On Linux (Ubuntu)

1. Install the GNU Fortran compiler: `sudo apt-get install gfortran`
1. Compile the source code for the interpreter and the test program: `gfortran -ffree-line-length-none -o test interpreter.f90 test.f90`
1. Run the test program: `./test`
1. To compile and run the unit tests: 
    ```bash
    gfortran -ffree-line-length-none -o unit_tests interpreter.f90 unit_tests.f90 
    ./unit_tests
    ```

## On MacOS

1. Install the gcc compiler with HomeBrew (which includes gfortran): `brew install gcc`
1. Compile the source code for the interpreter and the test program: `gfortran -ffree-line-length-none -o test interpreter.f90 test.f90`
1. Run the test program: `./test`
1. To compile and run the unit tests: 
    ```bash
    gfortran -ffree-line-length-none -o unit_tests interpreter.f90 unit_tests.f90 
    ./unit_tests
    ```