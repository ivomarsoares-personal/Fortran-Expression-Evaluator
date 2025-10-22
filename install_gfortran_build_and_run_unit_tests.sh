#!/bin/bash
sudo apt-get install gfortran
gfortran -ffree-line-length-none -o unit_tests interpreter.f90 unit_tests.f90
./unit_tests
