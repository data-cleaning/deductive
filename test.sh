#!/bin/bash

R -d "valgrind --tool=memcheck --leak-check=full" -e "devtools::test('pkg')"
#R  -e "devtools::test('pkg')"

