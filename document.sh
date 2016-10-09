#!/bin/bash

cp ./build/DESCRIPTION ./pkg
R -e "devtools::document('pkg')"
R CMD Rd2pdf --force --no-preview -o manual.pdf ./pkg


