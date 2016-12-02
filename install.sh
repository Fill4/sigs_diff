#!/bin/bash

cd lib/
make
cd ../src
make
mv sigs_diff ../sigs_diff
cd ../
