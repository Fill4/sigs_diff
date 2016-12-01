#!/bin/bash

cd lib/
make
cd ../src
make
mv sigs-freq ../sigs-freq
cd ../