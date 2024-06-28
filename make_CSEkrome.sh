#!/bin/bash

# This script is to run the krome code with the CSE network, using the 'CSE_run_krome.f' script.

# Copy the CSE_run_krome.f script to the krome build directory
cp -r CSE_run_krome.f krome/build
cp -r Makefile krome/build

# Make executable
cd krome/build
make gfortran

# # Move the executable 
cd ../../
cp krome/build/run_CSE_krome .