#!/bin/bash

# This script is to run the krome code with the CSE network, using the 'CSE_run_krome.f' script.


cp initial_abs.f krome/build
cp Makefile_abs krome/build/Makefile

# Make executable
cd krome/build
make gfortran
./initial_abs "../../data/20211015_gridC_Mdot1e-6_v17-5_T_eps_model_2022-12-24h17-06-51/abs/abs_1.txt"

cd ../..
# Copy the CSE_run_krome.f script to the krome build directory
cp CSE_run_krome.f krome/build
cp Makefile krome/build

# # Make executable
cd krome/build
make gfortran

# # # Move the executable 
# cd ../../
# cp krome/build/run_CSE_krome .