#!/bin/bash

# This script is used to build the krome code compatible with the UMIST rate16 chemical network.

# Copy the chemical network to the correct krome directory
cp -r react_umist_rate16 krome/networks

# Load the krome module
cd krome
python krome -n networks/react_umist_rate16.dat -noSinkCheck -noRecCheck -iRHS 