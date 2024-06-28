#!/bin/bash
echo "---------------------------------------------"

# This script is used to build the krome code compatible with the UMIST rate16 chemical network.

# Copy the UMIST rate16 network to the krome tools directory
cp -r choose_umist.py krome/tools/
cp -r umist_rate16.rates krome/tools/

# Navigate to correct krome directory
cd krome/tools

# Use the krome tool to convert the UMIST rate16 network to the correct format, with correct network
python choose_umist.py -n $1
python umist2krome.py
cp -r network_umist.dat ../networks/

echo "UMIST rate16 network has been converted to the correct format."
echo "KROME will now be built with the UMIST rate16 network."
echo "---------------------------------------------"
echo ""

# Load the krome module
cd ../
python krome -n networks/network_umist.dat -noSinkCheck -noRecCheck -iRHS 