#!/bin/bash
echo "---------------------------------------------"

arg=$1

# This script is used to build the krome code compatible with the UMIST rate16 chemical network.

# Copy the UMIST rate16 network to the krome tools directory
cp -r umist2krome_custm.py krome/tools/

if [ -z $arg ]
then
    echo "No chemical network specified as bash argument."
    echo "Please specify the network." 
    echo "Example: ./build_UMIST.sh umist_rate16"
    echo "---------------------------------------------"
    exit 1

fi

if [ $arg == 'umist_rate16' ]; then
    echo "umist rate16 network selected."
    cp -r umist_rate16.rates krome/tools/
else
    echo $arg "is not a present chemical network."
fi


# Navigate to correct krome directory
cd krome/tools

# Build the UMIST rate16 network in KROME format
pwd
echo ">> Building the UMIST rate16 network in KROME format."
python umist2krome_custm.py $arg
cp -r network_umist.dat ../networks/

echo "UMIST rate16 network has been converted to the correct format."
echo "KROME will now be built with the UMIST rate16 network."
echo "---------------------------------------------"
echo ""

# Load the krome module
cd ../
python krome -n networks/network_umist.dat -noSinkCheck -noRecCheck  -conserve -useN -noTlimits -unsafe -skipODEthermo -skipJacobian -iRHS 