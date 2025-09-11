#!/bin/bash
echo "---------------------------------------------"


# Parse main network argument and additional options
arg=$1
shift
extra_args="$@"

# This script is used to build the krome code compatible with the UMIST rate16 and rate22 chemical network.

# Copy the UMIST rate16 network to the krome tools directory
cp -r umist2krome_custm.py krome/tools/

if [ -z "$arg" ]
then
    echo "No chemical network specified as bash argument."
    echo "Please specify the network." 
    echo "Example: ./build_UMIST.sh umist_rate16"
    echo "Example: ./build_UMIST.sh umist_rate22"
    echo "You can also add options: ./build_UMIST.sh umist_rate16 -IP"
    echo "Or: ./build_UMIST.sh umist_rate22 -AP"
    echo "---------------------------------------------"
    exit 1

fi

if [ $arg == 'umist_rate16' ]; then
    echo "umist rate16 network selected."
    cp -r umist_rate16.rates krome/tools/
elif [ $arg == 'umist_rate22' ]; then
    echo "umist rate22 network selected."
    cp -r ../rate22_cse_code/rate22_final.rates krome/tools/umist_rate22.rates
    if [ -z "$extra_args" ]; then
        echo "No extra arguments provided."
    else
        echo "Extra arguments provided: $extra_args"
        if [[ "$extra_args" == *"-AP"* ]]; then
            ap_value=$(echo "$extra_args" | grep -oP '(?<=-AP=)[^ ]*')
            echo "AP option detected. Activating accretion photons for a companion with $ap_value K."
            [ -n "$ap_value" ] && cp -r ../rate22_cse_code/AP_"$ap_value"K.rates krome/tools/
        fi
        if [[ "$extra_args" == *"-IP"* ]]; then
            echo "IP option detected. Deactivating all photoreactions."
            cp -r ../rate22_cse_code/IP.rates krome/tools/
        fi
    fi
else
    echo $arg "is not a present chemical network."
    exit 1
fi

# Navigate to correct krome directory
cd krome/tools

# Build the UMIST rate16 network in KROME format
pwd
echo ">> Building the UMIST rate16 network in KROME format."
python umist2krome_custm.py $arg $extra_args
exit 1
# Add shielding to the H2 reaction (deactivate the H2 photodissociation reaction)
sed -i '/,H2,,H,H,,,.*user_xi/s/$/ * 0/' network_umist.dat

# Copy the network to the networks directory
cp -r network_umist.dat ../networks/

echo "UMIST rate16 network has been converted to the correct format."
echo "KROME will now be built with the UMIST rate16 network."
echo "---------------------------------------------"
echo ""


# Load the krome module
cd ../
# Pass extra_args to krome if present
python krome -n networks/network_umist.dat -iRHS -noSinkCheck -noRecCheck -noTlimits -unsafe -skipODEthermo -skipJacobian $extra_args
# -shielding R14
