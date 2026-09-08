#!/bin/bash

cd krome/build
for i in {1..133}
do
    echo ""
    echo "--------------------------------"
    echo "Running KROME for input_$i.in"
    ./run_CSE_krome ../../data/20211015_gridC_Mdot1e-6_v17-5_T_eps_model_2022-12-24h17-06-51/input/$i.in
done