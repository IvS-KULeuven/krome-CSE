#!/bin/bash

for i in {1..134}
do
    echo "Running KROME for input_$i.in"
    echo "--------------------------------"
    ./run_CSE_krome.sh /STER/silkem/fortran_0D/input/20211015_gridC_Mdot1e-6_v17-5_T_eps_model_2022-12-24h17-06-51/krome/input_$i.in
    python save_krome_out.py $i
done