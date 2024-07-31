# krome-CSE
The chemistry code KROME is *in the process of* being made compatible with our AGB circumstellar envelope (CSE) chemistry, usually calculated using the [Rate22-CSE code](https://github.com/MarieVdS/rate22_cse_code), see also this [link](http://udfa.ajmarkwick.net/index.php?mode=downloads).

---

### How to run?
1. Run the bash script "build_UMIST.sh" with a chemical network as argument. 
2. Run the bash script "make_CSEkrome.sh". This will make an executable for file CSE_run_krome.f.
3. Run the bash script "run_CSE_krome.sh" with a inputfile as argument to properly run the executable.

---

### Updates

- 08/07/'24:
    
    KROME runs, but the radiation component for the photodissociation reactions does not work yet. 

- 12/07/'24:

    Correct rates, including radiation parameters $A_V$ and $\xi$ implemented (more info see [Maes et al. (2024)](https://ui.adsabs.harvard.edu/abs/2024ApJ...969...79M/abstract)), via script 
    ```
    umist2krome_custm.py
    ```
    originally written by Tommaso Grassi.

- 31/07/'24:

    Reading in parent species file + physical input parameters works. Initialising KROME works. Saving output of KROME works. 

    KROME chemical evolution does NOT coincide with our [Rate22-CSE code](https://github.com/MarieVdS/rate22_cse_code). We (Mats Esseldeurs and me, Silke Maes) think the issue lies within the cosmic ray reaction of H2. In the [Rate22-CSE code](https://github.com/MarieVdS/rate22_cse_code) it is assumed that H2 is fully self-shielding, so that it stays roughly constant throughout the CSE. At the moment, this assumption is not yet realised in the kromeCSE code.
