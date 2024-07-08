import numpy as np
import sys


step = int(sys.argv[1])
print(step)
file = 'krome/build/KROME_ERROR_REPORT'


specs,output = np.loadtxt(file, skiprows=6, usecols=(1,2), max_rows=468, dtype='str', unpack=True)


# outfile = 'out/'+step+'.out'

outfile = '/STER/silkem/fortran_0D/rates/20211015_gridC_Mdot1e-6_v17-5_T_eps_model_2022-12-24h17-06-51/krome/abs_'+str(step+1)+'.txt'

with open(outfile, 'w') as f:
    for i in range(0,len(specs)):
        f.write(f'{i+1:>5} {specs[i]:<15} {output[i]}\n')