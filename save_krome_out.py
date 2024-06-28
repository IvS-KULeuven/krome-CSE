import numpy as np
import sys


step = sys.argv[1]
print(step)
file = 'krome/build/KROME_ERROR_REPORT'


specs,output = np.loadtxt(file, skiprows=6, usecols=(1,2), max_rows=468, dtype='str', unpack=True)


outfile = 'out/'+step+'.out'
with open(outfile, 'w') as f:
    for i in range(0,len(specs)):
        f.write(f'{specs[i]} {output[i]}\n')