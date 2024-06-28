import numpy            as np


class CSEmod():
    '''
    Class to load a 1D CSE model, calculated with the classical fortan code.
    For more info on this model, see https://github.com/MarieVdS/rate22_cse_code.
    '''
    def __init__(self, path):
        '''
        Load the 1D CSE model, given a path.

        The abundances are stored in a file 'csfrac_smooth.out', 
        the physical parameters are stored in a file 'csphyspar_smooth.out'.
        The input of the 1D CSE model is stored in a file 'inputChemistry_*.txt'.

        From these paths, retrieve
            - the abundances            --> self.n
            - the physical parameters (for more info on the parameters, see the paper)
                - radius                --> self.radius
                - density               --> self.dens
                - temperature           --> self.temp
                - visual extinction     --> self.Av
                - radiation parameter   --> self.xi
            - the time steps            --> self.time
            - input --> self.Rstar, self.Tstar, self.Mdot, self.v, self.eps, self.rtol, self.atol
        '''

        self.path = '/STER/silkem/CSEchem/' + path[:35] + '/'
        self.name = path[36:-1]
        data_path = self.path + '/models/'+self.name+'/'
        inp_path = self.path + 'models/inputChemistry_'+self.name+'.txt'

        abs_path = 'csnum_smooth.out'
        phys_path = 'csphyspar_smooth.out'

        ## retrieve input
        self.Rstar, self.Tstar, self.Mdot, self.v, self.eps, self.rtol, self.atol = read_input_1Dmodel(inp_path)

        ## retrieve abundances
        abs = read_data_1Dmodel(data_path+abs_path)
        self.n = abs

        ## retrieve physical parameters
        arr = np.loadtxt(data_path+phys_path, skiprows=4, usecols=(0,1,2,3,4))
        self.radius, self.dens, self.temp, self.Av, self.xi = arr[:,0], arr[:,1], arr[:,2], arr[:,3], arr[:,4]
        self.time = self.radius/(self.v) 
                

    def __len__(self):
        '''
        Return the length of the time array, which indicates the length of the 1D model.
        '''

        return len(self.time)

    def get_time(self):
        '''
        Return the time array of the 1D model.
        '''
        return self.time
    
    def get_phys(self):
        '''
        Return the physical parameters of the 1D model.
        '''
        return self.dens, self.temp, self.xi, self.Av
    
    def get_abs(self):
        '''
        Return the abundances of the 1D model.
        '''
        return self.n

    def get_dens(self):
        '''
        Return the density of the 1D model.
        '''
        return self.dens 
    
    def get_temp(self):
        '''
        Return the temperature of the 1D model.
        '''
        return self.temp

    def get_Av(self):
        '''
        Return the visual extinction of the 1D model.
        '''
        return self.Av
    
    def get_xi(self):
        '''
        Return the radiation parameter of the 1D model.
        '''
        return self.xi

    def get_vel(self):
        '''
        Return the velocity of the 1D model.
        '''
        return self.v
    
    def get_path(self):
        '''
        Return the path of the 1D model.
        '''
        return self.path

    def get_name(self):
        '''
        Return the name of the 1D model.
        '''
        return self.name
    
    def get_dt(self):
        '''
        Return the time steps of the 1D model.
        '''
        return self.time[1:] - self.time[:-1]
    
    def split_in_0D(self):
        '''
        Split the 1D model in 0D models.
        '''
        Δt   = self.get_dt()
        n_0D = self.get_abs()
        y    = 1.e-100  ## this number is added to xi, since it contains zeros
        p    = np.array([self.get_dens()[:-1], self.get_temp()[:-1], self.get_xi()[:-1]+y, self.get_Av()[:-1]])

        return Δt.astype(np.float64), n_0D.astype(np.float64), p.T.astype(np.float64)
    
        


def read_input_1Dmodel(file_name):
    '''
    Read input text file of 1D CSE models, given the filename.
    '''
    with open(file_name, 'r') as file:
        lines = file.readlines()
        lines = [item.rstrip() for item in lines]

    Rstar = float(lines[3][9:])
    Tstar = float(lines[4][9:])
    Mdot  = float(lines[5][8:])     ## Msol/yr
    v     = float(lines[6][11:])    ## sec
    eps   = float(lines[8][19:])

    rtol = float(lines[31][7:])
    atol = float(lines[32][6:])

    return Rstar, Tstar, Mdot, v, eps, rtol, atol


def read_data_1Dmodel(file_name):
    '''
    Read data text file of output abundances of 1D CSE models.

    This data text file is build up in an inconvenient way,
    hence the data is read in this specific way.

    The data is stored in a numpy array.
    '''
    with open(file_name, 'r') as file:
        part = []
        full = None
        for line in file:
            try:  
                if len(line) > 1: 
                    part.append([float(el) for el in line.split()])
            except:
                if len(part) != 0:
                    part = np.array(part)[:,1:]
                    if full is None:
                        full = part
                    else:
                        full = np.concatenate((full, part), axis = 1)
                part = []
    return full