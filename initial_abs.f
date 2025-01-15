      PROGRAM MAIN
      use krome_main 
      use krome_user
      IMPLICIT NONE

      CHARACTER*500 FOUTF,FPARENTS,INFILE,DUMMY
      CHARACTER*500 DIR, kfile, IDX
      INTEGER OU, N, NR
      INTEGER I,J
      DOUBLE PRECISION TSTART,Y(krome_nmols),T,SH,
     *     X(10),GR,DN,TFINAL,PI,KB,MH,MU,
     *     A_G, AUV, ALBEDO, RAD, TEMP,
     *     test_Av, test_xi, test_alb, AuvAv
      DOUBLE PRECISION, DIMENSION(6180) :: k
C  NC = Number of conserved species, NR = Number of reactions, N = Number of species (ODEs)
      PARAMETER(OU=8,NR=6173,N=krome_nmols)

C  PHYSICAL CONSTANTS
      DATA PI,MH,MU,KB/3.1415927,1.6605E-24,2.2,1.3807E-16/
C
C      write(*,*) '--------------------------------'
      write(*,*) ' >> Initial_abs is running ...'
C      write(*,*) '--------------------------------'


      call krome_init()

      Y(:) = 0.
      Y(KROME_idx_H2)   = 0.5d0
      Y(KROME_idx_He)   = 8.5d-2
      Y(KROME_idx_CO)   = 4d-4
      Y(KROME_idx_C2H2) = 2.19d-5
      Y(KROME_idx_HCN)  = 2.045d-5
      Y(KROME_idx_N2)   = 2d-5
      Y(KROME_idx_SiC2) = 9.35d-6
      Y(KROME_idx_CS)   = 5.3d-6
      Y(KROME_idx_SiS)  = 2.99d-6
      Y(KROME_idx_SiO)  = 2.51d-6
      Y(KROME_idx_CH4)  = 1.75d-6
      Y(KROME_idx_H2O)  = 1.275d-6
      Y(KROME_idx_HCl)  = 1.625d-7
      Y(KROME_idx_C2H4) = 3.425d-8
      Y(KROME_idx_NH3)  = 3d-8
      Y(KROME_idx_HCP)  = 1.25d-8
      Y(KROME_idx_HF)   = 8.5d-9
      Y(KROME_idx_H2S)  = 2d-9
      Y(KROME_idx_e) = krome_get_electrons(Y(:))

      CALL GETARG(1,FOUTF)

      OPEN(UNIT=222, FILE=FOUTF, STATUS = 'REPLACE')
      do i=1, size(Y)
          write(222, 100) Y(i)
      enddo
      CLOSE(UNIT=222)

 100  FORMAT(20X,ES11.2E3)
      
      end program main