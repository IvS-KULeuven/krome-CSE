C
C made by rate version 1.5 on Tue, May 15 2012 at 13:24:22
C
c issues users need to be aware of:
c ---------------------------------
c  expects species file 'dc.specs' with initial abundances filled in
c  produces steady state abundance file 'rate13steady.state'
c  produces full output file 'dc.out'
c  abundances in the output files are relative to H2 
c  except that of H2 itself.
c
c  this program needs to be compiled and linked to the odes file odes.f
c  and also to the GEAR package LSODE.
c
c  for more info see http://www.udfa.net
c
      PROGRAM MAIN
      use krome_main 
      use krome_user
      IMPLICIT NONE

      CHARACTER*500 FSPECS,FOUTF,FPARENTS,INFILE,DUMMY
      CHARACTER*500 DIR, kfile, IDX
      INTEGER OU, N, IANA, IRUN, UANA, NR
      INTEGER I,J,FSPEC,URATES,USPEC,UPARENTS, UIN, UTIME
      DOUBLE PRECISION TSTART,Y(468),T,SH,
     *     X(10),GR,DN,TFINAL,ACCR,HNR,PI,KB,MH,MU,
     *     ZETA, A_G, X_G, AUV, ALBEDO, RAD, TEMP,
     *     test_Av, test_xi, test_alb, AuvAv
      DOUBLE PRECISION, DIMENSION(6180) :: k
c      COMMON/BL1/ X,GR,DN,ACCR,HNR
c      COMMON/BL10/ SH
c      COMMON/BL3/ Y,X_G,A_G,TEMP,AV,ZETA,ALBEDO,RAD
C  NC = Number of conserved species, NR = Number of reactions, N = Number of species (ODEs)
      PARAMETER(OU=8,NR=6173,N=468)

C  PHYSICAL CONSTANTS
      DATA PI,MH,MU,KB/3.1415927,1.6605E-24,2.2,1.3807E-16/
C
C      write(*,*) '--------------------------------'
      write(*,*) ' >> CSE_run_krome is running ...'
C      write(*,*) '--------------------------------'


C  SPECIES FILE NAME
      FSPECS = 'rate16_krome.specs' 
C  OUTPUT FILE NAME
c      FOUTF = 'fortrandc13-C-rho5.63E+06-T2500-d1-Av1.out' 
      
c      FPARENTS = 'rates/C.parents'
C
      USPEC = 1
      URATES = 4
      UANA = 5
      UPARENTS = 37
      UIN = 16
      UTIME = 12
      
C  ANALYSE CHEMISTRY?
      IANA = 0
C  ANALYSIS TIME (IRUN)
      IRUN = 51

C  ---> READ IN INPUT PARAMETERS
CCCCCCCCCCCCCCCCCCCCCCCCCCCC

      CALL GETARG(1,INFILE)

      INFILE = TRIM(INFILE)
c      write(*,*) 'Input file:', INFILE

      IF(INFILE.EQ.'') THEN
          WRITE(*,*) 'ERROR: No input file specified!'
          WRITE(*,*) 'Useage: % ./run_CSE_krome input_parameters.in'
          STOP
      END IF

      OPEN(UNIT=UIN,FILE=INFILE)
C      DO I=1,4
C         READ(UIN,*)
C      END DO
      READ(UIN,*) DUMMY,DUMMY,DN
      READ(UIN,*) DUMMY,DUMMY,TEMP
      READ(UIN,*) DUMMY,DUMMY,RAD
      READ(UIN,*) DUMMY,DUMMY,AUV
      READ(UIN,*) DUMMY,DUMMY,TFINAL
      READ(UIN,*) DUMMY,DUMMY,TSTART
      READ(UIN,*) DUMMY,DUMMY,FPARENTS
      READ(UIN,*) DUMMY,DUMMY,FOUTF
      READ(UIN,*) DUMMY,DUMMY,DIR
      READ(UIN,*) DUMMY,DUMMY,IDX
      CLOSE(UNIT=UIN)

     
c      
C  open parent species file
      OPEN(UNIT=UPARENTS, FILE=FPARENTS, STATUS='OLD')
     
C
C  Physical parameters - temperature, density, cosmic ray
c  ionisation and UV radiation field scaling factors and visual extinction

      ZETA = 1.0
C  GRAIN RADIUS (cm) (0.1 micron = 1.0E-5 cm)
      A_G = 1.0E-5
C  GRAIN NUMBER DENSITY/H2 (assuming gas/dust = 200, rho = 3.5 g/cm^3)
C  USED FOR H-ATOM ACCRETION CALCULATION
      X_G = 1.5E-12
C	HNR = 1.0 FOR DENSE CLOUD CHEMISTRY
      HNR = 1.0
C   GRAIN ALBEDO
      ALBEDO = 0.5
C SET STICKING COEFFICIENT FOR H ATOMS
      SH = 0.3

      AuvAv = 4.65
C
      ACCR = SH*PI*(A_G**2.0)*DN*X_G*(8.0*KB*TEMP/(PI*MH))**0.5
C  H-ATOM ACCRETION RATE
C SET GRAIN SURFACE FORMATION OF H2 TO CLASSICAL RATE
C     GR = 5.2E-17*(TEMP*3.33E-3)**0.5
C     ACCR = GR*DN


C      WRITE(*,*)'--------------------------------'
      WRITE(*,*)'Input parameters:'
      WRITE(*,*)'       dens   ',DN
      WRITE(*,*)'       temp   ',TEMP
      WRITE(*,*)'       RAD    ',RAD
      WRITE(*,*)'       Auv    ',AUV
      WRITE(*,*)'       time ',TFINAL-TSTART
C      WRITE(*,*)'--------------------------------'
C
c Input section
c -------------
C read species file.. get species names, masses and initial abundances
C
C  parent species tov H2
C  density #/cm3
      DO 2 I = 1,N
c    print *, 'Reading species:', I
         READ(UPARENTS,100) Y(I)
c        write(*,*) Y(I)
         IF(Y(I).GT.0) THEN
            Y(I)=Y(I) 
c          write(*,*) Y(I)
         ENDIF
 2    CONTINUE


      call krome_init()

      call krome_set_user_Auv(AUV)
      call krome_set_user_xi(RAD)
      call krome_set_user_alb(ALBEDO)
      call krome_set_user_AuvAv(AuvAv)

c     Normalise abudances and balance charge conservation with e-
c      call krome_consistent_x(Y)

c     Test if values are correct in KROME
c
c      test_Av =  krome_get_user_av()
c      test_xi =  krome_get_user_xi()
c      test_alb =  krome_get_user_alb()
c      WRITE(*,*) 'xi  ',test_xi
c      WRITE(*,*) 'Av  ',test_Av
c      WRITE(*,*) 'abl ',test_alb
c
      kfile = DIR 
c      write(*,*) 'kfile: ', kfile

      k = krome_get_coef(TEMP,Y)
      OPEN(UNIT=223, FILE=kfile, STATUS = 'REPLACE')
      do i=1, size(k)
          write(223, 101) i, k(i)
      enddo



      DN = (DN * (2.0 + 4.0*0.17) * 1.6605E-24  ) 
c     ! DN = HNR * mu * mH   w.r.t. H2

      call krome(Y, DN,  TEMP, TFINAL-TSTART)

      

      OPEN(UNIT=222, FILE=FOUTF, STATUS = 'REPLACE')
      do i=1, size(Y)
          write(222, 100) Y(i)
      enddo

    



 100  FORMAT(20X,ES11.2E3)
 101  FORMAT(20X,I4,ES11.2E3)
      
      end program main