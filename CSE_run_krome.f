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
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      CHARACTER*10  SP(468),CSP(2),RE1,RE2,P1,P2,P3,P4,PARENT(468)
      CHARACTER*5 FLG
      CHARACTER*1 SR
      CHARACTER*11 JAC
      CHARACTER*500 FRATES,FSPECS,FOUTF,FOUTN,FPARENTS,INFILE,DUMMY
      CHARACTER*500 FTIME
      INTEGER OU
      INTEGER IWORK(1025),I,J,FSPEC,FRATE,FFRAC,FNUM,UFRAC,URATES,USPEC,
     *     NSPEC,NCONS,NPAR,ICO,ICOR,IRUN,ITOL,ITASK,ISTATE,IOPT,LRW,
     *     LIW,MF,NREAC,IS,IF,LI,LTAG,NEXTRA,IFREEZE,IANA,UANA,
     *     UPARENTS, UIN, UTIME
      DOUBLE PRECISION K(10000),MASS(468),NGD,RTOL,ATOL,TSTART,Y(468),
     *     T,RWORK(2000000),TOTAL(10),B(468,300),CINP(2),TAGE(300),SH,
     *     X(10),GR,DN,TFINAL,TLAST,KJ,ACCR,HNR,PI,KB,MH,MU,PABUND(468)
      COMMON/BL1/ K,X,TOTAL,GR,DN,ACCR,HNR,IFREEZE
      COMMON/BL10/ MASS,SH
      COMMON/BL3/ Y,X_G,A_G,TEMP,AV,ZETA,ALBEDO,RAD
C  NC = Number of conserved species, NR = Number of reactions, N = Number of species (ODEs)
      PARAMETER(OU=8,NC=2,NR=6173,N=468)

C  PHYSICAL CONSTANTS
      DATA PI,MH,MU,KB/3.1415927,1.6605E-24,2.2,1.3807E-16/
C
      write(*,*) '--------------------------------'
      write(*,*) 'CSE_run_krome is running ...'
      write(*,*) '--------------------------------'


C  SPECIES FILE NAME
      FSPECS = 'rate16_krome.specs' 
C  OUTPUT FILE NAME
c      FOUTF = 'fortrandc13-C-rho5.63E+06-T2500-d1-Av1.out' 
      
c      FPARENTS = 'rates/C.parents'
C
      USPEC = 1
      UFRAC = 3
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
      write(*,*) 'Input file:', INFILE

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
      READ(UIN,*) DUMMY,DUMMY,AV
      READ(UIN,*) DUMMY,DUMMY,TFINAL
      READ(UIN,*) DUMMY,DUMMY,TSTART
      READ(UIN,*) DUMMY,DUMMY,FPARENTS
      READ(UIN,*) DUMMY,DUMMY,FOUTF
      READ(UIN,*) DUMMY,DUMMY,FTIME
      CLOSE(UNIT=UIN)

      OPEN(UNIT=UTIME, FILE=FTIME)


      
c      
C  OPEN RATE, SPECIES AND OUTPUT FILES
      OPEN(UNIT=USPEC, FILE=FSPECS, STATUS='OLD')
      OPEN(UNIT=UPARENTS, FILE=FPARENTS, STATUS='OLD')
      OPEN(UNIT=UFRAC, FILE=FOUTF)
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
C
C  H-ATOM ACCRETION RATE
      ACCR = SH*PI*(A_G**2.0)*DN*X_G*(8.0*KB*TEMP/(PI*MH))**0.5
C SET GRAIN SURFACE FORMATION OF H2 TO CLASSICAL RATE
C     GR = 5.2E-17*(TEMP*3.33E-3)**0.5
C     ACCR = GR*DN


      WRITE(*,*)'--------------------------------'
      WRITE(*,*)'Input parameters:'
      WRITE(*,*)'       dens   ',DN
      WRITE(*,*)'       temp   ',TEMP
      WRITE(*,*)'       RAD    ',RAD
      WRITE(*,*)'       Av     ',AV
      WRITE(*,*)'       TFINAL ',TFINAL
      WRITE(*,*)'--------------------------------'
C
c Input section
c -------------
C read species file.. get species names, masses and initial abundances
C
C  parent species tov H
C  density #/cm3
c      WRITE(*,*)'Initially, relative to H2'
      WRITE(11,*)'Initially, relative to H2'
      DO 2 I = 1,N
c    print *, 'Reading species:', I
         READ(UPARENTS,100) Y(I)
         IF(Y(I).GT.0) THEN
c            WRITE(*,100)SP(I),2*Y(I),MASS(I)
C CONVERT ABUNDANCES TO PER UNIT VOLUME RATHER THAN FRACTIONAL WRT H2
c            Y(I)=Y(I)*DN
            Y(I)=Y(I)
         ENDIF
 2    CONTINUE
C


c      print *, 'Initial abundances:', Y

      write(*,*) 'num densities loaded'

      call krome_init()
      call krome(Y,  TEMP, TFINAL-TSTART)
c      call krome_dump(n,rwork,iwork,ni)


 100  FORMAT(20X,E20.2)
      
      end program main