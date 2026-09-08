      PROGRAM MAIN
      use krome_main 
      use krome_user
      IMPLICIT NONE

      CHARACTER*500 FOUTF,FPARENTS,INFILE,DUMMY
      CHARACTER*500 DIR, IDX
      INTEGER j,UPARENTS, UIN
      DOUBLE PRECISION dt,Y(krome_nmols),
     *     DN,TFINAL,PI,
     *     AUV, ALBEDO, RAD, TEMP,
     *     AuvAv

      ! PHYSICAL CONSTANTS
      DATA PI/3.1415927/

      print*,  ' >> CSE_run_krome is running ...'

      UPARENTS = 37
      UIN = 16

      ! ---> READ IN INPUT PARAMETERS

      CALL GETARG(1,INFILE)
      INFILE = TRIM(INFILE)
      IF(INFILE.EQ.'') THEN
          print*,  'ERROR: No input file specified!'
          print*,  'Useage: % ./run_CSE_krome input_parameters.in'
          STOP
      END IF

      OPEN(UNIT=UIN,FILE=INFILE)

      READ(UIN,*) DUMMY,DUMMY,DN
      READ(UIN,*) DUMMY,DUMMY,TEMP
      READ(UIN,*) DUMMY,DUMMY,RAD
      READ(UIN,*) DUMMY,DUMMY,AUV
      READ(UIN,*) DUMMY,DUMMY,TFINAL
      READ(UIN,*) DUMMY,DUMMY,dt
      READ(UIN,*) DUMMY,DUMMY,FPARENTS
      READ(UIN,*) DUMMY,DUMMY,FOUTF
      READ(UIN,*) DUMMY,DUMMY,DIR
      READ(UIN,*) DUMMY,DUMMY,IDX
      CLOSE(UNIT=UIN)

      ALBEDO = 0.5
      AuvAv = 4.65

      print*, '--------------------------------'
      print*, 'Input parameters:'
      print*, '       dens   ',DN
      print*, '       temp   ',TEMP
      print*, '       RAD    ',RAD
      print*, '       Auv    ',AUV
      print*, '       time   ',dt
      print*, '--------------------------------'

      ! open parent species file
      OPEN(UNIT=UPARENTS, FILE=FPARENTS, STATUS='OLD')
      ! parent species tov H2
      ! density #/cm3
      do j = 1,krome_nmols
         read(UPARENTS,100) Y(j)
      enddo

      call krome_init()

      call krome_set_user_Auv(AUV)
      call krome_set_user_xi(RAD)
      call krome_set_user_alb(ALBEDO)
      call krome_set_user_AuvAv(AuvAv)

      Y = Y*DN
      call krome(Y,  TEMP, dt)
      Y = Y/DN

      OPEN(UNIT=222, FILE=FOUTF, STATUS = 'REPLACE')
      do j=1, size(Y)
          write(222, 100) Y(j)
      enddo
      CLOSE(UNIT=222)

 100  FORMAT(20X,ES11.2E3)
      
      end program main