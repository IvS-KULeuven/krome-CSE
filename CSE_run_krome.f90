program main
    use, intrinsic :: iso_fortran_env, only: real64
    use krome_main
    use krome_user
    implicit none

    character(len=500) :: output_file, parent_file, input_file
    character(len=500) :: directory, index, label, message
    integer :: i, input_unit, parent_unit, output_unit, io_status
    real(real64) :: timestep, abundance(krome_nmols), density, final_time
    real(real64) :: temperature, radiation, auv, albedo, auv_av, CO_shielding
    real(real64) :: frace, lamdae, fosce, bands, ge0, taue
    real(real64) :: gammad, betae, getcor, h2col, xco, v

    call get_command_argument(1, input_file, status=io_status)
    input_file = trim(input_file)
    if (io_status /= 0 .or. input_file == '') then
        error stop 'Usage: ./run_CSE_krome input_parameters.in'
    end if

    open (newunit=input_unit, file=input_file, status='old', action='read', &
        iostat=io_status, iomsg=message)
    if (io_status /= 0) error stop trim(message)

    read (input_unit, *, iostat=io_status, iomsg=message) label, label, density
    read (input_unit, *, iostat=io_status, iomsg=message) label, label, temperature
    read (input_unit, *, iostat=io_status, iomsg=message) label, label, radiation
    read (input_unit, *, iostat=io_status, iomsg=message) label, label, auv
    read (input_unit, *, iostat=io_status, iomsg=message) label, label, final_time
    read (input_unit, *, iostat=io_status, iomsg=message) label, label, timestep
    read (input_unit, *, iostat=io_status, iomsg=message) label, label, parent_file
    read (input_unit, *, iostat=io_status, iomsg=message) label, label, output_file
    read (input_unit, *, iostat=io_status, iomsg=message) label, label, directory
    read (input_unit, *, iostat=io_status, iomsg=message) label, label, index
    close (input_unit)
    if (io_status /= 0) error stop trim(message)

    albedo = 0.5_real64
    auv_av = 4.65_real64

    write (*, '(a)') ' >> CSE_run_krome is running ...'
    write (*, '(a)') '--------------------------------'
    write (*, '(a)') 'Input parameters:'
    write (*, '(a,1x,es12.5)') '       dens  ', density
    write (*, '(a,1x,es12.5)') '       temp  ', temperature
    write (*, '(a,1x,es12.5)') '       RAD   ', radiation
    write (*, '(a,1x,es12.5)') '       Auv   ', auv
    write (*, '(a,1x,es12.5)') '       time  ', timestep
    write (*, '(a)') '--------------------------------'

    open (newunit=parent_unit, file=parent_file, status='old', action='read', &
        iostat=io_status, iomsg=message)
    if (io_status /= 0) error stop trim(message)
    do i = 1, krome_nmols
        read (parent_unit, '(20x,es11.2e3)', iostat=io_status, iomsg=message) abundance(i)
        if (io_status /= 0) error stop trim(message)
    end do
    close (parent_unit)

    call krome_init()
    call krome_set_user_Auv(auv)
    call krome_set_user_xi(radiation)
    call krome_set_user_alb(albedo)
    call krome_set_user_AuvAv(auv_av)

    ! fractional population of lower level
    frace = 1.0_real64 / 3.0_real64
    ! effective dissociative oscillator strength
    fosce = 0.017_real64
    ! effective wavelength (in cm)
    lamdae = 1000.0_real64 * 1.0e-8_real64
    ! effective number of bands
    bands = 1.0_real64
    ! unshielded photodissociation rate of co
    ge0 = 2.4e-10_real64
    ! calculate h2 column density
    h2col = auv / auv_av * 1.87e21_real64
    ! fractional abundance of co
    xco = abundance(krome_idx_CO)
    ! velocity (in cm/s)
    v = 17.5e5_real64
    ! calculate effective optical depth of co at radius
    taue = 0.0265_real64 * frace * fosce * lamdae * h2col * xco / v
    ! calculate continuum shielding by dust (morris and jura)
    gammad = exp(-1.644_real64 * auv**0.86_real64)
    ! morris/jura approximation to the full integral
    betae = (1.0_real64 - exp(-1.5_real64 * taue)) / (1.5_real64 * taue)
    ! calculate co photodissociation rate
    getcor = ge0 * betae * gammad * bands

    call krome_set_user_CO_shielding(getcor)
    print *, 'CO_shielding = ', getcor
      ! if krome_set_user_rscale routine exists, call it here to set the radial scale.
      ! RADIUS AT WHICH PHOTORATES DUE TO BINARY PHOTONS ARE CALCULATED
      ! RSCALE = 50 * R_STAR
      ! if (present(krome_set_user_rscale)) then
      !     call krome_set_user_rscale(RSCALE)
      ! endif

      ! if krome_set_user_Gstar routine exists, call it here to set the stellar radiation field.
      ! SET RATIO OF INTEGRATED UV FLUX AT RSCALE = 50 * R_STAR IN IRC+20126
      ! TO INTEGRATED DRAINE ISM FIELD
      ! INTEGRATED DRAINE UV FIELD = 5.89e11 PHOTONS M-2 S-1 SR-1 OVER 912-2050 A
      ! G_STAR = GETGSTAR(T_STAR,RSCALE/R_STAR)
      ! if (present(krome_set_user_Gstar)) then
      !     call krome_set_user_Gstar(G_STAR)
      ! endif

      ! if krome_set_user_Auv_star routine exists, call it here to set the stellar UV flux.
      ! if (present(krome_set_user_Auv_star)) then
      !     call krome_set_user_Auv_star(AUV_star)
      ! endif

      ! if krome_set_user_rbinscale routine exists, call it here to set the radial binary scale.
      !  SET RADIUS AT WHICH PHOTORATES DUE TO BINARY PHOTONS ARE CALCULATED
      !  THIS RADIUS IS TAKEN TO BE THE SAME AS FOR STELLAR PHOTONS,
      !  ENABLING TO DIRECTLY COMPARE PHOTORATES
      !  BINSCALE = RSCALE
      !  GBIN = GETGSTAR(TBIN,BINSCALE/RBIN)
      !  if (present(krome_set_user_rbinscale)) then
      !     call krome_set_user_rbinscale(BINSCALE)
      ! endif

      ! if krome_set_user_Gcomp routine exists, call it here to set the composite radiation field.
      ! GBIN = GETGSTAR(TBIN,BINSCALE/RBIN)
      ! if (present(krome_set_user_Gcomp)) then
      !     call krome_set_user_Gcomp(GBIN)
      ! endif

      ! if krome_set_user_Auv_comp routine exists, call it here to set the composite UV flux.
      ! if (present(krome_set_user_Auv_comp)) then
      !     call krome_set_user_Auv_comp(AUV_comp)
      ! endif

    abundance = abundance * density
    call krome(abundance, temperature, timestep)
    abundance = abundance / density

    open (newunit=output_unit, file=output_file, status='replace', action='write', &
        iostat=io_status, iomsg=message)
    if (io_status /= 0) error stop trim(message)
    do i = 1, size(abundance)
        write (output_unit, '(20x,es11.2e3)', iostat=io_status, iomsg=message) abundance(i)
        if (io_status /= 0) error stop trim(message)
    end do
    close (output_unit)
end program main