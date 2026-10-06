program main
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use krome_main
    use krome_user
    implicit none

    character(len=500) :: output_file, parent_file, input_file, profile_file
    character(len=500) :: directory, model_name, message, key, value
    character(len=2000) :: line
    character(len=16) :: species_names(krome_nmols)
    integer :: i, input_unit, parent_unit, profile_unit, output_unit, io_status
    integer :: separator, basename_start, row_count
    real(real64) :: timestep, abundance(krome_nmols), temperature
    real(real64) :: albedo, auv_av, zeta, velocity, physical(5), next_physical(5)
    logical :: have_velocity

    call get_command_argument(1, input_file, status=io_status)
    input_file = trim(input_file)
    if (io_status /= 0 .or. input_file == '') then
        error stop 'Usage: run_CSE_krome_1D models/inputChemistry_model_*.txt [parent_abundances [output_file]]'
    end if

    basename_start = scan(trim(input_file), '/', back=.true.) + 1
    model_name = input_file(basename_start:)
    if (index(trim(model_name), 'inputChemistry_') /= 1) error stop 'Expected an inputChemistry_model_*.txt file'
    model_name = model_name(len('inputChemistry_') + 1:)
    separator = len_trim(model_name)
    if (separator <= 4) error stop 'Missing model name in input filename'
    if (model_name(separator - 3:separator) /= '.txt') error stop 'Expected a .txt input file'
    model_name = model_name(:separator - 4)
    directory = input_file(:basename_start - 1)//trim(model_name)//'/'
    profile_file = trim(directory)//'csphyspar_smooth.out'
    output_file = trim(directory)//'csfrac_krome.out'
    if (command_argument_count() >= 2) then
        call get_command_argument(2, parent_file, status=io_status)
        if (io_status /= 0) error stop 'Cannot read parent abundance filename'
    else
        parent_file = ''
    end if
    if (command_argument_count() >= 3) then
        call get_command_argument(3, output_file, status=io_status)
        if (io_status /= 0) error stop 'Cannot read output filename'
    end if
    if (trim(output_file) == trim(profile_file) .or. trim(output_file) == trim(input_file) .or. &
        trim(output_file) == trim(parent_file)) error stop 'Output must not overwrite an input file'

    albedo = 0.5_real64
    auv_av = 4.65_real64
    zeta = 1.0_real64
    have_velocity = .false.
    open (newunit=input_unit, file=input_file, status='old', action='read', &
        iostat=io_status, iomsg=message)
    if (io_status /= 0) error stop trim(message)
    do
        read (input_unit, '(a)', iostat=io_status, iomsg=message) line
        if (is_iostat_end(io_status)) exit
        if (io_status /= 0) error stop trim(message)
        separator = index(line, '!')
        if (separator > 0) line = line(:separator - 1)
        separator = index(line, '=')
        if (separator == 0) cycle
        key = trim(adjustl(line(:separator - 1)))
        value = adjustl(line(separator + 1:))
        select case (trim(key))
        case ('VELOCITY')
            read (value, *, iostat=io_status, iomsg=message) velocity
            have_velocity = .true.
        case ('ALBEDO')
            read (value, *, iostat=io_status, iomsg=message) albedo
        case ('AUV_AV')
            read (value, *, iostat=io_status, iomsg=message) auv_av
        case ('ZETA')
            read (value, *, iostat=io_status, iomsg=message) zeta
        case default
            cycle
        end select
        if (io_status /= 0) error stop trim(message)
    end do
    close (input_unit)
    if (.not. have_velocity) error stop 'VELOCITY is required in the chemistry input'
    if (.not. ieee_is_finite(velocity) .or. velocity <= 0) error stop 'VELOCITY must be positive and finite'
    if (.not. ieee_is_finite(albedo) .or. albedo < 0 .or. albedo >= 1) error stop 'ALBEDO must be in [0, 1)'
    if (.not. ieee_is_finite(auv_av) .or. auv_av <= 0) error stop 'AUV_AV must be positive and finite'

    call krome_init()
    call krome_set_user_alb(albedo)
    call krome_set_user_AuvAv(auv_av)
    call krome_set_user_V(velocity)
    call krome_set_user_zeta(zeta)
    abundance = 0.0_real64
    if (parent_file /= '') then
        open (newunit=parent_unit, file=parent_file, status='old', action='read', &
            iostat=io_status, iomsg=message)
        if (io_status /= 0) error stop trim(message)
        do i = 1, krome_nmols
            read (parent_unit, *, iostat=io_status, iomsg=message) abundance(i)
            if (io_status /= 0) error stop trim(message)
        end do
        close (parent_unit)
    else
        abundance(KROME_idx_H2) = 0.5_real64
        abundance(KROME_idx_He) = 8.5e-2_real64
        abundance(KROME_idx_CO) = 4e-4_real64
        abundance(KROME_idx_C2H2) = 2.19e-5_real64
        abundance(KROME_idx_HCN) = 2.045e-5_real64
        abundance(KROME_idx_N2) = 2e-5_real64
        abundance(KROME_idx_SiC2) = 9.35e-6_real64
        abundance(KROME_idx_CS) = 5.3e-6_real64
        abundance(KROME_idx_SiS) = 2.99e-6_real64
        abundance(KROME_idx_SiO) = 2.51e-6_real64
        abundance(KROME_idx_CH4) = 1.75e-6_real64
        abundance(KROME_idx_H2O) = 1.275e-6_real64
        abundance(KROME_idx_HCl) = 1.625e-7_real64
        abundance(KROME_idx_C2H4) = 3.425e-8_real64
        abundance(KROME_idx_NH3) = 3e-8_real64
        abundance(KROME_idx_HCP) = 1.25e-8_real64
        abundance(KROME_idx_HF) = 8.5e-9_real64
        abundance(KROME_idx_H2S) = 2e-9_real64
        abundance(KROME_idx_e) = krome_get_electrons(abundance)
    end if
    if (.not. all(ieee_is_finite(abundance)) .or. any(abundance < 0)) then
        error stop 'Initial abundances must be nonnegative and finite'
    end if

    open (newunit=profile_unit, file=profile_file, status='old', action='read', &
        iostat=io_status, iomsg=message)
    if (io_status /= 0) error stop trim(message)
    do i = 1, 4
        read (profile_unit, '(a)', iostat=io_status, iomsg=message) line
        if (io_status /= 0) error stop trim(message)
    end do
    call read_physical(physical)
    if (is_iostat_end(io_status)) error stop 'Physical profile has no data rows'
    open (newunit=output_unit, file=output_file, status='replace', action='write', &
        iostat=io_status, iomsg=message)
    if (io_status /= 0) error stop trim(message)
    species_names = krome_get_names()
    write (output_unit, '(a)', advance='no') '# radius_cm'
    do i = 1, krome_nmols
        write (output_unit, '(1x,a)', advance='no') trim(species_names(i))
    end do
    write (output_unit, *)
    write (output_unit, '(*(es24.16e3,1x))') physical(1), abundance
    row_count = 1
    write (*, '(a)') 'Physical profile: '//trim(profile_file)
    do
        call read_physical(next_physical)
        if (is_iostat_end(io_status)) exit
        if (next_physical(1) <= physical(1)) error stop 'Profile radii must be strictly increasing'
        timestep = (next_physical(1) - physical(1)) / velocity
        temperature = physical(3)
        call krome_set_user_Auv(physical(4))
        call krome_set_user_xi(physical(5))
        abundance = abundance * physical(2)
        call krome(abundance, temperature, timestep)
        abundance = abundance / physical(2)
        write (output_unit, '(*(es24.16e3,1x))', iostat=io_status, iomsg=message) next_physical(1), abundance
        if (io_status /= 0) error stop trim(message)
        physical = next_physical
        row_count = row_count + 1
    end do
    close (profile_unit)
    close (output_unit)
    write (*, '(a,i0,a)') 'Wrote ', row_count, ' radial abundance rows to '//trim(output_file)

contains

    subroutine read_physical(values)
        real(real64), intent(out) :: values(5)

        do
            read (profile_unit, '(a)', iostat=io_status, iomsg=message) line
            if (is_iostat_end(io_status)) return
            if (io_status /= 0) error stop trim(message)
            if (len_trim(line) > 0) exit
        end do
        read (line, *, iostat=io_status, iomsg=message) values
        if (io_status /= 0) error stop trim(message)
        if (.not. all(ieee_is_finite(values))) error stop 'Physical profile contains nonfinite values'
        if (any(values(:3) <= 0)) error stop 'Radius, density, and temperature must be positive'
        if (any(values(4:) < 0)) error stop 'Extinction and radiation must be nonnegative'
    end subroutine read_physical
end program main