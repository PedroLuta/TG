program BEMT_Pedrinho
    implicit none

    real, parameter                         ::  pi = 4*atan(1.)

    integer                                 ::  err_status
    character (len = 256)                   ::  err_msg
    character (len = 256)                   ::  dummy_string

    character (len = 256)                   ::  input_prop_file, input_run_file
    character (len = 1000)                  ::  line
    integer                                 ::  input_prop_unit, input_run_unit

    character (len = 256)                   ::  prop_name
    integer                                 ::  prop_n_blades
    integer                                 ::  prop_n_stations
    real                                    ::  prop_radius_m

    real                                    ::  axial_velocity_m_s
    real                                    ::  planar_velocity_m_s
    real                                    ::  rpm
    real                                    ::  rho_kg_m3

    real, allocatable                       ::  radius_vec_adim(:)     !r/R
    real, allocatable                       ::  chord_vec_adim(:)      !c/R
    real, allocatable                       ::  twist_vec_deg(:)      
    character (len = 256), allocatable      ::  airfoil_vec(:)      

    real                                    ::  current_rR, current_cR, current_twist_deg, current_airfoil, current_azim_deg
    real                                    ::  current_tangential_velocity_m_s, current_equivalent_flow_velocity_m_s, current_reynolds

    integer                                 ::  i, j, k

 !===============================================GET INFO FROM INPUT FILES============================================
    call getarg(1, input_prop_file)
    call getarg(2, input_run_file)

    open(newunit = input_prop_unit, file = input_prop_file, status = 'old', iostat = err_status, iomsg = err_msg)
    if (err_status .ne. 0) then; write(*,*) "ERROR -> " // trim(err_msg); write(*, "(a)") err_msg; stop; end if
    open(newunit = input_run_unit, file = input_run_file, status = 'old', iostat = err_status, iomsg = err_msg)
    if (err_status .ne. 0) then; write(*,*) "ERROR -> " // trim(err_msg); write(*, "(a)") err_msg; stop; end if

   !----------------------------------------name--------------------------------------------------
    line = "!"
    do while ((trim(line) .eq. "") .or. (line(1:1) .eq. '!'))
        read(input_prop_unit, '(a)', end = 200) line
        line = trim_front(line)
    end do
    prop_name = trim(line) 
    write(*, *) trim(prop_name)

   !----------------------------------------number of blades--------------------------------------
    line = "!"
    do while ((trim(line) .eq. "") .or. (line(1:1) .eq. '!'))
        read(input_prop_unit, '(a)', end = 200) line
        line = trim_front(line)
    end do
    read(line, *) prop_n_blades 
    write(*, *) prop_n_blades

   !----------------------------------------radius------------------------------------------------
    line = "!"
    do while ((trim(line) .eq. "") .or. (line(1:1) .eq. '!'))
        read(input_prop_unit, '(a)', end = 200) line
        line = trim_front(line)
    end do
    read(line, *) prop_radius_m 
    write(*, *) prop_radius_m

   !----------------------------------------number of stations------------------------------------
    line = "!"
    do while ((trim(line) .eq. "") .or. (line(1:1) .eq. '!'))
        read(input_prop_unit, '(a)', end = 200) line
        line = trim_front(line)
    end do
    read(line, *) prop_n_stations 
    write(*, *) prop_n_stations

    allocate(radius_vec_adim(prop_n_stations))
    allocate(chord_vec_adim(prop_n_stations))
    allocate(twist_vec_deg(prop_n_stations))
    allocate(airfoil_vec(prop_n_stations))

   !----------------------------------------blade description-------------------------------------
    line = "!"
    do i = 1, prop_n_stations
        read(input_prop_unit, '(a)', end = 200) line
        line = trim_front(line)
        read(line, *) radius_vec_adim(i), chord_vec_adim(i), twist_vec_deg(i), airfoil_vec(i) 
    end do
    write(*, *) radius_vec_adim
    write(*, *) chord_vec_adim
    write(*, *) twist_vec_deg
    write(*, *) airfoil_vec

   !----------------------------------------axial velocity----------------------------------------
    line = "!"
    do while ((trim(line) .eq. "") .or. (line(1:1) .eq. '!'))
        read(input_run_unit, '(a)', end = 200) line
        line = trim_front(line)
    end do
    read(line, *) axial_velocity_m_s 
    write(*, *) axial_velocity_m_s

   !----------------------------------------radial velocity---------------------------------------
    line = "!"
    do while ((trim(line) .eq. "") .or. (line(1:1) .eq. '!'))
        read(input_run_unit, '(a)', end = 200) line
        line = trim_front(line)
    end do
    read(line, *) planar_velocity_m_s 
    write(*, *) planar_velocity_m_s

   !----------------------------------------rpm---------------------------------------------------
    line = "!"
    do while ((trim(line) .eq. "") .or. (line(1:1) .eq. '!'))
        read(input_run_unit, '(a)', end = 200) line
        line = trim_front(line)
    end do
    read(line, *) rpm 
    write(*, *) rpm

   !----------------------------------------Air density-------------------------------------------
    line = "!"
    do while ((trim(line) .eq. "") .or. (line(1:1) .eq. '!'))
        read(input_run_unit, '(a)', end = 200) line
        line = trim_front(line)
    end do
    read(line, *) rho_kg_m3
    write(*, *) rho_kg_m3

 !===============================================ITERATE============================================

    do i = 1, prop_n_stations
        do current_azim_deg = 0, 360, 15
            current_tangential_velocity_m_s = (rpm*2*pi/60)*(current_rR*prop_radius_m) + (planar_velocity_m_s*sin(current_azim_deg*pi/180))
            current_equivalent_flow_velocity_m_s = sqrt((current_tangential_velocity_m_s**2) + (axial_velocity_m_s**2))
            current_reynolds = current_equivalent_flow_velocity_m_s*current_cR*prop_radius_m/((1.8/100000)/rho_kg_m3)

            
        end do
    end do
    
    go to 201
    200 continue !file is not set correctly
    201 continue !END

    contains
        subroutine run_cmd(command, log_fil, supress) !subroutine to run the command prompt 
            implicit none 
        
            character (len = *), intent(in)         ::  command
            character (len = *), intent(in)         ::  log_fil
            logical, intent(in)                     ::  supress
            integer                                 ::  err_status
            character (len = 256)                   ::  err_msg
            character (len = 256)                   ::  aux_file = "bin\residual\aux_batfile.bat"
            character (len = 256)                   ::  res_file = "bin\residual\resid.trash"
            integer                                 ::  aux_unit

            call delete_file(aux_file)
            open(newunit = aux_unit, file = aux_file, status = 'new', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
                write(aux_unit, '(a)') trim(command)
            close(aux_unit)

            if (supress) then
                call execute_command_line(trim(aux_file) // " > " // trim(res_file))
            else
                call execute_command_line(trim(aux_file))
            end if

            call delete_file(aux_file)
            ! call delete_file(res_file)

        end subroutine

        subroutine delete_file(fnam) !subroutine to delete a file. If the file does not exist, it does nothing
            implicit none
            character (len = *), intent(in)     ::  fnam
            character (len = 256)               ::  cwd, file
            integer                             ::  stat
            integer                             ::  unit_n

            if (fnam(2:3) .ne. ":\") then
                call getcwd(cwd)
                file = trim(cwd)//"\"//fnam
            else
                file = fnam
            end if

            open(newunit = unit_n, iostat = stat, file = trim(file), status = 'unknown')
            if (stat .eq. 0) then
                close(unit_n, status = 'delete')
            end if
        end subroutine

        subroutine copy_file(in_fname, out_fname, log_fil) !subroutine to copy a file into another file
            character (len = *), intent(in)         ::  in_fname
            character (len = *), intent(in)         ::  out_fname
            character (len = *), intent(in)         ::  log_fil
            character (len = 1000)                  ::  command
            character (len = 256)                   ::  resid = "bin\residual\copy_resid"

            call delete_file(out_fname)
            command = 'copy "' // trim(in_fname) // '" "' // trim(out_fname) // '" > ' // trim(resid)
            call run_cmd(command, log_fil, .true.)
            call delete_file(resid)

        end subroutine copy_file

        subroutine get_files_on_dir(files, dir_name, log_fil) !subroutine to list all the files on a directory
            character (len = 256), allocatable, intent(out) ::  files(:)
            character (len = *), intent(in)                 ::  log_fil
            character (len = *), intent(in)                 ::  dir_name
            integer                                         ::  n_lines, i
            character (len = 256)                           ::  file_w_filenames = "bin\residual\resid.txt"
            integer                                         ::  unit_n
            integer                                         ::  err_status
            character (len = 256)                           ::  err_msg
            character (len = 256)                           ::  cwd, command, directory, dumchar

            call delete_file(file_w_filenames)
            if (dir_name(2:3) .ne. ":\") then
                call getcwd(cwd)
                directory = trim(cwd)//"\"//dir_name
            else
                directory = dir_name
            end if

            command = 'dir "' // trim(directory) // '" /b > ' // file_w_filenames
            call run_cmd(trim(command), log_fil, .true.)

            open(newunit = unit_n, file = file_w_filenames, status = 'old', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if

            n_lines = 0
            do
                read(unit_n, *, end = 200) dumchar
                n_lines = n_lines + 1
            end do
            200 continue
            allocate(files(n_lines))

            rewind unit_n
            do i = 1, n_lines
                read(unit_n, '(a)', end = 201) files(i)
            end do
            201 continue
            close(unit_n)
            call delete_file(file_w_filenames)
        end subroutine

        function upcase(string) result(upper) !function to return the input in upper case
            implicit none
            character (len = *), intent(in) ::  string
            character (len = len(string))   ::  upper
            integer                         ::  j
            
            do j = 1,len(string)
              if((string(j:j) >= "a") .and. (string(j:j) <= "z")) then
                   upper(j:j) = achar(iachar(string(j:j)) - 32)
              else
                   upper(j:j) = string(j:j)
              end if
            end do
        end function upcase

        function lowcase(string) result(lower) !function to return the input in lower case
            implicit none
            character (len = *), intent(in) ::  string
            character (len = len(string))   ::  lower
            integer                         ::  j
            
            do j = 1, len(string)
              if((string(j:j) >= "A") .and. (string(j:j) <= "Z")) then
                   lower(j:j) = achar(iachar(string(j:j)) + 32)
              else
                   lower(j:j) = string(j:j)
              end if
            end do
        end function lowcase

        function file_exists(fnam) result(exists) !function to return if the file exists
            implicit none
            character (len = *), intent(in) ::  fnam
            logical                         ::  exists
            character (len = 256)           ::  cwd, file
            integer                         ::  stat
            integer                         ::  unit_n

            if (fnam(2:3) .ne. ":\") then
                call getcwd(cwd)
                file = trim(cwd)//"\"//fnam
            else
                file = fnam
            end if

            exists = .true.
            open(newunit = unit_n, iostat = stat, file = trim(file), status = 'old')
            if (stat .ne. 0) then
                exists = .false.
            end if

        end function file_exists

        function count_ocurrences_line(line, word) result(n_ocurrences) !funcion to return the number of ocurrences of a string on a bigger string
            implicit none
            character (len = *), intent(in)     ::  line
            character (len = *), intent(in)     ::  word
            integer                             ::  n_ocurrences
            integer                             ::  i
            
            n_ocurrences = 0
            do i = 1, len_trim(line) - len_trim(word) + 1
                if (line(i:i + len_trim(word) - 1) .eq. trim(word)) n_ocurrences = n_ocurrences + 1
            end do

        end function count_ocurrences_line

        function trim_front(line) result(trimmed) !function to return the line without white spaces in front
            implicit none
            character (len = *), intent(in) ::  line
            character (len = len(line))     ::  trimmed

            trimmed = line

            if (trim(trimmed) .eq. "") then
                continue
            else
                do while ((trimmed(1:1) .eq. " ") .or. (trimmed(1:1) .eq. achar(9))) !Eliminate initial blank spaces and tabs
                    trimmed = trimmed(2:len(trimmed))
                end do
            end if

        end function trim_front

        subroutine clean_folder(folder, log_fil)
            implicit none
            character (len = *), intent(in)     ::  folder
            character (len = *), intent(in)     ::  log_fil
            character (len = 256), allocatable  ::  files(:)
            integer                             ::  i

            call get_files_on_dir(files, folder, log_fil)

            do i = 1, size(files)
                call delete_file(trim(folder) // "\" // files(i))
            end do
        end subroutine clean_folder

end program BEMT_Pedrinho