program test
    implicit none
    
    
    contains
        subroutine run_cmd(command, log_fil, supress) !subroutine to run the command prompt 
            implicit none 
        
            character (len = *), intent(in)         ::  command
            character (len = *), intent(in)         ::  log_fil
            logical, intent(in)                     ::  supress
            integer                                 ::  err_status
            character (len = 256)                   ::  err_msg
            character (len = 256)                   ::  aux_file = "aux_batfile.bat"
            character (len = 256)                   ::  res_file = "resid.trash"
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

            ! call delete_file(aux_file)
            ! call delete_file(res_file)

        end subroutine

        subroutine run_xfoil(results)
            implicit none
            character (len = 256)           ::  xfoil_run_definition
            character (len = 256)           ::  xfoil_run_file = "def_xfoil.inp"
            character (len = 256)           ::  system_comand = "xfoil.exe < " // trim(xfoil_run_file)
            character (len = 256)           ::  out_fil = "xfoil_output.txt"
            integer                         ::  err_status
            character (len = 256)           ::  err_iomsg
            character (len = 256)           ::  line
            integer                         ::  out_unit
            integer                         ::  i, j, k
            integer                         ::  n_outputs
            real, allocatable, intent(out)  ::  results(:,:)

            xfoil_run_definition = &
            "plop"              // achar(13) // achar(10) // &
            "g f"               // achar(13) // achar(10) // &
            ""                  // achar(13) // achar(10) // &
            "load clarky.txt"   // achar(13) // achar(10) // &
            ""                  // achar(13) // achar(10) // &
            "oper"              // achar(13) // achar(10) // &
            "iter 100"          // achar(13) // achar(10) // &
            "visc 200000"       // achar(13) // achar(10) // &
            "M 0.01"            // achar(13) // achar(10) // &
            "pacc"              // achar(13) // achar(10) // &
            trim(out_fil)       // achar(13) // achar(10) // &
            ""                  // achar(13) // achar(10) // &
            "aseq 0 10 1"       // achar(13) // achar(10) // &
            ""                  // achar(13) // achar(10) // &
            ""                  // achar(13) // achar(10) // &
            ""                  // achar(13) // achar(10) // &
            "quit"              // achar(13) // achar(10) // &

            system_comand = "xfoil.exe < def_xfoil.inp"
            out_fil = "xfoil_output.txt"

            call delete_file(out_fil)
            call system(system_comand)

            open(unit = out_unit, file = out_fil, status = 'old', iostat = err_status, iomsg = err_iomsg)
            call check_error(err_status, err_iomsg)

            i = 1
            n_outputs = 0
            do 
                read(out_unit, '(a)', end = 200) 
                if (i .gt. 12) then 
                    n_outputs = n_outputs + 1
                end if
                i = i + 1
            end do
            200 continue

            allocate(results(7, n_outputs))
            rewind out_unit

            i = 1
            j = 1
            do 
                read(out_unit, '(a)', end = 201) line
                if (i .gt. 12) then 
                    read(line, *) (results(k, j), k = 1, 7)
                    j = j + 1
                end if
                i = i + 1
            end do
            201 continue

            close(out_unit)

            call delete_file(out_fil)

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
    
end program test