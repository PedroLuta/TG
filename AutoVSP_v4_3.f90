program autovsp
    implicit none

!===================================================================================================================================================================
! automatização de simulações e outputs do openvsp
! automatization and output handling for openvsp
! preparado por/made by Pedro Luta - 13/07/2022 (dd/mm/yyyy)
!
!===================================================================================================================================================================
! v0 - single rotor with outputs of coefficients by time and loads
! v1 - implementation of cross-sections, failchecks and console log
!   v1.1 - implementation of hover_flag 
! v2 - support for symmetry and multiple rpms
! v3 - support for existing .vsp3 files
!    - quality of life upgrades
! v4 - change of functionalities with focus on modularization and support for all geometry files
!===================================================================================================================================================================

!===========================================variables========================================================================================================================
    !constants
    ! real, parameter                     ::  pi = 4*atan(1.0)    

    !operating system
    character (len = 32)                ::  operating_system    !character to store in what operating system the code is being run

    !date, time and disclaimer
    character (len = 8)                 ::  current_date
    character (len = 8)                 ::  current_time
    character (len = 10)                ::  current_time_char
    character (len = 5)                 ::  current_timezone
    integer                             ::  aux_integers(8)
    character (len = 256)               ::  disclaimer
    
    !files
    integer                             ::  inp_unit, log_unit              ! unit integers to handle files
    integer                             ::  unified_over_time_unit, unified_stats_unit, unified_loads_unit                   ! unit integers to handle files
    character (len = 256)               ::  inp_fil, tplt_fil, scpt_fil, log_fil, template_file2, copy_fil          ! 
    character (len = 256)               ::  main1_fil, main2_fil                ! 
    character (len = 256)               ::  log_fil_name, scpt_fil_name               ! 
    character (len = 256)               ::  unified_over_time_file_name, unified_stats_file_name, unified_loads_file_name                ! 
    character (len = 256)               ::  unified_over_time_file, unified_stats_file, unified_loads_file                ! 
    character (len = 256)               ::  temp_fils(2)                                    ! temporary files to store template script alterations 
    character (len = 256)               ::  temp_fils2(2)                                    ! temporary files to store template script alterations 
    character (len = 256), allocatable  ::  cwd_files(:)                                    ! names of files in current working directory
    character (len = 256)               ::  cwd                                             ! name of current working directory
    character (len = 256)               ::  folder_name, results_folder, bin_residual_folder

    !error handling
    integer                             ::  err_status                                      ! error status 
    character (len = 256)               ::  err_msg                                         ! error message 
    
    !main variables
    character (len = 256)               ::  lin_title, description
    character (len = 10000)             ::  line                                      
    character (len = 10000)             ::  change                                          ! string to change on template script
    character (len = 10000)             ::  rpm_code, sections_code                                              
    character (len = 10000)             ::  post_processing_files                                               
    character (len = 100)               ::  post_processing_nlastrev_choices                                               
    character (len = 10000)             ::  analysis_names                                               
    character (len = 256), allocatable  ::  params_char_vec(:), params_char_vec2(:)                              ! vector to store the parameters on a line
    character (len = 256)               ::  system_command                                  ! string to store the system commands
    character (len = 256)               ::  x_radius, x_form, prev_x_radius
    character (len = 256)               ::  af_fil, prop_name, file_name, wing_name, analysis_name
    character (len = 256)               ::  prop_rpm
    character (len = 256)               ::  prop_identifier
    integer                             ::  n_params
    integer                             ::  n_xsec, props_defined, files_defined, wings_defined, n_wing_sections, analysis_defined
    integer, allocatable                ::  integer_auxiliar_vector(:)
    logical                             ::  only_post_process, description_defined, nlast_revs_defined, changed_flag
    
    !auxiliar variables
    integer                             ::  i, j, k, l                                    ! iterators 
    real                                ::  dumreal1, dumreal2, dumreal3                    ! dummy floats 
    character (len = 256)               ::  dumchar1, dumchar2, dumchar3                    ! dummy strings


!==========================================initial procedures=======================================================
   !----------files declaration----------
    inp_fil = 'AutoVSP.inp'                  ! input file
    results_folder = "results"    
    bin_residual_folder = "bin\residual"
    scpt_fil_name = "main.vspscript"            ! final script file
    scpt_fil = trim(bin_residual_folder) // "\" // trim(scpt_fil_name)            ! final script file
    log_fil_name = "console_log.txt"
    log_fil = trim(bin_residual_folder) // "\" // trim(log_fil_name)               ! file to store the console log and errors
    temp_fils = (/trim(bin_residual_folder) // '\tempfile1', trim(bin_residual_folder) // '\tempfile2'/)    ! temporary files to store changed script files
    temp_fils2 = (/trim(bin_residual_folder) // '\tempfile3', trim(bin_residual_folder) // '\tempfile4'/)    ! temporary files to store changed script files
    main1_fil = trim(bin_residual_folder) // "\main1.vspscript"
    main2_fil = trim(bin_residual_folder) // "\main2.vspscript"
    unified_over_time_file_name = "unified_over_time_results.txt"
    unified_stats_file_name = "unified_stats_results.txt"
    unified_loads_file_name = "unified_loads_results.txt"
    unified_over_time_file = trim(bin_residual_folder) // "\" // trim(unified_over_time_file_name)
    unified_stats_file = trim(bin_residual_folder) // "\" // trim(unified_stats_file_name)
    unified_loads_file = trim(bin_residual_folder) // "\" // trim(unified_loads_file_name)
    
    operating_system = "windows"


!===========================================start============================================================================================================
    !open the input file
    open(newunit = inp_unit, file = inp_fil, status = 'old', iostat = err_status, iomsg = err_msg)
    if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
    
    line = '$'
    do 
!===========================================change inputs on template file============================================================================================================
        description = "default_description_name"
        post_processing_files = ""
        post_processing_nlastrev_choices = ""
        analysis_names = ""
        description_defined = .false.
        props_defined = 0
        wings_defined = 0
        files_defined = 0
        analysis_defined = 0
        only_post_process = .false.

        !search for the next line that is not commented on the input file
        do while ((trim(line) .eq. "") .or. (line(1:1) .eq. '$'))
            read(inp_unit, '(a)', end = 200) line
            line = trim_front(line)
        end do

        !----------date and time-------------
        call date_and_time(current_date, current_time_char, current_timezone, aux_integers)
        write(current_time, "(i2,a1,i2,a1,i2)") aux_integers(5), ":", aux_integers(6), ":", aux_integers(7)
        disclaimer = "=========================================This is a product of AutoVSP v4.3=========================================" // achar(13) // achar(10) // "Date of execution: " // current_date // achar(13) // achar(10) // "Time of execution: " // current_time // achar(13) // achar(10) // "Timezone: " // current_timezone // achar(10) // achar(13)

        !clear the folder that is going to be used for the purpose of file manipulation
        call clean_folder(bin_residual_folder, log_fil, operating_system)

        !display disclaimer
        call throw_warning(disclaimer, log_fil)

        !copy the main template files to start a new script
        call copy_file("bin\template_main1.vspscript", main1_fil, log_fil, operating_system)       
        call copy_file("bin\template_main2.vspscript", main2_fil, log_fil, operating_system) 

        
    !------------------------script editing------------------------------------
        do
            !for each card displayed here, a function is manipulated based on the corresponding template file
         !------------------------one-line cards------------------------------
            if (lowcase(line(1:11)) .eq. "description") then
                if (description_defined) then ! already defined a description
                    err_msg = "---> ERROR -> Multiple descripions defined, please define only one, skipping to next script"
                    call throw_warning(err_msg, log_fil)
                    go to 889
                end if
                read(line, *) dumchar1, description
                tplt_fil = "bin\template_descript.vspscript"
                description_defined = .true.

                call copy_file(tplt_fil, temp_fils(1), log_fil, operating_system)
                j = 1
                lin_title = "name"
                call write_to_script(lin_title, trim(description), temp_fils, j, log_fil, changed_flag)
                if (.not. changed_flag) then
                    err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                    call throw_warning(err_msg, log_fil)
                    go to 889
                end if

                call copy_file(temp_fils( 1 + mod(j + 1, 2)), trim(bin_residual_folder) // "\ready_descript.vspscript", log_fil, operating_system) ! copy final temporary file to script file
            
            else if (lowcase(line(1:4)) .eq. "file") then
                read(line, *) dumchar1, file_name
                files_defined = files_defined + 1
                tplt_fil = "bin\template_file.vspscript"

                call copy_file(tplt_fil, temp_fils(1), log_fil, operating_system)
                j = 1
                write(dumchar2, "(i1)") files_defined
                if (props_defined .ge. 10) write(dumchar2, "(i2)") files_defined
                lin_title = "iter"
                call write_to_script(lin_title, trim(dumchar2), temp_fils, j, log_fil, changed_flag)
                if (.not. changed_flag) then
                    err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                    call throw_warning(err_msg, log_fil)
                    go to 889
                end if
                lin_title = "name"
                call write_to_script(lin_title, trim(file_name), temp_fils, j, log_fil, changed_flag)
                if (.not. changed_flag) then
                    err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                    call throw_warning(err_msg, log_fil)
                    go to 889
                end if

                call copy_file(temp_fils( 1 + mod(j + 1, 2)), trim(bin_residual_folder) // "\ready_file" // trim(dumchar2) // ".vspscript", log_fil, operating_system) ! copy final temporary file to script file
            
            else if (lowcase(line(1:13)) .eq. "generate_geom") then
                
                tplt_fil = "bin\template_gen_geom.vspscript"
                call copy_file(tplt_fil, trim(bin_residual_folder) // "\ready_gen_geom.vspscript", log_fil, operating_system) ! copy final temporary file to script file
            
            else if (lowcase(line(1:4)) .eq. "post") then
                call count_parameters_line(line, " " // achar(9), n_params, .true.)
                if (n_params .eq. 2) then
                    read(line, *) dumchar1, dumchar2
                    post_processing_nlastrev_choices = trim(post_processing_nlastrev_choices)// "1,"
                else if (n_params .eq. 3) then
                    read(line, *) dumchar1, dumchar2, dumchar3
                    post_processing_nlastrev_choices = trim(post_processing_nlastrev_choices) // trim(dumchar3) // ","
                end if
                post_processing_files = trim(post_processing_files) // trim(dumchar2) // ","
                analysis_names = trim(analysis_names) // trim(dumchar2) // ","
            else if (lowcase(line(1:9)) .eq. "only_post") then
                only_post_process = .true.
         !------------------------multiple-line cards------------------------------
            else if (lowcase(line(1:4)) .eq. "prop") then

                read(line, *) dumchar1, prop_name
                props_defined = props_defined + 1
                tplt_fil = "bin\template_prop.vspscript"

                call copy_file(tplt_fil, temp_fils(1), log_fil, operating_system)
                j = 1
                write(dumchar2, "(i1)") props_defined
                if (props_defined .ge. 10) write(dumchar2, "(i2)") props_defined
                lin_title = "iter"
                call write_to_script(lin_title, trim(dumchar2), temp_fils, j, log_fil, changed_flag)
                if (.not. changed_flag) then
                    err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                    call throw_warning(err_msg, log_fil)
                    go to 889
                end if
                lin_title = "name"
                call write_to_script(lin_title, trim(prop_name), temp_fils, j, log_fil, changed_flag)
                if (.not. changed_flag) then
                    err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                    call throw_warning(err_msg, log_fil)
                    go to 889
                end if


                read(inp_unit, '(a)', end = 200) line
                line = trim_front(line) 

                do while (lowcase(line(1:8)) .ne. "end_prop")

                    call count_parameters_line(line, " " // achar(9), n_params, .true.) ! count parameters separated by " " with no repetition, for example "var val" is the same number of parameters as "var      val"
                    allocate(params_char_vec(n_params - 1))
                    read(line, *) lin_title, (params_char_vec(i), i = 1, n_params - 1)  ! separate the inputs into various strings

                    if (lowcase(trim(lin_title)) .eq. "nxsec") then
                        call write_to_script(lin_title, params_char_vec(1), temp_fils, j, log_fil, changed_flag) ! write the number of cross sections
                        if (.not. changed_flag) then
                            err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                            call throw_warning(err_msg, log_fil)
                            go to 889
                        end if
                        read(params_char_vec(1), *) n_xsec
                        change = ""
                        prev_x_radius = "0"
                        do i = 0, n_xsec - 1 
                            read(inp_unit, '(a)', end = 200) line
                            line = trim_front(line) 

                            call count_parameters_line(line, " " // achar(9), n_params, .true.)
                            if (n_params < 3) then
                                err_msg = "---> ERROR -> not enough arguments for the cross-section were determined, each cross-section needs the title 'xsec', a radius position (r/r) and a form value (0-18), skipping to next script"
                                call throw_warning(err_msg, log_fil)
                                ! stop
                                go to 889
                            end if 

                            read(line, *) lin_title, x_radius, x_form
                            if (lin_title .ne. "xsec") then
                                err_msg = "---> ERROR -> not enough cross-sections were determined, check the number of inputs with title 'xsec', skipping to next script"
                                call throw_warning(err_msg, log_fil)
                                ! stop
                                go to 889
                            end if

                            if (trim(x_form) .eq. "12") then
                                read(line, *) lin_title, x_radius, x_form, af_fil ! if x_form is airfoil file form, read the airfoil file
                                if (.not. file_exists("airfoil\" // trim(af_fil))) then   
                                    err_msg = "---> ERROR -> airfoil " // trim(af_fil) // " was not found, check if the inputted airfoil can be found on airfoils folder, skipping to next script" 
                                    call throw_warning(err_msg, log_fil)
                                    ! stop
                                    go to 889
                                end if
                            end if

                            read(x_radius, *) dumreal1
                            read(prev_x_radius, *) dumreal2
                            if (dumreal2 .gt. dumreal1) then
                                err_msg = "---> ERROR -> please define the cross-sections in order, starting from the hub, skipping to next script"
                                call throw_warning(err_msg, log_fil)
                                ! stop
                                go to 889
                            end if

                            if ((i .eq. (n_xsec - 1)) .and. (dumreal1 - 1.0 .gt. 0.001)) then
                                err_msg = "---> WARNING -> last cross section radial position (r/R) replaced with 1.0"
                                call throw_warning(err_msg, log_fil)
                            end if

                            write(dumchar1, "(i2)") i
                            if (trim(x_form) .eq. "12") then
                                change = trim(change) // "ChangeXSecShape(xsurf_id, "// trim(dumchar1) // ", " // trim(x_form) // "); SetParmVal(GetXSecParm( GetXSec(xsurf_id, "// trim(dumchar1) // "), 'RadiusFrac' ), " // trim(x_radius) // "); ReadFileAirfoil( GetXSec(xsurf_id, "// trim(dumchar1) // "), 'airfoils\\" // trim(af_fil) // "' );" // achar(13) // achar(10) // achar(9)
                            else
                                change = trim(change) // "ChangeXSecShape(xsurf_id, "// trim(dumchar1) // ", " // trim(x_form) // "); SetParmVal(GetXSecParm( GetXSec(xsurf_id, "// trim(dumchar1) // "), 'RadiusFrac' ), " // trim(x_radius) // ");" // achar(13) // achar(10) // achar(9)
                            end if
                            prev_x_radius = x_radius
                        end do
                        lin_title = "xsec_code"
                        call write_to_script(lin_title, change, temp_fils, j, log_fil, changed_flag)
                        if (.not. changed_flag) then
                            err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                            call throw_warning(err_msg, log_fil)
                            go to 889
                        end if
                    else if (lowcase(trim(line)) .eq. "end") then
                        deallocate(params_char_vec)
                        go to 201
                    else if ((trim(line) .eq. "") .or. (line(1:1) .eq. "$")) then ! comment or blank line
                        continue
                    else
                        i = 1
                        if (n_params > 2) then !input is a vector, add commas
                            change = trim(params_char_vec(1))
                            i = i + 1
                            do while (i < n_params)
                                change = trim(change) // "," // trim(params_char_vec(i))
                                i = i + 1
                            end do
                        else
                            change = trim(params_char_vec(i))
                        end if
                        call write_to_script(lin_title, change, temp_fils, j, log_fil, changed_flag)
                        if (.not. changed_flag) then
                            err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                            call throw_warning(err_msg, log_fil)
                            go to 889
                        end if
                    end if

                    deallocate(params_char_vec)
                    read(inp_unit, '(a)', end = 200) line
                    line = trim_front(line) 
                end do

                call copy_file(temp_fils( 1 + mod(j + 1, 2)), trim(bin_residual_folder) // "\ready_prop" // trim(dumchar2) // ".vspscript", log_fil, operating_system) ! copy final temporary file to script file

            else if (lowcase(line(1:8)) .eq. "analysis") then
                rpm_code = ""
                read(line, *) dumchar1, analysis_name
                analysis_names = trim(analysis_names) // trim(analysis_name) // ","
                analysis_defined = analysis_defined + 1
                nlast_revs_defined = .false.
                tplt_fil = "bin\template_analysis.vspscript"
                post_processing_files = trim(post_processing_files) // trim(results_folder) // "\" // trim(description) // "\" // trim(analysis_name) // "\" // trim(description) // "_raw.csv,"

                call copy_file(tplt_fil, temp_fils(1), log_fil, operating_system)
                j = 1
                write(dumchar2, "(i1)") analysis_defined
                if (analysis_defined .ge. 10) write(dumchar2, "(i2)") analysis_defined
                lin_title = "iter"
                call write_to_script(lin_title, trim(dumchar2), temp_fils, j, log_fil, changed_flag)
                if (.not. changed_flag) then
                    err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                    call throw_warning(err_msg, log_fil)
                    go to 889
                end if
                lin_title = "name"
                call write_to_script(lin_title, trim(analysis_name), temp_fils, j, log_fil, changed_flag)
                if (.not. changed_flag) then
                    err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                    call throw_warning(err_msg, log_fil)
                    go to 889
                end if


                read(inp_unit, '(a)', end = 200) line 
                line = trim_front(line) 

                do
                    call count_parameters_line(line, " " // achar(9), n_params, .true.) ! count parameters separated by " " with no repetition, for example "var val" is the same number of parameters as "var      val"
                    allocate(params_char_vec(n_params - 1))
                    read(line, *) lin_title, (params_char_vec(i), i = 1, n_params - 1)  ! separate the inputs into various strings

                    if (lowcase(trim(lin_title)) .eq. "rpm") then
                        if (n_params .eq. 3) then
                            prop_identifier = params_char_vec(1)
                            prop_rpm = params_char_vec(2)
                            rpm_code = trim(rpm_code) // "else if (GetGeomName(geom_ids[j]) == '" // trim(prop_identifier) // "') {SetParmVal( FindParm( FindUnsteadyGroup(i), 'RPM', 'UnsteadyGroup'), " // trim(prop_rpm) // ");}" // achar(13) // achar(10) // achar(9) // achar(9) // achar(9) // achar(9)
                        else
                            err_msg = "---> ERROR -> not enough arguments for the rpm were determined, please determine the name of the rotor and the value of rpm, skipping to next script"
                            call throw_warning(err_msg, log_fil)
                            ! stop
                            go to 889
                        end if
                    else if (lowcase(trim(lin_title)) .eq. "last_revs_post") then
                        post_processing_nlastrev_choices = trim(post_processing_nlastrev_choices) // trim(params_char_vec(1)) // ","
                        nlast_revs_defined = .true.
                        ! post_process = .true.
                    else if (lowcase(trim(line)) .eq. "end") then
                        deallocate(params_char_vec)
                        go to 201
                    else if ((trim(line) .eq. "") .or. (line(1:1) .eq. "$")) then ! comment or blank line
                        continue
                    else if (lowcase(line(1:12)) .eq. "end_analysis") then
                        deallocate(params_char_vec)
                        go to 297
                    else
                        i = 1
                        if (n_params > 2) then !input is a vector, add commas
                            change = trim(params_char_vec(1))
                            i = i + 1
                            do while (i < n_params)
                                change = trim(change) // "," // trim(params_char_vec(i))
                                i = i + 1
                            end do
                        else
                            change = trim(params_char_vec(i))
                        end if
                        call write_to_script(lin_title, change, temp_fils, j, log_fil, changed_flag)
                        if (.not. changed_flag) then
                            err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                            call throw_warning(err_msg, log_fil)
                            go to 889
                        end if
                    end if 

                    deallocate(params_char_vec)
                    read(inp_unit, '(a)', end = 200) line 
                    line = trim_front(line) 
                end do
                297 continue
                
                if (.not. nlast_revs_defined) then
                    post_processing_nlastrev_choices = trim(post_processing_nlastrev_choices) // "1,"
                end if

                lin_title = "rpm_code"
                call write_to_script(lin_title, trim(rpm_code), temp_fils, j, log_fil, changed_flag)
                if (.not. changed_flag) then
                    err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                    call throw_warning(err_msg, log_fil)
                    go to 889
                end if
                call copy_file(temp_fils( 1 + mod(j + 1, 2)), trim(bin_residual_folder) // "\ready_analysis" // trim(dumchar2) // ".vspscript", log_fil, operating_system) ! copy final temporary file to script file

            
            else if (lowcase(line(1:4)) .eq. "wing") then
                read(line, *) dumchar1, wing_name
                wings_defined = wings_defined + 1
                tplt_fil = "bin\template_wing.vspscript"
                sections_code = ""

                call copy_file(tplt_fil, temp_fils(1), log_fil, operating_system)
                j = 1
                write(dumchar2, "(i1)") wings_defined
                if (wings_defined .ge. 10) write(dumchar2, "(i2)") wings_defined
                lin_title = "iter"
                call write_to_script(lin_title, trim(dumchar2), temp_fils, j, log_fil, changed_flag)
                if (.not. changed_flag) then
                    err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                    call throw_warning(err_msg, log_fil)
                    go to 889
                end if
                lin_title = "name"
                call write_to_script(lin_title, trim(wing_name), temp_fils, j, log_fil, changed_flag)
                if (.not. changed_flag) then
                    err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                    call throw_warning(err_msg, log_fil)
                    go to 889
                end if


                read(inp_unit, '(a)', end = 200) line 
                line = trim_front(line) 

                333 continue
                do while (lowcase(line(1:8)) .ne. "end_wing")

                    call count_parameters_line(line, " " // achar(9), n_params, .true.) ! count parameters separated by " " with no repetition, for example "var val" is the same number of parameters as "var      val"
                    allocate(params_char_vec(n_params - 1))
                    read(line, *) lin_title, (params_char_vec(i), i = 1, n_params - 1)  ! separate the inputs into various strings
                    
                    if (lowcase(trim(lin_title)) .eq. "nsections") then

                        call write_to_script(lin_title, params_char_vec(1), temp_fils, j, log_fil, changed_flag) ! write the number of cross sections
                        if (.not. changed_flag) then
                            err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                            call throw_warning(err_msg, log_fil)
                            go to 889
                        end if
                        read(params_char_vec(1), *) n_wing_sections
                        change = ""
                        template_file2 = "bin\template_wing_section.vspscript"

                        read(inp_unit, '(a)', end = 200) line 
                        line = trim_front(line)

                        do i = 1, n_wing_sections
                            call copy_file(template_file2, temp_fils2(1), log_fil, operating_system)
                            l = 1
                            write(dumchar2, "(i1)") wings_defined
                            write(dumchar3, "(i1)") i
                            if (wings_defined .ge. 10) write(dumchar2, "(i2)") wings_defined
                            if (i .ge. 10) write(dumchar3, "(i2)") i
                            lin_title = "iter1"
                            call write_to_script(lin_title, trim(dumchar2), temp_fils2, l, log_fil, changed_flag)
                            if (.not. changed_flag) then
                                err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                                call throw_warning(err_msg, log_fil)
                                go to 889
                            end if
                            lin_title = "iter2"
                            call write_to_script(lin_title, trim(dumchar3), temp_fils2, l, log_fil, changed_flag)
                            if (.not. changed_flag) then
                                err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                                call throw_warning(err_msg, log_fil)
                                go to 889
                            end if

                            if (trim(line) .ne. "section") then
                                err_msg = "---> ERROR -> please define your sections after you have defined the number of sections, skipping to next script"
                                call throw_warning(err_msg, log_fil)
                                ! stop
                                go to 889
                            end if

                            read(inp_unit, '(a)', end = 200) line 
                            line = trim_front(line)

                            do while (lowcase(line(1:11)) .ne. "end_section")

                                read(line, *) lin_title, change

                                if ((lowcase(trim(lin_title)) .eq. "root_airfoil") .or. (lowcase(trim(lin_title)) .eq. "tip_airfoil")) then
                                    if (.not. file_exists("airfoils\" // trim(change))) then
                                        err_msg = "---> ERROR -> airfoil " // trim(change) // " not found on airfoils folder, skipping to next script"
                                        call throw_warning(err_msg, log_fil)
                                        ! stop
                                        go to 889
                                    end if
                                end if

                                call write_to_script(trim(lin_title), trim(change), temp_fils2, l, log_fil, changed_flag)
                                if (.not. changed_flag) then
                                    err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                                    call throw_warning(err_msg, log_fil)
                                    go to 889
                                end if
                                read(inp_unit, '(a)', end = 200) line 
                                line = trim_front(line)
                            end do
                            sections_code = trim(sections_code) // "check = wing" // trim(dumchar2) // "_section" // trim(dumchar3) //"(wing_id); if (check != 0) {return 1;}" // achar(13) // achar(10) // achar(9)
                            call dump_function2(temp_fils( 1 + mod(j + 1, 2)), temp_fils2( 1 + mod(l + 1, 2)), log_fil, operating_system)
                            
                            read(inp_unit, '(a)', end = 200) line 
                            line = trim_front(line)
                        end do

                        lin_title = "sections_code"
                        change = trim(sections_code)
                        call write_to_script(lin_title, change, temp_fils, j, log_fil, changed_flag)
                        if (.not. changed_flag) then
                            err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                            call throw_warning(err_msg, log_fil)
                            go to 889
                        end if
                        call delete_file(temp_fils2(1))
                        call delete_file(temp_fils2(2))
                        deallocate(params_char_vec)
                        go to 333
                    else if (lowcase(trim(line)) .eq. "end") then
                        go to 201
                    else if ((trim(line) .eq. "") .or. (line(1:1) .eq. "$")) then ! comment or blank line
                        continue
                    else
                        change = params_char_vec(1)
                        call write_to_script(lin_title, change, temp_fils, j, log_fil, changed_flag)
                        if (.not. changed_flag) then
                            err_msg = "---> ERROR -> parameter " // lowcase(trim(lin_title)) // " has not been found on template file, please check the appendix 1 of the documentation to see the correct writing of the parameter, skipping to next script"
                            call throw_warning(err_msg, log_fil)
                            go to 889
                        end if
                    end if    

                    deallocate(params_char_vec)
                    read(inp_unit, '(a)', end = 200) line 
                    line = trim_front(line) 
                end do

                call copy_file(temp_fils( 1 + mod(j + 1, 2)), trim(bin_residual_folder) // "\ready_wing" // trim(dumchar2) // ".vspscript", log_fil, operating_system) ! copy final temporary file to script file
         !------------------------end, comments and exceptions-----------------------------------
            else if (lowcase(trim(line)) .eq. "end") then
                go to 201
            else if ((trim(line) .eq. "") .or. (line(1:1) .eq. "$")) then       ! comment or blank line
                continue
            else
                write(*,*) "---> ERROR -> input not recognized '" // trim(line) // "', skipping to next script"
                ! stop
                go to 889
            end if

            read(inp_unit, '(a)', end = 200) line 
            line = trim_front(line) !retirar caracteres vazios do começo da linha

        end do
        201 continue

        if (only_post_process) then
            go to 999
        else if (.not. description_defined) then
            err_msg = "Description not defined, skipping to next script"
            call throw_warning(err_msg, log_fil)
            go to 889
        end if

!===========================================write all the script procedures in order (import files -> wings -> props -> description -> generate geometry -> analysis)============================================================================================================! 
        call get_files_on_dir(cwd_files, trim(bin_residual_folder), log_fil, operating_system)
        do i = 1, size(cwd_files)
            if (cwd_files(i)(1:10) .eq. "ready_file") then 
                call dump_function(main1_fil, main2_fil, trim(bin_residual_folder) // "\" // cwd_files(i), log_fil, operating_system)
            end if
        end do
        do i = 1, size(cwd_files)
            if (cwd_files(i)(1:10) .eq. "ready_wing") then 
                call dump_function(main1_fil, main2_fil, trim(bin_residual_folder) // "\" // cwd_files(i), log_fil, operating_system)
            end if
        end do
        do i = 1, size(cwd_files)
            if (cwd_files(i)(1:10) .eq. "ready_prop") then 
                call dump_function(main1_fil, main2_fil, trim(bin_residual_folder) // "\" // cwd_files(i), log_fil, operating_system)
            end if
        end do
        do i = 1, size(cwd_files)
            if (cwd_files(i)(1:14) .eq. "ready_descript") then 
                call dump_function(main1_fil, main2_fil, trim(bin_residual_folder) // "\" // cwd_files(i), log_fil, operating_system)
            end if
        end do
        do i = 1, size(cwd_files)
            if (cwd_files(i)(1:14) .eq. "ready_gen_geom") then 
                call dump_function(main1_fil, main2_fil, trim(bin_residual_folder) // "\" // cwd_files(i), log_fil, operating_system)
            end if
        end do
        do i = 1, size(cwd_files)
            if (cwd_files(i)(1:14) .eq. "ready_analysis") then 
                call dump_function(main1_fil, main2_fil, trim(bin_residual_folder) // "\" // cwd_files(i), log_fil, operating_system)
            end if
        end do
        
        !join the 2 main files into the final main file
        call join_files(main1_fil, main2_fil, scpt_fil, log_fil)

!===========================================create folders============================================================================================================
        !create the folders to be used in storing of results
        !this is important to happen before the script is run because the script assumes these folders already exist
        
        call count_parameters_line(analysis_names, ",", n_params, .true.) ! 
        allocate(params_char_vec(n_params - 1))
        read(analysis_names, *) (params_char_vec(i), i = 1, n_params - 1)  ! separate the inputs into various strings
        
        if (trim(operating_system) .eq. "windows") then
            system_command = "mkdir " // trim(results_folder) ! general results folder
            call run_cmd(system_command, log_fil, .true.)
            system_command = "mkdir " // trim(results_folder) // "\" // trim(description) ! folder for the script run
            call run_cmd(system_command, log_fil, .true.)
            do i = 1, n_params - 1
                system_command = "mkdir " // trim(results_folder) // "\" // trim(description) // "\" // trim(params_char_vec(i)) ! folder for each analysis
                call run_cmd(system_command, log_fil, .true.)
            end do
        else if (trim(operating_system) .eq. "linux") then
            system_command = "mkdir " // trim(results_folder) ! make a folder to store results
            call run_cmd(system_command, log_fil, .true.)
            system_command = "mkdir " // trim(results_folder) // "/" // trim(description)
            call run_cmd(system_command, log_fil, .true.)
            do i = 1, n_params - 1
                system_command = "mkdir " // trim(results_folder) // "/" // trim(description) // "/" // trim(params_char_vec(i))
                call run_cmd(system_command, log_fil, .true.)
            end do
        end if

        deallocate(params_char_vec)


!===========================================run analysis============================================================================================================

        write(*,'(A)') "---> PROCEED -> running script " // trim(description)
        

        ! 'powershell ".\vspscript.exe -script ' // trim(scpt_fil) // ' | tee ' // trim(log_fil) // '"'
        if (trim(operating_system) .eq. "windows") then
            ! system_command = "vspscript.exe -script " // trim(scpt_fil) // " >> " // trim(log_fil)
            system_command = 'powershell ".\vspscript.exe -script ' // trim(scpt_fil) // ' | tee ' // trim(log_fil) // '"'
        else if (trim(operating_system) .eq. "linux") then
            ! system_command = "vspscript.exe -script " // trim(scpt_fil) // " >> " // trim(log_fil)
            system_command = 'powershell ".\vspscript.exe -script ' // trim(scpt_fil) // ' | tee ' // trim(log_fil) // '"'
        end if

        call run_cmd(system_command, log_fil, .false.) !this is the script call

        ! check if OpenVSP returned any errors
        open(newunit = log_unit, file = log_fil, status = 'old', iostat = err_status, iomsg = err_msg)
        if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
        line = "$"
        read(log_unit, '(a)', end = 250) line
        line = trim_front(line)
        do
            if (trim(line) .eq. "stop_code_now") then
                close(log_unit)
                err_msg = "---> TERMINATE -> OpenVSP returned errors for script" // trim(description) // ", terminating other processes"
                call throw_warning(err_msg, log_fil)
                go to 1001
            end if

            read(log_unit, '(a)', end = 250) line
            line = trim_front(line)
        end do

        250 continue
        close(log_unit)
        write(*, '(a)') "---> PROCEED -> OpenVSP script run successfully, continuing other process"

!===========================================interpret results=========================================================================================================
        999 continue

        !get all the files, name of analysis and last revolution user choices for the post-processing
        call count_parameters_line(post_processing_files, ",", n_params, .true.) ! 
        if (n_params - 1 .eq. 0) go to 1001
        allocate(params_char_vec(n_params - 1))
        allocate(params_char_vec2(n_params - 1))
        allocate(integer_auxiliar_vector(n_params - 1))
        read(post_processing_files, *) (params_char_vec(i), i = 1, n_params - 1)  ! separate the inputs into various strings
        read(post_processing_nlastrev_choices, *) (integer_auxiliar_vector(i), i = 1, n_params - 1)
        read(analysis_names, *) (params_char_vec2(i), i = 1, n_params - 1)

        !create new unified files
        call delete_file(unified_over_time_file)
        open(newunit = unified_over_time_unit, file = unified_over_time_file, status = 'new', iostat = err_status, iomsg = err_msg)
        if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
        write(unified_over_time_unit, "(a)") disclaimer
        close(unified_over_time_unit)
        call delete_file(unified_stats_file)
        open(newunit = unified_stats_unit, file = unified_stats_file, status = 'new', iostat = err_status, iomsg = err_msg)
        if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
        write(unified_stats_unit, "(a)") disclaimer
        close(unified_stats_unit)
        call delete_file(unified_loads_file)
        open(newunit = unified_loads_unit, file = unified_loads_file, status = 'new', iostat = err_status, iomsg = err_msg)
        if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
        write(unified_loads_unit, "(a)") disclaimer
        close(unified_loads_unit)

        !for each analysis described, post-process
        do l = 1, n_params - 1
            if (file_exists(trim(params_char_vec(l)))) then
                write(*,'(A)') "---> PROCESS -> processing analysis " // trim(params_char_vec2(l))
                call post_process_csv(params_char_vec(l), params_char_vec2(l), integer_auxiliar_vector(l), log_fil, disclaimer, unified_over_time_file, unified_stats_file, unified_loads_file, bin_residual_folder)
                write(*,'(A)') "---> END PROCESS -> processing of " // trim(params_char_vec2(l)) // " ended"
            else
                err_msg = "---> ERROR -> file '" // trim(params_char_vec(l)) // "' has not been found"
                call throw_warning(err_msg, log_fil)
            end if
        end do
!===========================================copy files and cleanup=========================================================================================================
        deallocate(params_char_vec)
        deallocate(params_char_vec2)
        deallocate(integer_auxiliar_vector)

        !copy the used files from the bin to the results folder
        if (.not. only_post_process) then
            if (trim(operating_system) .eq. "windows") then
                folder_name = trim(results_folder) // "\" // trim(description)
                copy_fil = trim(folder_name) // "\" // trim(unified_over_time_file_name)
                call copy_file(unified_over_time_file, copy_fil, log_fil, operating_system)
                call delete_file(unified_over_time_file)
                copy_fil = trim(folder_name) // "\" // trim(unified_stats_file_name)
                call copy_file(unified_stats_file, copy_fil, log_fil, operating_system)
                call delete_file(unified_stats_file)
                copy_fil = trim(folder_name) // "\" // trim(unified_loads_file_name)
                call copy_file(unified_loads_file, copy_fil, log_fil, operating_system)
                call delete_file(unified_loads_file)
            else if (trim(operating_system) .eq. "linux") then
                folder_name = trim(results_folder) // "/" // trim(description)
                copy_fil = trim(folder_name) // "/" // trim(unified_over_time_file_name)
                call copy_file(unified_over_time_file, copy_fil, log_fil, operating_system)
                call delete_file(unified_over_time_file)
                copy_fil = trim(folder_name) // "/" // trim(unified_stats_file_name)
                call copy_file(unified_stats_file, copy_fil, log_fil, operating_system)
                call delete_file(unified_stats_file)
                copy_fil = trim(folder_name) // "/" // trim(unified_loads_file_name)
                call copy_file(unified_loads_file, copy_fil, log_fil, operating_system)
                call delete_file(unified_loads_file)
            end if
        else
            if (trim(operating_system) .eq. "windows") then
                folder_name = trim(results_folder)
                copy_fil = trim(folder_name) // "\" // current_date // "_" // current_time_char(1:6) // "_"  // trim(unified_over_time_file_name)
                call copy_file(unified_over_time_file, copy_fil, log_fil, operating_system)
                call delete_file(unified_over_time_file)
                copy_fil = trim(folder_name) // "\" // current_date // "_" // current_time_char(1:6) // "_"  // trim(unified_stats_file_name)
                call copy_file(unified_stats_file, copy_fil, log_fil, operating_system)
                call delete_file(unified_stats_file)
                copy_fil = trim(folder_name) // "\" // current_date // "_" // current_time_char(1:6) // "_"  // trim(unified_loads_file_name)
                call copy_file(unified_loads_file, copy_fil, log_fil, operating_system)
                call delete_file(unified_loads_file)
            else if (trim(operating_system) .eq. "linux") then
                folder_name = trim(results_folder)
                copy_fil = trim(folder_name) // "/" // current_date // "_" // current_time_char(1:6) // "_" // trim(unified_over_time_file_name)
                call copy_file(unified_over_time_file, copy_fil, log_fil, operating_system)
                call delete_file(unified_over_time_file)
                copy_fil = trim(folder_name) // "/" // current_date // "_" // current_time_char(1:6) // "_" // trim(unified_stats_file_name)
                call copy_file(unified_stats_file, copy_fil, log_fil, operating_system)
                call delete_file(unified_stats_file)
                copy_fil = trim(folder_name) // "/" // current_date // "_" // current_time_char(1:6) // "_" // trim(unified_loads_file_name)
                call copy_file(unified_loads_file, copy_fil, log_fil, operating_system)
                call delete_file(unified_loads_file)
            end if
        end if

        1001 continue

        if (.not. only_post_process) then
            if (trim(operating_system) .eq. "windows") then
                folder_name = trim(results_folder) // "\" // trim(description)
                copy_fil = trim(folder_name) // "\" // trim(log_fil_name)
                call copy_file(log_fil, copy_fil, log_fil, operating_system)
                call delete_file(log_fil)
                copy_fil = trim(folder_name) // "\" // trim(scpt_fil_name)
                call copy_file(scpt_fil, copy_fil, log_fil, operating_system)
                call delete_file(scpt_fil)
            else if (trim(operating_system) .eq. "linux") then
                folder_name = trim(results_folder) // "/" // trim(description)
                copy_fil = trim(folder_name) // "/" // trim(log_fil_name)
                call copy_file(log_fil, copy_fil, log_fil, operating_system)
                call delete_file(log_fil)
                copy_fil = trim(folder_name) // "/" // trim(scpt_fil_name)
                call copy_file(scpt_fil, copy_fil, log_fil, operating_system)
                call delete_file(scpt_fil)
            end if
        end if

        write(*,'(A)') "---> END BLOCK -> BLOCK finished"
        write(*,*) 
        write(*,*) 

        go to 888 !skip the next bit of code

        889 continue ! block to skip an analysis that got an error, the script only gets here if an exception is thrown
        do while ((lowcase(trim(line)) .ne. "end"))
            read(inp_unit, '(a)', end = 200) line !end ok
            line = trim_front(line)
        end do

        888 continue

        read(inp_unit, '(a)', end = 200) line !end ok
        line = trim_front(line)
    end do
!===========================================end=========================================================================================================
    200 continue
    call delete_file(temp_fils(1))
    call delete_file(temp_fils(2))
    
    close(inp_unit)

    call clean_folder(bin_residual_folder, log_fil, operating_system)

    write(*,'(a)') "---> FINISH -> all scripts finished"

!===========================================functions and subroutines=========================================================================================================

    contains
       !-----------AutoVSP related functions and subroutines----------------
        subroutine write_to_script(lin_title, change, temp_fils, j, log_fil, changed_flag) !subroutine to deal with fle re-writing after an input has been declared
            implicit none
            character (len = *), intent(in)     ::  lin_title
            character (len = *), intent(in)     ::  change
            character (len = *), intent(in)     ::  log_fil
            integer, intent(out)                ::  j
            character (len = *), intent(in)     ::  temp_fils(2)
            character (len = 256)               ::  wrt_fil, rd_fil, quest
            logical, intent(out)                ::  changed_flag
            character (len = 256)               ::  err_msg

            quest = "__"//lowcase(trim(lin_title))//"__"
            wrt_fil = temp_fils( 1 + mod(j, 2))
            rd_fil = temp_fils( 1 + mod(j + 1, 2))

            call change_on_file(rd_fil, wrt_fil, quest, change, .false., .true., changed_flag, log_fil)
            j = j + 1

        end subroutine write_to_script

        subroutine change_on_file(in_fname, out_fname, quest, change, first_flag, uncommentate_flag, changed_flag, log_fil) !subroutine to search for the declared name and substitute for the input
            implicit none
            character (len = *), intent(in)         ::  in_fname
            character (len = *), intent(in)         ::  out_fname
            character (len = *), intent(in)         ::  quest
            character (len = *), intent(in)         ::  change
            character (len = *), intent(in)         ::  log_fil
            logical, intent(in)                     ::  first_flag
            logical, intent(in)                     ::  uncommentate_flag
            logical, intent(out)                    ::  changed_flag
            logical                                 ::  done
            character (len = 10000)                 ::  line, placeholder
            integer                                 ::  i, j
            integer                                 ::  len_orig, len_changed, delta, len_line
            integer                                 ::  in_unit, out_unit
            integer                                 ::  err_status
            character (len = 256)                   ::  err_msg

            len_orig = len_trim(quest)
            len_changed = len_trim(change)
            delta = len_changed - len_orig

            open(newunit = in_unit, file = in_fname, status = "old", iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
            call delete_file(out_fname)
            open(newunit = out_unit, file = out_fname, status = "new", iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if

            changed_flag = .false.
            done = .false.
            do
                read(in_unit, '(a)', end = 200) line
                do i = 1, len_trim(line)
                    if ((line(i:i + len_orig - 1) .eq. trim(quest)) .and. (.not. done)) then
                        len_line = len_trim(line)
                        if (uncommentate_flag) then
                            do j = 1, len_line - 1
                                if (line(j:j + 1) .eq. "//") then
                                    line(j:j + 1) = "  "
                                    go to 300
                                end if
                            end do
                        end if
                        300 continue
                        placeholder = line(i + len_orig:len_trim(line))
                        line(i:len_trim(line)) = ""
                        line(i:i + len_changed - 1) = trim(change)
                        line(i + len_changed:len_line + delta) = trim(placeholder)
                        if (first_flag) then
                            done = .true.
                        end if
                        changed_flag = .true.
                    end if
                end do
                write(out_unit, '(a)') trim(line)
            end do
            200 continue
            close(in_unit)
            close(out_unit)
        end subroutine change_on_file

        subroutine throw_warning(err_msg, log_fil) !subroutine to display a message and also write that message on the console_log
            implicit none 
            character (len = *), intent(in)     ::  err_msg
            character (len = *), intent(in)     ::  log_fil
            integer                             ::  err_unit

            write (*,"(a)") err_msg
            open(newunit = err_unit, file = log_fil, status = "unknown")
                call fseek(err_unit, 0, 2)
                write (err_unit,*) 
                write (err_unit,"(a)") err_msg
            close(err_unit)
        end subroutine 

        subroutine count_parameters_line(line, separators, n_params, no_repeat) !subroutine to return the number of parameters on a line
            implicit none
            character (len = *), intent(in)     ::  line
            character (len = *), intent(in)     ::  separators
            logical, intent(in)                 ::  no_repeat
            integer, intent(out)                ::  n_params
            integer                             ::  i, j
            logical                             ::  aux_bool, equal

            aux_bool = .true.
            n_params = 0
            do i = 1, len_trim(line)
                equal = .false.
                do j = 1, len(separators)
                    if (line(i:i) .eq. separators(j:j)) equal = .true.
                end do
                if (equal) then
                    if (aux_bool) then
                        n_params = n_params + 1
                    end if
                    if (no_repeat) then
                        aux_bool = .false.
                    end if
                else
                    aux_bool = .true.
                end if
            end do
            n_params = n_params + 1 ! count title
        end subroutine count_parameters_line

        subroutine dump_function(main1_fil, main2_fil, template_file, log_fil, operating_system) !subroutine to write the function template file on the main file
            implicit none
            character (len = *), intent(in) ::  template_file
            character (len = *), intent(in) ::  log_fil
            character (len = *), intent(in) ::  operating_system
            character (len = *), intent(in) ::  main1_fil 
            character (len = *), intent(in) ::  main2_fil
            integer                         ::  tp_unit, m1_unit, m2_unit
            integer                         ::  err_status
            character (len = 256)           ::  err_msg
            character (len = 10000)         ::  line

            open(newunit = tp_unit, file = template_file, status = 'old', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
            
            open(newunit = m1_unit, file = main1_fil, status = 'unknown', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
            
            open(newunit = m2_unit, file = main2_fil, status = 'unknown', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if

            call fseek(m1_unit, 0, 2)
            call fseek(m2_unit, 0, 2)
            read(tp_unit, '(A)', end = 600) line
            write(m1_unit, '(A)') achar(9) // "check = " // trim(line(5:len_trim(line))) // "; if (check != 0) {return 1;}"
            write(m2_unit, '(A)') trim(line)
            do
                read(tp_unit, '(A)', end = 600) line
                write(m2_unit, '(A)') trim(line)
            end do

            600 continue

            close(tp_unit)
            close(m1_unit)
            close(m2_unit)
            call delete_file(template_file)

        end subroutine dump_function

        subroutine dump_function2(main2_fil, template_file, log_fil, operating_system) !subroutine to write the function template file on the main file
            implicit none
            character (len = *), intent(in) ::  template_file
            character (len = *), intent(in) ::  log_fil
            character (len = *), intent(in) ::  operating_system
            character (len = *), intent(in) ::  main2_fil
            integer                         ::  tp_unit, m1_unit, m2_unit
            integer                         ::  err_status
            character (len = 256)           ::  err_msg
            character (len = 10000)         ::  line

            open(newunit = tp_unit, file = template_file, status = 'old', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
            
            open(newunit = m2_unit, file = main2_fil, status = 'unknown', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if

            call fseek(m2_unit, 0, 2)
            do
                read(tp_unit, '(A)', end = 600) line
                write(m2_unit, '(A)') trim(line)
            end do

            600 continue

            close(tp_unit)
            close(m2_unit)
            call delete_file(template_file)

        end subroutine dump_function2

        subroutine join_files(file1, file2, final_file, log_fil) !subroutine to join 2 files together
            implicit none
            character (len = *), intent(in) ::  file1
            character (len = *), intent(in) ::  file2
            character (len = *), intent(in) ::  final_file
            character (len = *), intent(in) ::  log_fil
            integer                         ::  f1_unit, f2_unit, fin_unit
            integer                         ::  err_status
            character (len = 256)           ::  err_msg
            character (len = 10000)         ::  line

            open(newunit = f1_unit, file = file1, status = 'old', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if

            open(newunit = f2_unit, file = file2, status = 'old', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
            
            call delete_file(final_file)
            open(newunit = fin_unit, file = final_file, status = 'new', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
        
            do
                read(f1_unit, '(a)', end = 700) line
                write(fin_unit, '(a)') trim(line)
            end do
            700 continue
            do
                read(f2_unit, '(a)', end = 701) line
                write(fin_unit, '(a)') trim(line)
            end do
            701 continue
            
            close(f1_unit)
            close(f2_unit)
            close(fin_unit)

            call delete_file(file1)
            call delete_file(file2)

        end subroutine join_files

        subroutine post_process_csv(csv_fil, analysis_name, choice_nlastrevs, log_fil, disclaimer, unified_over_time_file, unified_stats_file, unified_loads_file, bin_residual_folder) !post-processing subroutine
            implicit none
            
           !variables
            !files
            character (len = *), intent(in)     ::  csv_fil
            character (len = *), intent(in)     ::  unified_over_time_file
            character (len = *), intent(in)     ::  unified_stats_file
            character (len = *), intent(in)     ::  unified_loads_file
            character (len = 256)               ::  over_time_file, stats_file, loads_file
            integer                             ::  csv_unit, over_time_unit, stats_unit, loads_unit                  ! unit integers to handle files
            integer                             ::  unified_over_time_unit, unified_stats_unit, unified_loads_unit                 ! unit integers to handle files
            character (len = *), intent(in)     ::  log_fil

            character (len = *), intent(in)     ::  bin_residual_folder
            character (len = *), intent(in)     ::  analysis_name
            character (len = *), intent(in)     ::  disclaimer
            integer, intent(in)                 ::  choice_nlastrevs
            real, parameter                     ::  pi = 4*atan(1.0)
            character                           ::  file_delimiter = "," 
            character (len = 256)               ::  fmt                                             ! character to store formt string
            integer                             ::  err_status                                      ! error status 
            character (len = 256)               ::  err_msg                                         ! error message 
            character (len = 256)               ::  lin_title
            character (len = 10000)             ::  line                                      
            character (len = 256)               ::  label, label2                   
            character (len = 256)               ::  prop_name                   
            character (len = 256)               ::  prop_number_char, n_timesteps_lastrev_char, n_timesteps_total_char, critical_ntimesteps_lastrev_char
            integer                             ::  n_params, n_timesteps_lastrev, n_timesteps_total, n_props, critical_ntimesteps_lastrev
            real                                ::  analy_data(5)                                   ! vector to store analysis data (1 - sref) (2 - cref) (3 - vref) (4 - rho)
            real                                ::  omega, diam, rpm                   
            real, allocatable                   ::  coefs_vec(:), time_vec(:), angle_vec(:)         ! vector of real numbers to be used arbitrarily
            real, allocatable                   ::  coefs_vec2(:), time_vec2(:), angle_vec2(:)         ! vector of real numbers to be used arbitrarily
            real, allocatable                   ::  short_time_vec(:), short_angle_vec(:)           ! vector of real numbers to be used arbitrarily
            real, allocatable                   ::  s_vec(:), station_vec(:)             ! vector to store the force or moment results on a line
            real, dimension(:,:), allocatable   ::  coefs_mat, fm_mat                               ! vector to store the radial number of a rotor anaysis
            real, allocatable                   ::  last_rev_vec(:)                                ! vector to store the numerical results of the last revolution 
            integer                             ::  n_stations, n_blades
            logical                             ::  vref_defined, time_analysis
            integer                             ::  i, j, k, l, prop_iterator                                     ! iterators 
            real                                ::  dumreal1, dumreal2, dumreal3                    ! dummy floats 
            character (len = 256)               ::  dumchar1, dumchar2, dumchar3                    ! dummy strings

           !initial definitions
            vref_defined = .false.
            over_time_file = csv_fil(1:len_trim(csv_fil) - 4) // "_over_time_results.txt"
            stats_file = csv_fil(1:len_trim(csv_fil) - 4) // "_stats_results.txt"
            loads_file = csv_fil(1:len_trim(csv_fil) - 4) // "_loads_results.txt"

           !file openings
            !raw file
            open(newunit = csv_unit, file = csv_fil, status = 'old', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
            call delete_file(over_time_file)

            !output files
            call delete_file(over_time_file)
            open(newunit = over_time_unit, file = over_time_file, status = 'unknown', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
            write(over_time_unit, "(a)") disclaimer
            
            call delete_file(stats_file)
            open(newunit = stats_unit, file = stats_file, status = 'unknown', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
            write(stats_unit, "(a)") disclaimer
            
            call delete_file(loads_file)
            open(newunit = loads_unit, file = loads_file, status = 'unknown', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
            write(loads_unit, "(a)") disclaimer

            !unified output files
            open(newunit = unified_over_time_unit, file = unified_over_time_file, status = 'old', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
            call fseek(unified_over_time_unit, 0, 2)
            write(unified_over_time_unit, "(a)") "==============================RESULTS FOR ANALYSIS " // trim(analysis_name) // "============================================="
            
            open(newunit = unified_stats_unit, file = unified_stats_file, status = 'old', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
            call fseek(unified_stats_unit, 0, 2)
            write(unified_stats_unit, "(a)") "==============================RESULTS FOR ANALYSIS " // trim(analysis_name) // "============================================="
            
            open(newunit = unified_loads_unit, file = unified_loads_file, status = 'old', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
            call fseek(unified_loads_unit, 0, 2)
            write(unified_loads_unit, "(a)") "==============================RESULTS FOR ANALYSIS " // trim(analysis_name) // "============================================="

           !----------------------------------Get simulation parameters---------------------------
            line = "$"
            do while ((line(1:7) .ne. "FC_Cref")) 
                read(csv_unit, '(a)', end = 210) line
            end do
            read(line, *) lin_title, analy_data(2)
            do while ((line(1:6) .ne. "FC_Rho")) 
                read(csv_unit, '(a)', end = 210) line
            end do
            read(line, *) lin_title, analy_data(4)
            do while ((line(1:7) .ne. "FC_Sref")) 
                read(csv_unit, '(a)', end = 210) line
            end do
            read(line, *) lin_title, analy_data(1)
            do while ((line(1:7) .ne. "FC_Vinf")) 
                read(csv_unit, '(a)', end = 210) line
            end do
            read(line, *) lin_title, analy_data(5)

            rewind csv_unit
            line = "$"
            do while ((line(14:30) .ne. "VSPAERO_Blade_Avg")) 
                read(csv_unit, '(a)', end = 211) line
            end do
            do while ((line(1:6) .ne. "TipVel")) 
                read(csv_unit, '(a)', end = 211) line
            end do
            read(line, *) lin_title, dumreal1
            do while ((line(1:6) .ne. "V_Vref")) 
                read(csv_unit, '(a)', end = 211) line
            end do
            call count_parameters_line(line, file_delimiter, n_params, .false.)
            read(line, *) lin_title, (dumreal3, k = 1, n_params - 2), dumreal2
            analy_data(3) = dumreal1/dumreal2 ! vref = Vtip/(v/vref) assuming v is at the tip

            210 continue
            vref_defined = .true.
            211 continue

           !----------------------------------Get number of propellers simulated---------------------------
            rewind csv_unit
            line = "$"
            do while ((line(1:10) .ne. "ResultsVec")) 
                read(csv_unit, '(a)', end = 212) line
            end do
            n_props = count_ocurrences_line(line, "VSPAERO_Rotor")

           !----------------------------------Get number of timesteps---------------------------
            rewind csv_unit
            line = "$"
            do while ((line(14:28) .ne. "VSPAERO_History")) 
                read(csv_unit, '(a)', end = 209) line
            end do
            do while ((line(1:4) .ne. "Time"))
                read(csv_unit, '(a)', end = 289) line 
            end do
            call count_parameters_line(line, file_delimiter, n_params, .false.)
            n_timesteps_total = n_params - 1
            time_analysis = .true.
            go to 288
            
            289 continue
                n_timesteps_total = 1
                time_analysis = .false.
            288 continue

            critical_ntimesteps_lastrev = 0
            allocate(angle_vec(n_timesteps_total))
            do prop_iterator = 1, n_props
                rewind csv_unit
                do j = 1, prop_iterator ! look for block of the index of the prop
                    line = "$"
                    do while ((line(14:26) .ne. "VSPAERO_Rotor")) 
                        read(csv_unit, '(a)', end = 233) line
                    end do
                end do

                do while (line(1:5) .ne. 'Angle')
                    read(csv_unit, '(a)', end = 233) line
                end do
                read(line, *) dumchar1, (angle_vec(k), k = 1, n_timesteps_total)
                do i = 1, n_timesteps_total
                    angle_vec(i) = mod(angle_vec(i), 360.0)
                end do 
                n_timesteps_lastrev = 2 !starts at 2 to compensate for errors in the calculus
                do i = 1, n_timesteps_total - 1 ! count the number of timesteps it takes this rotor to complete a full revolution
                    if (angle_vec(i + 1) < angle_vec(i)) then
                        go to 401
                    end if
                    n_timesteps_lastrev = n_timesteps_lastrev + 1
                end do
                err_msg = "*last revolution of rotor " // trim(prop_number_char) // " not complete, it is recommended to inpu a faster rpm*"
                call throw_warning(err_msg, log_fil)
                401 continue
                if (n_timesteps_lastrev .gt. critical_ntimesteps_lastrev) critical_ntimesteps_lastrev = n_timesteps_lastrev
            end do
            233 continue
            critical_ntimesteps_lastrev = min(critical_ntimesteps_lastrev*choice_nlastrevs, n_timesteps_total)

            212 continue
           !----------------------------------global coefficients---------------------------

            write(over_time_unit, '(A)') 
            write(over_time_unit, '(A)') "======global coefficients and forces======"
            write(unified_over_time_unit, '(A)') 
            write(unified_over_time_unit, '(A)') "======global coefficients and forces======"
            write(stats_unit, '(A)') 
            write(stats_unit, '(A)') "======global coefficients and forces======"
            write(stats_unit, "(5a15)") "component", "min", "max", "mean", "p2p"
            write(unified_stats_unit, '(A)') 
            write(unified_stats_unit, '(A)') "======global coefficients and forces======"
            write(unified_stats_unit, "(5a15)") "component", "min", "max", "mean", "p2p"

            allocate(coefs_vec(n_timesteps_total))
            allocate(time_vec(n_timesteps_total))
            allocate(last_rev_vec(critical_ntimesteps_lastrev))

            rewind csv_unit
            line = "$"
            do while ((line(1:4) .ne. "Time"))
                read(csv_unit, '(a)', end = 289) line 
            end do
            read(line, *) dumchar1, (time_vec(k), k = 1, n_timesteps_total)

            write(critical_ntimesteps_lastrev_char,*) (critical_ntimesteps_lastrev)
            fmt = "(a15,"//trim(critical_ntimesteps_lastrev_char)//"f15.6)"
            last_rev_vec(1:critical_ntimesteps_lastrev) = time_vec((n_timesteps_total + 1) - (critical_ntimesteps_lastrev):n_timesteps_total)
            write(over_time_unit, fmt) "time", (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
            write(unified_over_time_unit, fmt) "time", (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)

            rewind csv_unit
            line = "$"
            do while ((line(14:28) .ne. "VSPAERO_History")) 
                read(csv_unit, '(a)', end = 209) line
            end do
            do while ((line(1:3) .ne. "CDi"))
                read(csv_unit, '(a)', end = 209) line !skips first 4 lines
            end do
            do while (line(1:2)  .ne. 'E,')
                read(line, *) lin_title, (coefs_vec(k), k = 1, n_timesteps_total)
                fmt = "(a15,"//trim(critical_ntimesteps_lastrev_char)//"f15.6)"
                last_rev_vec(1:critical_ntimesteps_lastrev) = coefs_vec((n_timesteps_total + 1) - (critical_ntimesteps_lastrev):n_timesteps_total)
                write(over_time_unit, fmt) trim(lin_title), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
                write(unified_over_time_unit, fmt) trim(lin_title), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
                write(stats_unit, '(a15,4f15.6)') trim(lin_title), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                write(unified_stats_unit, '(a15,4f15.6)') trim(lin_title), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                if ((lin_title .eq. "CMx") .or. (lin_title .eq. "CMy") .or. (lin_title .eq. "CMz")) then
                    do k = 1, n_timesteps_total !calculate and write moments
                        if (vref_defined) coefs_vec(k) = analy_data(4)*(analy_data(3)**2)*analy_data(1)*analy_data(2)*coefs_vec(k)/2 !rho*(vinf**2)*s_ref*c_ref*coef_val/2
                        if (.not. vref_defined) coefs_vec(k) = analy_data(4)*(analy_data(5)**2)*analy_data(1)*analy_data(2)*coefs_vec(k)/2 !rho*(vinf**2)*s_ref*c_ref*coef_val/2
                    end do
                else 
                    do k = 1, n_timesteps_total !calculate and write moments
                        if (vref_defined) coefs_vec(k) = analy_data(4)*(analy_data(3)**2)*analy_data(1)*coefs_vec(k)/2 !rho*(vinf**2)*s_ref*coef_val/2
                        if (.not. vref_defined) coefs_vec(k) = analy_data(4)*(analy_data(5)**2)*analy_data(1)*coefs_vec(k)/2 !rho*(vinf**2)*s_ref*coef_val/2    
                    end do

                end if 
                fmt = "(a15,"//trim(critical_ntimesteps_lastrev_char)//"f15.3)"
                last_rev_vec(1:critical_ntimesteps_lastrev) = coefs_vec((n_timesteps_total + 1) - (critical_ntimesteps_lastrev):n_timesteps_total)
                write(over_time_unit, fmt) lin_title(2:len_trim(lin_title)), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
                write(unified_over_time_unit, fmt) lin_title(2:len_trim(lin_title)), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
                write(stats_unit, '(a15,4f15.3)') trim(lin_title(2:len_trim(lin_title))), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                write(unified_stats_unit, '(a15,4f15.3)') trim(lin_title(2:len_trim(lin_title))), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                
                read(csv_unit, '(a)', end = 209) line
                do while ((line(1:6)  .eq. 'CDi_Un') .or. (line(1:6) .eq. 'CFx_Un') .or. (line(1:6)  .eq. 'CFy_Un') .or. (line(1:6)  .eq. 'CFz_Un') .or. (line(1:5)  .eq. 'CL_Un') .or. (line(1:6)  .eq. 'CMx_Un') .or. (line(1:6)  .eq. 'CMy_Un') .or. (line(1:6)  .eq. 'CMz_Un') .or. (line(1:5)  .eq. 'CS_Un'))
                    read(csv_unit, '(a)', end = 209) line
                end do 
            end do
            209 continue

            if (.not. time_analysis) go to 1003

           !----------------------------------do for each rotor------------------------------------ 
            do prop_iterator = 1, n_props
                n_blades = 0
                rewind csv_unit
                line = "$"
               !------------------------get number of blades
                do
                    do while ((line(14:30) .ne. "VSPAERO_Blade_Avg")) 
                        read(csv_unit, '(a)', end = 213) line
                    end do
                    do while ((line(1:9) .ne. "Rotor_Num")) 
                        read(csv_unit, '(a)', end = 213) line
                    end do
                    read(line, *) lin_title, dumreal1
                    if (dumreal1 .eq. prop_iterator) n_blades = n_blades + 1
                end do
                213 continue
               !------------------------get propeller name
                rewind csv_unit
                do j = 1, prop_iterator ! look for block of the index of the prop
                    line = "$"
                    do while ((line(14:26) .ne. "VSPAERO_Rotor")) 
                        read(csv_unit, '(a)', end = 203) line
                    end do
                end do
                do while ((line(1:10) .ne. "Group_Name")) 
                    read(csv_unit, '(a)', end = 214) line
                end do
                read(line, *) lin_title, prop_name
                prop_name = prop_name(1:len_trim(prop_name) - 7)
                214 continue

               !------------------------get diameter and rpm of rotor
                rewind csv_unit
                write(prop_number_char, '(i2)') prop_iterator
                do j = 1, prop_iterator ! look for block of the index of the prop
                    line = "$"
                    do while ((line(14:26) .ne. "VSPAERO_Rotor")) 
                        read(csv_unit, '(a)', end = 203) line
                    end do
                end do
                do while (line(1:8) .ne. 'Diameter')
                    read(csv_unit, '(a)', end = 203) line
                end do
                read(line, *) lin_title, diam
                do while (line(1:3) .ne. 'RPM')
                    read(csv_unit, '(a)', end = 203) line
                end do
                read(line, *) lin_title, rpm
                omega = rpm*2*pi/60

               !------------------------get angle variation over time
                rewind csv_unit
                do j = 1, prop_iterator ! look for block of the index of the prop
                    line = "$"
                    do while ((line(14:26) .ne. "VSPAERO_Rotor")) 
                        read(csv_unit, '(a)', end = 203) line
                    end do
                end do

                do while (line(1:5) .ne. 'Angle')
                    read(csv_unit, '(a)', end = 203) line
                end do
                read(line, *) dumchar1, (angle_vec(k), k = 1, n_timesteps_total)
                do i = 1, n_timesteps_total
                    angle_vec(i) = mod(angle_vec(i), 360.0)
                end do 

               !------------------------last revolution rotor global coefficients------------------------------------
                write(over_time_unit, '(A)') 
                write(over_time_unit, '(A)') "======last rev coefs and forces by time/angle for rotor: " // trim(prop_name) // "======"
                write(unified_over_time_unit, '(A)') 
                write(unified_over_time_unit, '(A)') "======last rev coefs and forces by time/angle for rotor: " // trim(prop_name) // "======"
                write(stats_unit, '(A)') 
                write(stats_unit, '(A)') "======last rev coefs and forces by time/angle for rotor: " // trim(prop_name) // "======"
                write(stats_unit, "(5a15)") "component", "min", "max", "mean", "p2p"
                write(unified_stats_unit, '(A)') 
                write(unified_stats_unit, '(A)') "======last rev coefs and forces by time/angle for rotor: " // trim(prop_name) // "======"
                write(unified_stats_unit, "(5a15)") "component", "min", "max", "mean", "p2p"

                last_rev_vec(1:critical_ntimesteps_lastrev) = angle_vec((n_timesteps_total + 1) - (critical_ntimesteps_lastrev):n_timesteps_total)
                fmt = "(a15,"//trim(critical_ntimesteps_lastrev_char)//"f15.1)"
                write(over_time_unit, fmt) "angle", (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
                write(unified_over_time_unit, fmt) "angle", (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)

                last_rev_vec(1:critical_ntimesteps_lastrev) = time_vec((n_timesteps_total + 1) - (critical_ntimesteps_lastrev):n_timesteps_total)
                fmt = "(a15,"//trim(critical_ntimesteps_lastrev_char)//"f15.5)"
                write(unified_over_time_unit, fmt) "time", (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)

                rewind csv_unit
                do j = 1, prop_iterator ! position the reader above the groups block again
                    line = "$"
                    do while ((line(14:26) .ne. "VSPAERO_Group")) 
                        read(csv_unit, '(a)', end = 203) line
                    end do
                end do
                do while ((line(1:3) .ne. "CD,"))
                    read(csv_unit, '(a)', end = 203) line 
                end do
                do while (line(1:10) .ne. 'Group_Name') ! process every coefficient untill the block ends
                    read(line, *) lin_title, (coefs_vec(k), k = 1, n_timesteps_total)
                    label = ""
                    if ((lin_title .eq. 'Cmx') .or. (lin_title .eq. 'Cmy') .or. (lin_title .eq. 'Cmz')) then
                        last_rev_vec(1:critical_ntimesteps_lastrev) = coefs_vec((n_timesteps_total + 1) - (critical_ntimesteps_lastrev):n_timesteps_total) 
                        label = lin_title
                        fmt = "(a15,"//trim(critical_ntimesteps_lastrev_char)//"f15.6)"
                        write(over_time_unit, fmt) trim(label), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev) !write coefs
                        write(unified_over_time_unit, fmt) trim(label), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev) !write coefs
                        write(stats_unit, '(a15,4f15.6)') trim(label), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                        write(unified_stats_unit, '(a15,4f15.6)') trim(label), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                        do k = 1, critical_ntimesteps_lastrev !calculate and write moments
                            if (vref_defined) last_rev_vec(k) = analy_data(4)*(analy_data(3)**2)*analy_data(1)*analy_data(2)*last_rev_vec(k)/2 !rho*(vinf**2)*s_ref*c_ref*coef_val/2
                            if (.not. vref_defined) last_rev_vec(k) = analy_data(4)*(analy_data(5)**2)*analy_data(1)*analy_data(2)*last_rev_vec(k)/2 !rho*(vinf**2)*s_ref*c_ref*coef_val/2
                        end do
                        label = "M"//lin_title(3:3)
                        fmt = "(a15,"//trim(critical_ntimesteps_lastrev_char)//"f15.3)"
                        write(over_time_unit, fmt) trim(label), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
                        write(unified_over_time_unit, fmt) trim(label), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
                        write(stats_unit, '(a15,4f15.3)') trim(label), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                        write(unified_stats_unit, '(a15,4f15.3)') trim(label), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                    else if ((lin_title .eq. 'Cx') .or. (lin_title .eq. 'Cy') .or. (lin_title .eq. 'Cz')) then
                        last_rev_vec(1:critical_ntimesteps_lastrev) = coefs_vec((n_timesteps_total + 1) - (critical_ntimesteps_lastrev):n_timesteps_total)
                        label = lin_title
                        fmt = "(a15,"//trim(critical_ntimesteps_lastrev_char)//"f15.6)"
                        write(over_time_unit, fmt) trim(label), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev) !write coefs
                        write(unified_over_time_unit, fmt) trim(label), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev) !write coefs
                        write(stats_unit, '(a15,4f15.6)') trim(label), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                        write(unified_stats_unit, '(a15,4f15.6)') trim(label), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                        do k = 1, critical_ntimesteps_lastrev !calculate and write forces
                            if (vref_defined) last_rev_vec(k) = analy_data(4)*(analy_data(3)**2)*analy_data(1)*last_rev_vec(k)/2 !rho*(vinf**2)*s_ref*coef_val/2
                            if (.not. vref_defined) last_rev_vec(k) = analy_data(4)*(analy_data(5)**2)*analy_data(1)*last_rev_vec(k)/2 !rho*(vinf**2)*s_ref*coef_val/2
                        end do
                        label = "F"//lin_title(2:2)
                        fmt = "(a15,"//trim(critical_ntimesteps_lastrev_char)//"f15.3)"
                        write(over_time_unit, fmt) trim(label), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
                        write(unified_over_time_unit, fmt) trim(label), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
                        write(stats_unit, '(a15,4f15.3)') trim(label), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                        write(unified_stats_unit, '(a15,4f15.3)') trim(label), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                    end if
                    read(csv_unit, '(a)', end = 203) line !reads next line
                end do 
                203 continue

               !------------------------last revoluton rotor metrics-----------------------------
                rewind csv_unit

                write(over_time_unit, '(A)') 
                write(over_time_unit, '(A)') "======last rev rotor metrics for rotor: " // trim(prop_name) // "======"
                write(unified_over_time_unit, '(A)') 
                write(unified_over_time_unit, '(A)') "======last rev rotor metrics for rotor: " // trim(prop_name) // "======"

                last_rev_vec(1:critical_ntimesteps_lastrev) = angle_vec((n_timesteps_total + 1) - (critical_ntimesteps_lastrev):n_timesteps_total)
                fmt = "(a15,"//trim(critical_ntimesteps_lastrev_char)//"f15.1)"
                write(over_time_unit, fmt) "angle", (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
                write(unified_over_time_unit, fmt) "angle", (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)

                last_rev_vec(1:critical_ntimesteps_lastrev) = time_vec((n_timesteps_total + 1) - (critical_ntimesteps_lastrev):n_timesteps_total)
                fmt = "(a15,"//trim(critical_ntimesteps_lastrev_char)//"f15.6)"
                write(over_time_unit, fmt) "time", (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
                write(unified_over_time_unit, fmt) "time", (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)

                do j = 1, prop_iterator
                    line = "$"
                    do while ((line(14:26) .ne. "VSPAERO_Rotor")) 
                        read(csv_unit, '(a)', end = 207) line
                    end do
                end do
                do while ((line(1:3) .ne. "CP,"))
                    read(csv_unit, '(a)', end = 207) line !skips first 4 lines
                end do
                do while (line(1:4)  .ne. 'Time')
                    read(line, *) lin_title, (coefs_vec(k), k = 1, n_timesteps_total)
                    last_rev_vec(1:critical_ntimesteps_lastrev) = coefs_vec((n_timesteps_total + 1) - (critical_ntimesteps_lastrev):n_timesteps_total) 
                    fmt = "(a15,"//trim(critical_ntimesteps_lastrev_char)//"f15.6)"
                    if ((lin_title .eq. "Thrust") .or. (lin_title .eq. "Moment")) fmt = "(a15,"//trim(critical_ntimesteps_lastrev_char)//"f15.3)"
                    write(over_time_unit, fmt) trim(lin_title), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
                    write(unified_over_time_unit, fmt) trim(lin_title), (last_rev_vec(k), k = 1, critical_ntimesteps_lastrev)
                    write(stats_unit, '(a15,4f15.6)') trim(lin_title), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                    write(unified_stats_unit, '(a15,4f15.6)') trim(lin_title), minval(last_rev_vec), maxval(last_rev_vec), sum(last_rev_vec)/size(last_rev_vec), maxval(last_rev_vec) - minval(last_rev_vec)
                    
                    read(csv_unit, '(a)', end = 207) line
                    do while ((line(1:8)  .eq. 'Diameter') .or. (line(1:10) .eq. 'Group_Name') .or. (line(1:2)  .eq. 'J,') .or. (line(1:3)  .eq. 'RPM') .or. (line(1:9)  .eq. 'Rotor_Num') .or. (line(1:7)  .eq. 'Momenti') .or. (line(1:7)  .eq. 'Momento') .or. (line(1:7)  .eq. 'Thrusti') .or. (line(1:7)  .eq. 'Thrusto'))
                        read(csv_unit, '(a)', end = 207) line
                    end do 
                end do

                207 continue

               

               !------------------------last revolution loads------------------------------------

                write(loads_unit, '(A)') 
                write(loads_unit, '(A)') "======last rev blade loads for rotor: " // trim(prop_name) // "======="
                write(unified_loads_unit, '(A)') 
                write(unified_loads_unit, '(A)') "======last rev blade loads for rotor: " // trim(prop_name) // "======="
                
                rewind csv_unit
                do j = 1, (prop_iterator + ((prop_iterator - 1)*(n_blades - 1))) ! find the block of the first blade of the prop being handled
                    line = "$"
                    do while ((line(14:36) .ne. "VSPAERO_Blade_Last_Rev")) 
                        read(csv_unit, '(a)', end = 205) line
                    end do
                end do
                do while ((line(1:5) .ne. "Angle"))
                    read(csv_unit, '(a)', end = 205) line !skips first 3 lines
                end do
                call count_parameters_line(line, file_delimiter, n_params, .false.)

                allocate(coefs_vec2(n_params - 1))
                allocate(time_vec2(n_params - 1))
                allocate(angle_vec2(n_params - 1))
                allocate(station_vec(n_params - 1))

                do while ((line(1:7) .ne. "Station"))
                    read(csv_unit, '(a)', end = 205) line !skips first 3 lines
                end do

                read(line, *) dumchar1, (station_vec(k), k = 1, n_params - 1)
                do i = 1, n_params - 2 
                    if (station_vec(i + 1) < station_vec(i)) then
                        n_stations = station_vec(i)
                    end if
                end do

                allocate(s_vec(n_stations))
                allocate(coefs_mat(critical_ntimesteps_lastrev, n_stations))
                allocate(fm_mat(critical_ntimesteps_lastrev, n_stations))
                allocate(short_angle_vec(critical_ntimesteps_lastrev))
                allocate(short_time_vec(critical_ntimesteps_lastrev))

                rewind csv_unit
                do j = 1, (prop_iterator + ((prop_iterator - 1)*(n_blades - 1))) ! find the block of the first blade of the prop being handled
                    line = "$"
                    do while ((line(14:36) .ne. "VSPAERO_Blade_Last_Rev")) 
                        read(csv_unit, '(a)', end = 205) line
                    end do
                end do
                do while ((line(1:5) .ne. "Angle"))
                    read(csv_unit, '(a)', end = 205) line !skips first 3 lines
                end do

                read(line, *) dumchar1, (angle_vec2(k), k = 1, n_params - 1)
                do i = 1, n_params - 1
                    angle_vec2(i) = mod(angle_vec2(i), 2*pi)*180/pi
                end do

                do while (line(1:2) .ne. 'S,')
                    read(csv_unit, '(a)', end = 205) line
                end do
                read(line, *) dumchar1, (s_vec(k), k = 1, n_stations)

                do while (line(1:7) .ne. 'Station')
                    read(csv_unit, '(a)', end = 205) line
                end do
                read(line, *) dumchar1, (station_vec(k), k = 1, n_params - 1)

                do while (line(1:4) .ne. 'Time')
                    read(csv_unit, '(a)', end = 205) line
                end do
                read(line, *) dumchar1, (time_vec2(k), k = 1, n_params - 1)

                rewind csv_unit
                line = "$"
                do j = 1, (prop_iterator + ((prop_iterator - 1)*(n_blades - 1)))
                    line = "$"
                    do while ((line(14:36) .ne. "VSPAERO_Blade_Last_Rev")) 
                        read(csv_unit, '(a)', end = 203) line
                    end do
                end do
                do while ((line(1:9) .ne. "Blade_Num"))
                    read(csv_unit, '(a)', end = 205) line 
                end do

                read(csv_unit, '(a)', end = 205) line
                label = ""

                do while (line(1:5) .ne. 'Chord')
                    read(line, *) lin_title, (coefs_vec2(k), k = 1, n_params - 1)

                    if ((lin_title .eq. 'CN_H') .or. (lin_title .eq. 'CS_H') .or. (lin_title .eq. 'CT_H') .or. (lin_title .eq. 'CQ_H')) then
                        label = trim(lin_title)
                        do i = 1, n_stations
                            k = 1
                            do j = 1, n_params - 1
                                if (station_vec(j) .eq. i) then
                                    coefs_mat(k, i) = coefs_vec2(j)
                                    if (lin_title .eq. 'CQ_H') then
                                        fm_mat(k, i) = coefs_vec2(j)*analy_data(4)*((omega*(diam/2))**2)*pi*((diam/2)**2)*diam/2!forces go here coef = dot(df,svec) / (density * a *(omega*r)^2) rho = analy_data(4) a = pi*r^2
                                        label2 = "moment"
                                    else
                                        fm_mat(k, i) = coefs_vec2(j)*analy_data(4)*((omega*(diam/2))**2)*pi*((diam/2)**2)!forces go here coef = dot(df,svec) / (density * a *(omega*r)^2) rho = analy_data(4) a = pi*r^2
                                        label2 = "force"
                                    end if
                                    short_angle_vec(k) = angle_vec2(j)
                                    short_time_vec(k) = time_vec2(j)
                                    k = k + 1
                                end if
                            end do
                        end do
                        write(loads_unit, '(A)') 
                        write(unified_loads_unit, '(A)') 
                        fmt = '(2a15, '//trim(critical_ntimesteps_lastrev_char)//'f15.5)'
                        write(loads_unit, fmt) "-", "time", (short_time_vec(i), i = 1, critical_ntimesteps_lastrev)
                        write(unified_loads_unit, fmt) "-", "time", (short_time_vec(i), i = 1, critical_ntimesteps_lastrev)
                        fmt = '(2a15, '//trim(critical_ntimesteps_lastrev_char)//'f15.1)'
                        write(loads_unit, fmt) "-", "angle", (short_angle_vec(i), i = 1, critical_ntimesteps_lastrev)
                        write(unified_loads_unit, fmt) "-", "angle", (short_angle_vec(i), i = 1, critical_ntimesteps_lastrev)
                        fmt = '(2a15,'//trim(critical_ntimesteps_lastrev_char)//'a15)'
                        write(loads_unit, fmt) "station", "s", (trim(label), i = 1, critical_ntimesteps_lastrev)
                        write(unified_loads_unit, fmt) "station", "s", (trim(label), i = 1, critical_ntimesteps_lastrev)
                        fmt = '(f15.1, f15.4, '//trim(critical_ntimesteps_lastrev_char)//'f15.6)'
                        do i = 1, n_stations
                            write(loads_unit, fmt) station_vec(i), s_vec(i), (coefs_mat(k, i), k = 1, critical_ntimesteps_lastrev)
                            write(unified_loads_unit, fmt) station_vec(i), s_vec(i), (coefs_mat(k, i), k = 1, critical_ntimesteps_lastrev)
                        end do 
                        fmt = '(2a15,'//trim(critical_ntimesteps_lastrev_char)//'a15)'
                        write(loads_unit, fmt) "station", "s", (trim(label2), i = 1, critical_ntimesteps_lastrev)
                        write(unified_loads_unit, fmt) "station", "s", (trim(label2), i = 1, critical_ntimesteps_lastrev)
                        fmt = '(f15.1, f15.4, '//trim(critical_ntimesteps_lastrev_char)//'f15.6)'
                        do i = 1, n_stations
                            write(loads_unit, fmt) station_vec(i), s_vec(i), (fm_mat(k, i), k = 1, critical_ntimesteps_lastrev)
                            write(unified_loads_unit, fmt) station_vec(i), s_vec(i), (fm_mat(k, i), k = 1, critical_ntimesteps_lastrev)
                        end do 
                    end if



                    read(csv_unit, '(a)', end = 205) line !reads next line
                end do 

                205 continue

                write(loads_unit, '(A)')
                write(unified_loads_unit, '(A)')

                deallocate(coefs_vec2)
                deallocate(time_vec2)
                deallocate(angle_vec2)
                deallocate(s_vec)
                deallocate(station_vec)
                deallocate(coefs_mat)
                deallocate(fm_mat)
                deallocate(short_angle_vec)
                deallocate(short_time_vec)
            end do

            write(unified_over_time_unit, '(A)')
            write(unified_over_time_unit, '(A)')
            write(unified_over_time_unit, '(A)')
            write(unified_stats_unit, '(A)')
            write(unified_stats_unit, '(A)')
            write(unified_stats_unit, '(A)')
            write(unified_loads_unit, '(A)')
            write(unified_loads_unit, '(A)')
            write(unified_loads_unit, '(A)')

            1003 continue

            close(csv_unit)
            close(over_time_unit)
            close(unified_over_time_unit)
            close(stats_unit)
            close(unified_stats_unit)
            close(loads_unit)
            close(unified_loads_unit)

            deallocate(coefs_vec)
            deallocate(time_vec)
            deallocate(angle_vec)
            deallocate(last_rev_vec)
        end subroutine post_process_csv

       !-----------general functions and subroutines----------------

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
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if
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

        subroutine copy_file(in_fname, out_fname, log_fil, operating_system) !subroutine to copy a file into another file
            character (len = *), intent(in)         ::  in_fname
            character (len = *), intent(in)         ::  out_fname
            character (len = *), intent(in)         ::  log_fil
            character (len = *), intent(in)         ::  operating_system
            character (len = 1000)                  ::  command
            character (len = 256)                   ::  resid = "bin\residual\copy_resid"

            call delete_file(out_fname)
            if (trim(operating_system) .eq. "windows") then
                command = 'copy "' // trim(in_fname) // '" "' // trim(out_fname) // '" > ' // trim(resid)
            else if (trim(operating_system) .eq. "linux") then
                command = 'cp "' // trim(in_fname) // '" "' // trim(out_fname) // '" > ' // trim(resid)
            end if
            call run_cmd(command, log_fil, .true.)
            call delete_file(resid)

        end subroutine copy_file

        subroutine get_files_on_dir(files, dir_name, log_fil, operating_system) !subroutine to list all the files on a directory
            character (len = 256), allocatable, intent(out) ::  files(:)
            character (len = *), intent(in)                 ::  log_fil
            character (len = *), intent(in)                 ::  dir_name
            character (len = *), intent(in)                 ::  operating_system
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

            if (trim(operating_system) .eq. "windows") then
                command = "dir " // trim(directory) // " /b > " // file_w_filenames
            else if (trim(operating_system) .eq. "linux") then
                command = "ls " // trim(directory) // " > " // file_w_filenames
            end if
            call run_cmd(trim(command), log_fil, .true.)

            open(newunit = unit_n, file = file_w_filenames, status = 'old', iostat = err_status, iomsg = err_msg)
            if (err_status .ne. 0) then; write(*,*) "---> ERROR -> " // trim(err_msg); call throw_warning(err_msg, log_fil); stop; end if

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

        subroutine clean_folder(folder, log_fil, operating_system)
            implicit none
            character (len = *), intent(in)     ::  folder
            character (len = *), intent(in)     ::  log_fil
            character (len = *), intent(in)     ::  operating_system
            character (len = 256), allocatable  ::  files(:)
            integer                             ::  i

            call get_files_on_dir(files, folder, log_fil, operating_system)

            do i = 1, size(files)
                call delete_file(trim(folder) // "\" // files(i))
            end do
        end subroutine clean_folder

end program autovsp