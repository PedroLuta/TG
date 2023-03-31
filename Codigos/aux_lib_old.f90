MODULE aux_lib
    IMPLICIT NONE
    CONTAINS
!--------------------------GENERAL AUXILIARY-----------------------------------        
        SUBROUTINE DELETE_FILE(FNAM)
            ! THIS SUBROUTINE DELETES A FILE IF IT DETECTS THAT IT EXISTS, OTHERWISE IT DOES NOTHING
            IMPLICIT NONE
            CHARACTER (LEN = 256), INTENT(IN)   ::  FNAM
            CHARACTER (LEN = 256)               ::  cwd, FILE
            INTEGER                             ::  STAT
            INTEGER                             ::  UNIT_N
            IF (FNAM(2:3) .NE. ":\") THEN
                CALL GETCWD(CWD)
                FILE = TRIM(CWD)//"\"//FNAM
            ELSE
                FILE = FNAM
            END IF
            OPEN(newunit = unit_n, IOSTAT = STAT, FILE = TRIM(FILE), STATUS = 'OLD')
            IF (STAT .EQ. 0) THEN
                CLOSE(UNIT_N, STATUS = 'DELETE')
            END IF
        END SUBROUTINE
        
        SUBROUTINE CHECK_ERROR(ERR_STATUS, ERR_MSG)
            ! THIS SUBROUTINE CHECKS ERR_STATUS, IF IT IS DIFFERENT THAN ZERO, PRINTS ERR_MSG AND STOPS THE CODE
            IMPLICIT NONE 
            INTEGER, INTENT(IN)         :: ERR_STATUS
            CHARACTER*256, INTENT(IN)   :: ERR_MSG
            IF (ERR_STATUS .NE. 0) THEN
                WRITE (*,*) "Error, " // ERR_MSG
                STOP
            END IF
        END SUBROUTINE 

        SUBROUTINE COUNT_PARAMETERS_LINE(LINE, SEPARATOR, N_PARAMS, NO_REPEAT)
            ! THIS FUNCION RETURNS THE NUMBER OF PARAMETERS ON THE LINE
            IMPLICIT NONE
            CHARACTER (LEN = 10000), INTENT(IN) ::  LINE
            CHARACTER (LEN = 256), INTENT(IN)   ::  SEPARATOR
            LOGICAL, INTENT(IN)                 ::  NO_REPEAT
            INTEGER                             ::  N_PARAMS, I
            LOGICAL                             ::  AUX_BOOL
            AUX_BOOL = .TRUE.
            N_PARAMS = 0
            DO I = 1, LEN_TRIM(LINE)
                IF ((LINE(I:I) .EQ. TRIM(SEPARATOR)) .AND. AUX_BOOL) THEN
                    N_PARAMS = N_PARAMS + 1
                    IF (NO_REPEAT) THEN
                        AUX_BOOL = .FALSE.
                    END IF
                ELSE
                    AUX_BOOL = .TRUE.
                END IF
            END DO
            N_PARAMS = N_PARAMS + 1
        END SUBROUTINE COUNT_PARAMETERS_LINE_NOREPEAT

        SUBROUTINE GET_FILES_ON_DIR(FILES, DIR_NAME)
            character (len = 256), allocatable, intent(out) ::  FILES(:)
            character (len = 256), intent(in)               ::  DIR_NAME
            INTEGER                                         ::  N_LINES, I
            CHARACTER (LEN = 256)                           ::  FILE_W_FILENAMES = "files.txt"
            INTEGER                                         ::  UNIT_N
            INTEGER                                         ::  ERR_STATUS
            CHARACTER (LEN = 256)                           ::  ERR_MSG
            CHARACTER (LEN = 256)                           ::  CWD, COMAND, DIRECTORY, DUMCHAR
            CALL DELETE_FILE(FILE_W_FILENAMES)
            IF (DIR_NAME(2:3) .NE. ":\") THEN
                CALL GETCWD(CWD)
                DIRECTORY = TRIM(CWD)//"\"//DIR_NAME
            ELSE
                DIRECTORY = DIR_NAME
            END IF

            COMAND = "dir " // TRIM(DIRECTORY) // " /b > " // FILE_W_FILENAMES
            CALL SYSTEM(TRIM(COMAND))

            OPEN(newunit = unit_n, FILE = FILE_W_FILENAMES, STATUS = 'OLD', IOSTAT = ERR_STATUS, IOMSG = ERR_MSG)
            CALL CHECK_ERROR(ERR_STATUS, ERR_MSG)

            N_LINES = 0
            DO
                READ(UNIT_N, *, END = 200) DUMCHAR
                N_LINES = N_LINES + 1
            END DO
            200 CONTINUE
            REWIND UNIT_N

            ALLOCATE(FILES(N_LINES))

            DO I = 1, N_LINES
                READ(UNIT_N, '(A)', END = 201) FILES(I)
            END DO
            201 CONTINUE
            CLOSE(UNIT_N)
            CALL DELETE_FILE(FILE_W_FILENAMES)
        END SUBROUTINE

        SUBROUTINE CHANGE_ON_FILE(IN_FNAME, OUT_FNAME, QUEST, CHANGE, FIRST_FLAG)
            character (len = 256), INTENT(IN)       ::  in_fname
            character (len = 256), INTENT(IN)       ::  out_fname
            character (len = 256), INTENT(IN)       ::  quest
            character (len = 10000), INTENT(IN)     ::  change
            LOGICAL, INTENT(IN)                     ::  FIRST_FLAG
            LOGICAL                                 ::  DONE
            character (len = 10000)                 ::  line, placeholder
            integer                                 ::  i
            INTEGER                                 ::  len_orig, len_changed, delta, len_line
            integer                                 ::  in_unit, out_unit
            INTEGER                                 ::  ERR_STATUS
            CHARACTER*256                           ::  ERR_MSG

            len_orig = LEN_TRIM(quest)
            len_changed = LEN_TRIM(change)
            delta = len_changed - len_orig

            DONE = .FALSE.

            open(newunit = in_unit, file = in_fname, status = "OLD", IOSTAT = ERR_STATUS, IOMSG = ERR_MSG)
            CALL CHECK_ERROR(ERR_STATUS, ERR_MSG)
            open(newunit = out_unit, file = out_fname, status = "UNKNOWN", IOSTAT = ERR_STATUS, IOMSG = ERR_MSG)
            !open(newunit = out_unit, file = out_fname, status = "NEW", IOSTAT = ERR_STATUS, IOMSG = ERR_MSG)
            CALL CHECK_ERROR(ERR_STATUS, ERR_MSG)
            do
                read(in_unit, '(A)', end = 200) line
                do i = 1, LEN_TRIM(line)
                    if ((line(i:i + len_orig - 1) .eq. trim(quest)) .AND. (.NOT. DONE)) then
                        len_line = len_trim(line)
                        placeholder = line(i + len_orig:LEN_TRIM(line))
                        line(i:len_trim(line)) = ""
                        line(i:i + len_changed - 1) = trim(change)
                        line(i + len_changed:len_line + delta) = trim(placeholder)
                        IF (FIRST_FLAG) THEN
                            DONE = .true.
                        END IF
                    end if
                end do
                write(out_unit, '(A)') trim(line)
            end do
            200 continue
            close(in_unit)
            close(out_unit)
        end SUBROUTINE CHANGE_ON_FILE

!--------------------------XFOIL AUXILIARY-----------------------------------
        subroutine run_xfoil(results)
            implicit none
            character (len = 256)           ::  system_comand
            character (len = 256)           ::  out_fil
            INTEGER                         ::  ERR_STATUS
            CHARACTER (len = 256)           ::  ERR_IOMSG
            CHARACTER (LEN = 256)           ::  LINE
            integer                         ::  out_unit
            integer                         ::  i, j, k
            integer                         ::  n_outputs
            real, allocatable, intent(out)  ::  results(:,:)

            system_comand = "xfoil.exe < def_xfoil.inp"
            out_fil = "xfoil_output.txt"

            CALL DELETE_FILE_REL(out_fil)
            CALL SYSTEM(system_comand)

            OPEN(UNIT = out_unit, FILE = out_fil, STATUS = 'OLD', IOSTAT = ERR_STATUS, IOMSG = ERR_IOMSG)
            CALL CHECK_ERROR(ERR_STATUS, ERR_IOMSG)

            i = 1
            n_outputs = 0
            DO 
                READ(out_unit, '(A)', END = 200) 
                if (i .gt. 12) then 
                    n_outputs = n_outputs + 1
                end if
                i = i + 1
            END DO
            200 continue

            ALLOCATE(results(7, n_outputs))
            rewind out_unit

            i = 1
            j = 1
            DO 
                READ(out_unit, '(A)', END = 201) LINE
                if (i .gt. 12) then 
                    read(line, *) (results(k, j), k = 1, 7)
                    j = j + 1
                end if
                i = i + 1
            END DO
            201 continue

            CLOSE(out_unit)

            CALL DELETE_FILE(out_fil)

        end subroutine

        ! subroutine get_n_xfoil_outputs(n)
        !     integer, intent(out)    ::  n
        !     integer                 ::  i, fnum = 20
        !     character (len = 256)   ::  fname = "xfoil_output.txt"
        !     INTEGER                 ::  ERR_STATUS
        !     CHARACTER*256           ::  ERR_IOMSG

        !     OPEN(UNIT = fnum, FILE = fname, STATUS = 'OLD', IOSTAT = ERR_STATUS, IOMSG = ERR_IOMSG)
        !     CALL CHECK_ERROR(ERR_STATUS, ERR_IOMSG)
        !     rewind fnum
        !     i = 1
        !     n = 0
        !     DO 
        !         READ(fnum, '(A)', END = 200) 
        !         if (i .gt. 12) then 
        !             n = n + 1
        !         end if
        !         i = i + 1
        !     END DO
        !     200 continue
        !     CLOSE(fnum)
        ! end subroutine

        ! subroutine read_xfoil_output(results)
        !     character (len = 256)           ::  outname = "xfoil_output.txt"
        !     integer                         ::  outnum = 21
        !     INTEGER                         ::  ERR_STATUS
        !     CHARACTER (LEN = 256)           ::  ERR_IOMSG
        !     CHARACTER (LEN = 256)           ::  LINE
        !     integer                         ::  i, j, k, n_points
        !     real, allocatable, intent(out)  ::  results(:,:)

        !     call get_n_xfoil_outputs(n_points)

        !     OPEN(UNIT = outnum, FILE = outname, STATUS = 'OLD', IOSTAT = ERR_STATUS, IOMSG = ERR_IOMSG)
        !     CALL CHECK_ERROR(ERR_STATUS, ERR_IOMSG)
        !     rewind outnum

        !     ALLOCATE(results(7, n_points))

        !     i = 1
        !     j = 1
        !     DO 
        !         READ(outnum, '(A)', END = 201) LINE
        !         if (i .gt. 12) then 
        !             read(line, *) (results(k, j), k = 1, 7)
        !             j = j + 1
        !         end if
        !         i = i + 1
        !     END DO
        !     201 continue

        !     CLOSE(outnum)

        !     CALL DELETE_FILE_REL(outname)
        ! end subroutine
END MODULE aux_lib