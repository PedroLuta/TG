c***********************************************************************
c    Module:  io.f
c 
c    Copyright (C) 2005 Mark Drela 
c 
c    This program is free software; you can redistribute it and/or modify
c    it under the terms of the GNU General Public License as published by
c    the Free Software Foundation; either version 2 of the License, or
c    (at your option) any later version.

c    This program is distributed in the hope that it will be useful,
c    but WITHOUT ANY WARRANTY; without even the implied warranty of
c    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c    GNU General Public License for more details.

c    You should have received a copy of the GNU General Public License
c    along with this program; if not, write to the Free Software
c    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
c***********************************************************************

      SUBROUTINE FREAD(LU,LINE,ILINE,IERR, CVAL)
            CHARACTER*(*) LINE, CVAL

            10   CONTINUE
            ILINE = ILINE + 1

            READ(LU,1000,END=90) LINE
            1000 FORMAT(A)

            IF(INDEX('#!',LINE(1:1)) .NE. 0) GO TO 10
            IF(LINE.EQ.' ') GO TO 10

            KB = INDEX(LINE,'!') - 1
            IF(KB.LE.0) KB = LEN(LINE)

            CVAL = LINE(1:KB)
            IERR = 0
            RETURN

            80   IERR = 1
            RETURN

            90   IERR = -1
            RETURN
      END

      SUBROUTINE RREAD(LU,LINE,ILINE,IERR, NVAL,VAL)
            CHARACTER*(*) LINE
            REAL VAL(*)

            LOGICAL ERROR

            10 CONTINUE
            ILINE = ILINE + 1

            READ(LU,1000,END = 90) LINE
            1000 FORMAT(A)

            IF(INDEX('#!',LINE(1:1)) .NE. 0) GO TO 10
            IF(LINE.EQ.' ') GO TO 10

            KB = INDEX(LINE,'!') - 1
            IF(KB.LE.0) KB = LEN(LINE)

            CALL GETFLT(LINE(1:KB),VAL,NVAL,ERROR)
            IF(ERROR) THEN
                  IERR = 1
            ELSE
                  IERR = 0
            ENDIF
            RETURN
            90   IERR = -1
            RETURN
      END

      SUBROUTINE IREAD(LU,LINE,ILINE,IERR, NVAL,IVAL)
            CHARACTER*(*) LINE
            INTEGER IVAL(*)

            LOGICAL ERROR

            10   CONTINUE
            ILINE = ILINE + 1

            READ(LU,1000,END=90) LINE
            1000 FORMAT(A)

            IF(INDEX('#!',LINE(1:1)) .NE. 0) GO TO 10
            IF(LINE.EQ.' ') GO TO 10

            KB = INDEX(LINE,'!') - 1
            IF(KB.LE.0) KB = LEN(LINE)

            CALL GETINT(LINE(1:KB),IVAL,NVAL,ERROR)
            IF(ERROR) THEN
                  IERR = 1
            ELSE
                  IERR = 0
            ENDIF
            RETURN

            90   IERR = -1
            RETURN
      END

      SUBROUTINE GETFLT(INPUT,RNUM,NR,ERROR)
            !----------------------------------------------------------------
            !     Parses character string INPUT into an array
            !     of real numbers returned in RNUM(1..NR).
            !
            !     Will attempt to extract no more than NR numbers,
            !     unless NR = 0, in which case all numbers present 
            !     in INPUT will be extracted.
            !
            !     NR returns how many numbers were actually extracted.
            !----------------------------------------------------------------
            CHARACTER*(*) INPUT
            REAL RNUM(*)
            LOGICAL ERROR
            CHARACTER*1 TAB

            TAB = CHAR(9)

            !---- number of characters to be examined
            ILEN = LEN(INPUT)

            !---- ignore everything after a "!" character
            K = INDEX(INPUT,'!')
            IF(K.GT.0) ILEN = K-1

            !---- set limit on numbers to be read
            NINP = NR
            IF(NINP.EQ.0) NINP = ILEN/2 + 1

            NR = 0

            IF(ILEN.EQ.0) RETURN

            !---- extract numbers
            N = 0
            K = 1
            DO 10 IPASS=1, ILEN
                  !------ find next blank or tab (pretend there's one after the end of the string)
                  KBLK = INDEX(INPUT(K:ILEN),' ') + K - 1
                  KTAB = INDEX(INPUT(K:ILEN),TAB) + K - 1

                  IF(KBLK.EQ.K-1) KBLK = ILEN + 1
                  IF(KTAB.EQ.K-1) KTAB = ILEN + 1
                        
                  KSPACE = MIN( KBLK , KTAB )
                        
                  IF(KSPACE.EQ.K) THEN
                        !------- just skip this space
                        K = K+1
                        GO TO 9
                  ENDIF

                  !------ also find next comma
                  KCOMMA = INDEX(INPUT(K:ILEN),',') + K - 1
                  IF(KCOMMA.EQ.K-1) KCOMMA = ILEN + 1

                  !------ space is farther down, so we ran into something...
                  N = N+1

                  !------ bug out early if no more numbers are to be read
                  IF(N.GT.NINP) GO TO 11

                  !------ set ending delimiter position for this number
                  KDELIM = MIN(KSPACE,KCOMMA)

                  IF(K.EQ.KDELIM) THEN
                        !------- nothing but a comma... just keep looking
                        K = K+1
                        GO TO 9
                  ENDIF

                  !------ whatever we have, it is in substring K:KEND
                  KEND = KDELIM - 1
                  READ(INPUT(K:KEND),*,ERR=20) RNUM(N)
                  NR = N

                  !------ keep looking after delimiter
                  K = KDELIM + 1

                  9     IF(K.GE.ILEN) GO TO 11
                  10    CONTINUE

                  !---- normal return
                  11   CONTINUE
                  ERROR = .FALSE.
                  RETURN

                  !---- bzzzt !!!
                  20   CONTINUE
                  !   WRITE(*,*) 'GETFLT: List-directed read error.'
                  ERROR = .TRUE.
                  RETURN
            END DO
      END ! GETFLT

      SUBROUTINE GETINT(INPUT,A,N,ERROR)
            !----------------------------------------------------------
            !     Parses character string INPUT into an array
            !     of integer numbers returned in A(1...N)

            !     Will attempt to extract no more than N numbers, 
            !     unless N = 0, in which case all numbers present
            !     in INPUT will be extracted.

            !     N returns how many numbers were actually extracted.
            !----------------------------------------------------------
            CHARACTER*(*) INPUT
            INTEGER A(*)
            LOGICAL ERROR
            CHARACTER*130 REC

            !---- only first 128 characters in INPUT will be parsed
            ILEN = MIN( LEN(INPUT) , 128 )
            ILENP = ILEN + 2

            !---- put input into local work string (which will be munched)
            REC(1:ILENP) = INPUT(1:ILEN) // ' ,'

            !---- ignore everything after a "!" character
            K = INDEX(REC,'!')
            IF(K.GT.0) REC(1:ILEN) = REC(1:K-1)

            NINP = N

            !---- count up how many numbers are to be extracted
            N = 0
            K = 1
            DO 10 IPASS=1, ILEN
                  !------ search for next space or comma starting with current index K
                  KSPACE = INDEX(REC(K:ILENP),' ') + K - 1
                  KCOMMA = INDEX(REC(K:ILENP),',') + K - 1

                  IF(K.EQ.KSPACE) THEN
                        !------- just skip this space
                        K = K+1
                        GO TO 9
                  ENDIF

                  IF(K.EQ.KCOMMA) THEN
                        !------- comma found.. increment number count and keep looking
                        N = N+1
                        K = K+1
                        GO TO 9
                  ENDIF

                  !------ neither space nor comma found, so we ran into a number...
                  !-    ...increment number counter and keep looking after next space or comma
                  N = N+1
                  K = MIN(KSPACE,KCOMMA) + 1

                  9     IF(K.GE.ILEN) GO TO 11
                  10   CONTINUE

                  !---- decide on how many numbers to read, and go ahead and read them
                  11   IF(NINP.GT.0) N = MIN( N, NINP )
                  READ(REC(1:ILEN),*,ERR=20) (A(I),I=1,N)
                  ERROR = .FALSE.
                  RETURN

                  !---- bzzzt !!!
                  20   CONTINUE
                  !   WRITE(*,*) 'GETINT: String-to-integer conversion error.'
                  N = 0
                  ERROR = .TRUE.
                  RETURN
      END

      SUBROUTINE PPARSE(LINE,P1,P2,NP,IERR)
            !----------------------------------------------------
            !     Extracts sequence parameters from string LINE,
            !     which must have one of the following formats:
            
            !       P1
            !       P1,P2
            !       P1,P2,DP
            !       P1,P2/NP
            !----------------------------------------------------
            CHARACTER*(*) LINE

            N = LEN(LINE)

            KCOMMA1 = INDEX(LINE,',')
            KSLASH  = INDEX(LINE,'/')
            IF(KCOMMA1.GT.0 .AND. KSLASH.EQ.0) THEN
                  K = KCOMMA1 + 1
                  KCOMMA2 = INDEX(LINE(K:N),',') + K - 1
            ELSE
                  KCOMMA2 = 0
            ENDIF


            IERR = 0

            IF(KCOMMA1.EQ.0) THEN
                  !----- only a single parameter is present
                  READ(LINE,*,ERR=80,END=90) P1
                  P2 = P1
                  NP = 1
                  RETURN

            ELSE
                  !----- two or three sequence parameters
                  K = KCOMMA1 - 1
                  READ(LINE(1:K),*,ERR=80,END=90) P1

                  IF(KCOMMA2.EQ.0 .AND. KSLASH.EQ.0) THEN
                        !------ no step size parameter given
                        K = KCOMMA1 + 1
                        READ(LINE(K:N),*,ERR=80,END=90) P2
                        IF(P1.EQ.P2) THEN
                              !------- same two endpoints are given -- zero interval with no steps
                              NP = 1
                        ELSE
                              !------- only two endpoints are given -- set default number of steps
                              NP = 5
                        ENDIF
                        RETURN

                  ELSEIF(KCOMMA2.NE.0) THEN
                        !------ interval step size is given
                        KA = KCOMMA1 + 1
                        KB = KCOMMA2 - 1
                        READ(LINE(KA:KB),*,ERR=80,END=90) P2
                        K = KCOMMA2 + 1
                        READ(LINE(K:N),*,ERR=80,END=90) PDEL
                        RNUM = (P2-P1)/PDEL
                        NP = INT( ABS(RNUM) + 1.49)
                        RETURN
         
                  ELSEIF(KSLASH.NE.0) THEN
                        KA = KCOMMA1 + 1
                        KB = KSLASH  - 1

                        READ(LINE(KA:KB),*,ERR=80,END=90) P2
                        K = KSLASH + 1
                        READ(LINE(K:N),*,ERR=80,END=90) RNUM
                        NP = INT( ABS(RNUM) + 0.01 )
                        RETURN
                  ENDIF

            ENDIF

            80   CONTINUE
            IERR = +1
            RETURN

            90   CONTINUE
            IERR = -1
            RETURN

      END ! PPARSE

      SUBROUTINE GETARG0(IARG,ARG)
            !------------------------------------------------
            !     Same as GETARG, but...

            !     ...in the case of Intel Fortran, this one
            !     doesn't barf if there's no Unix argument 
            !      (just returns blank string instead)
            !------------------------------------------------
            CHARACTER*(*) ARG

            NARG = IARGC()
            IF(NARG.GE.IARG) THEN
                  CALL GETARG(IARG,ARG)
            ELSE
                  ARG = ' '
            ENDIF

            RETURN
      END ! GETARG0

      SUBROUTINE STRIP(STRING,NS)
            !---------------------------------------------------
            !     Strips all blanks off string STRING
            !     and returns non-blank part in STRING.
            !     Returns length of the non-blank part in NS.
            !---------------------------------------------------
            CHARACTER*(*) STRING
            N = LEN(STRING)

            !---- find last non-blank character
            DO NS = N, 1, -1
                  IF(STRING(NS:NS).NE.' ') GO TO 11
            ENDDO
            NS = 0
            RETURN

            !---- start blank-stripping loop
            11 CONTINUE

            !---- find first blank character before NS
            DO K0 = 1, NS
                  IF(STRING(K0:K0).EQ.' ') GO TO 21
            ENDDO
            RETURN
            21 CONTINUE

            !---- find first non-blank character following first blank
            DO K1 = K0+1, NS
                  IF(STRING(K1:K1).NE.' ') GO TO 31
            ENDDO
            31 CONTINUE

            !---- shift STRING to remove blank segment
            NSDEL = K1 - K0
            STRING(K0:NS-NSDEL) = STRING(K1:NS)
            NS = NS - NSDEL

            GO TO 11

      END ! STRIP
