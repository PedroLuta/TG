SUBROUTINE MOTORQ(OMEGA, VOLT, IMOTYPE, PARMOT,NMPAR, Q, Q_OMEGA, Q_VOLT, I, I_OMEGA, I_VOLT)

    REAL PARMOT(*)
    REAL I, I_OMEGA, I_VOLT

    REAL KVRPM, KQRPM, KVRAD, KQRAD
    DATA PI /3.1415926/
    DATA EPS /1.0E-6/

    IF(IMOTYPE.EQ.1) THEN
        IF(NMPAR.LT.3) THEN
            WRITE(*,*) 'MOTORQ: Motor model 1 needs  3  parameters.', '  Number passed in:', IMOTYPE
            STOP
        ENDIF

        RMOTOR = PARMOT(1)    ! R   (Ohms)      motor resistance
        ZLOADI = PARMOT(2)    ! Io  (Amps)      zero-load current
        KVRPM  = PARMOT(3)    ! Kv  (rpm/Volt)  motor constant

        KVRAD = KVRPM * PI/30.0
        KQRAD = KVRAD

        VM       = OMEGA/KVRAD
        VM_OMEGA = 1.0  /KVRAD

        I       = (VOLT - VM      )/RMOTOR
        I_OMEGA =       - VM_OMEGA /RMOTOR
        I_VOLT  =  1.0             /RMOTOR

        Q       = (I       - ZLOADI)/KQRAD
        Q_OMEGA =  I_OMEGA          /KQRAD
        Q_VOLT  =  I_VOLT           /KQRAD

    ELSEIF(IMOTYPE.EQ.2) THEN
        IF(NMPAR.LT.3) THEN
            WRITE(*,*) 'MOTORQ: Motor model 2 needs at least 3 parameters.', '  Number passed in:', IMOTYPE
            STOP
        ENDIF

        RMOTOR0 = PARMOT(1)    ! R0  (Ohms)      motor resistance
        ZLOADI0 = PARMOT(2)    ! Io0 (Amps)      zero-load current
        KVRPM   = PARMOT(3)    ! Kv  (rpm/Volt)  motor constant
        KQRPM   = PARMOT(4)    ! Kq  (rpm/Volt)  motor constant
        TAU     = PARMOT(5)    ! tau
        ZLOADI1 = PARMOT(6)    ! Io1
        ZLOADI2 = PARMOT(7)    ! Io2
        RMOTOR2 = PARMOT(8)    ! R2  (Ohms/Amp^2)

        IF(KQRPM .EQ. 0.0) KQRPM = KVRPM

        KVRAD = KVRPM * PI/30.0
        KQRAD = KQRPM * PI/30.0

        VM       = (1.0 + TAU*OMEGA    )*OMEGA/KVRAD
        VM_OMEGA = (1.0 + TAU*OMEGA*2.0)      /KVRAD

        I = (VOLT - VM)/RMOTOR0
        DO ITER = 1, 10
            RES = I*(RMOTOR0 + RMOTOR2*I**2) + VM - VOLT
            RES_I = RMOTOR0 + 3.0*RMOTOR2*I**2
            I = I - RES/RES_I
            IF(ABS(RES) .LT. EPS*MAX(1.0,ABS(VOLT))) GO TO 11
        ENDDO
        WRITE(*,*) 'MOTOR: Current convergence failed'

        11    CONTINUE
        RES_OMEGA = VM_OMEGA
        RES_VOLT  = -1.0
        I_OMEGA = -RES_OMEGA/RES_I
        I_VOLT  = -RES_VOLT /RES_I

        ZLOADI       = ZLOADI0 + ZLOADI1*OMEGA + ZLOADI2*OMEGA**2
        ZLOADI_OMEGA =           ZLOADI1       + ZLOADI2*OMEGA * 2.0

        Q       = (I       - ZLOADI      ) / KQRAD
        Q_OMEGA = (I_OMEGA - ZLOADI_OMEGA) / KQRAD
        Q_VOLT  =  I_VOLT                  / KQRAD

    ELSE

        WRITE(*,*) 'MOTORQ: Undefined motor type index:', IMOTYPE
        STOP
    ENDIF

    RETURN
END ! MOTORQ



SUBROUTINE VOLTM(OMEGA, Q, IMOTYPE, PARMOT,NMPAR, VOLT, VOLT_OMEGA, VOLT_Q, AMPS, AMPS_OMEGA, AMPS_Q)

    REAL PARMOT(*)
    REAL KVRPM, KQRPM, KVRAD, KQRAD

    DATA PI /3.1415926/
    DATA EPS /1.0E-6/

    IF(IMOTYPE.EQ.1) THEN
        RMOTOR = PARMOT(1)    ! R   (Ohms)      motor resistance
        ZLOADI = PARMOT(2)    ! Io  (Amps)      zero-load current
        KVRPM  = PARMOT(3)    ! Kv  (rpm/Volt)  motor constant
        KVRAD = KVRPM*PI/30.0
        KQRAD = KVRAD
        AMPS = Q*KQRAD + ZLOADI
        VOLT = AMPS*RMOTOR + OMEGA/KVRAD
    ELSEIF(IMOTYPE.EQ.2) THEN
        RMOTOR = PARMOT(1)    ! R   (Ohms)      motor resistance
        ZLOADI = PARMOT(2)    ! Io  (Amps)      zero-load current
        KVRPM  = PARMOT(3)    ! Kv  (rpm/Volt)  motor constant
        KVRAD = KVRPM*PI/30.0
        KQRAD = KVRAD
        AMPS = Q*KQRAD + ZLOADI
        VOLT = AMPS*RMOTOR + OMEGA/KVRAD
    ELSE
        VOLT = 1.0
    ENDIF

    DO ITER = 1, 20
        CALL MOTORQ(OMEGA,VOLT, IMOTYPE, PARMOT,NMPAR, QM,QM_OMEGA,QM_VOLT, AM,AM_OMEGA,AM_VOLT )

        RES      = QM - Q
        RES_VOLT = QM_VOLT

        DVOLT = -RES/RES_VOLT

        IF(ABS(DVOLT) .LT. EPS*MAX(1.0,ABS(VOLT))) GO TO 10

        VOLT = VOLT + DVOLT
    ENDDO
    WRITE(*,*) '** VOLTM: Voltage convergence failed. Res =', RES

    10   CONTINUE
    RES_OMEGA = QM_OMEGA
    RES_Q     = -1.0

    VOLT_OMEGA = -RES_OMEGA / RES_VOLT
    VOLT_Q     = -RES_Q     / RES_VOLT

    AMPS_OMEGA = AM_VOLT*VOLT_OMEGA + AM_OMEGA
    AMPS_Q     = AM_VOLT*VOLT_Q

    RETURN
END ! VOLTM

