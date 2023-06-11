SUBROUTINE GVCALC(CHORD,BETA,R,BLDS,RAD,VEL,OMG,VSO,CL0,DCLDA,CLMIN,CLMAX,MCRIT,GAM,GAM_VEL,GAM_OMG,GAM_CH,GAM_BE, VA, VA_VEL, VA_OMG, VA_CH, VA_BE, VT, VT_VEL, VT_OMG, VT_CH, VT_BE, CL, CL_VEL, CL_OMG, CL_CH, CL_BE, STALL, LCONV)
    IMPLICIT REAL (A-H,M,O-Z)
    LOGICAL STALL, LCONV

    DATA PI / 3.14159265 /
    DATA EPS / 1.0E-6 /

    DATA MSQMAX    / 0.9    /

    U0A = 0.
    U0T = 0.

    UA     = VEL   + U0A
    UA_VEL = 1.0
    UA_U0A =         1.0

    UT     = OMG*R - U0T
    UT_OMG =     R
    UT_U0T =       - 1.0

    WZ = SQRT(UA**2 + UT**2)
    WZ_VEL = (UA/WZ)*UA_VEL
    WZ_OMG = (UT/WZ)*UT_OMG

    PSI1 = ATAN2(UA,UT)
    PSI2 = BETA + CL0/DCLDA
    PSI = MAX( PSI1 , PSI2 )

    LCONV = .FALSE.
    DO ITER = 1, 20
        COSP = COS(PSI)
        SINP = SIN(PSI)

        WA     = 0.5*UA     + 0.5*WZ    *SINP
        WA_PSI =              0.5*WZ    *COSP
        WA_VEL = 0.5*UA_VEL + 0.5*WZ_VEL*SINP
        WA_OMG =              0.5*WZ_OMG*SINP

        WT     = 0.5*UT     + 0.5*WZ    *COSP
        WT_PSI =            - 0.5*WZ    *SINP
        WT_VEL =              0.5*WZ_VEL*COSP
        WT_OMG = 0.5*UT_OMG + 0.5*WZ_OMG*COSP

        IF(WA.EQ.0.0) THEN
            F     = 1.0
            F_PSI = 0.
            F_VEL = 0.
            F_OMG = 0.

            ADW     = 0.
            ADW_PSI = 0.
            ADW_VEL = 0.
            ADW_OMG = 0.
        ELSE
            TSR = WT/WA * RAD/R
            TSR_PSI = (WT_PSI * RAD/R - TSR*WA_PSI)/WA
            TSR_VEL = (WT_VEL * RAD/R - TSR*WA_VEL)/WA
            TSR_OMG = (WT_OMG * RAD/R - TSR*WA_OMG)/WA

            FARG     = 0.5*BLDS*(1.0-R/RAD)*TSR
            FARG_TSR = 0.5*BLDS*(1.0-R/RAD)
            FARG = MIN( FARG , 20.0 )

            FEXP = EXP(-FARG)
            FEXP_TSR = -FEXP*FARG_TSR

            F = (2.0/PI) * ACOS(FEXP)
            F_TSR = -(2.0/PI) / SQRT(1.0 - FEXP**2) * FEXP_TSR

            F_PSI = F_TSR*TSR_PSI
            F_VEL = F_TSR*TSR_VEL
            F_OMG = F_TSR*TSR_OMG

            ADW     =  1.0    /TSR
            ADW_PSI = -TSR_PSI/TSR**2
            ADW_VEL = -TSR_VEL/TSR**2
            ADW_OMG = -TSR_OMG/TSR**2
        ENDIF

        VA     = WA     - UA
        VA_PSI = WA_PSI
        VA_VEL = WA_VEL - UA_VEL
        VA_OMG = WA_OMG

        VT     = UT     - WT
        VT_PSI =        - WT_PSI
        VT_VEL =        - WT_VEL
        VT_OMG = UT_OMG - WT_OMG

        QBI = 4.0/BLDS
        PIR = SQRT((PI*R)**2 + (QBI*RAD*ADW)**2)
        PIR_ADW = (QBI*RAD)**2*ADW/PIR
        PIR_PSI = PIR_ADW*ADW_PSI
        PIR_VEL = PIR_ADW*ADW_VEL
        PIR_OMG = PIR_ADW*ADW_OMG

        GAM     = QBI* F*VT                *PIR
        GAM_PSI = QBI*(F*VT_PSI + F_PSI*VT)*PIR + QBI*F*VT*PIR_PSI
        GAM_VEL = QBI*(F*VT_VEL + F_VEL*VT)*PIR + QBI*F*VT*PIR_VEL
        GAM_OMG = QBI*(F*VT_OMG + F_OMG*VT)*PIR + QBI*F*VT*PIR_OMG

        WSQ = WA**2 + WT**2
        W = SQRT(WSQ)
        W_PSI = (WA*WA_PSI + WT*WT_PSI)/W
        W_VEL = (WA*WA_VEL + WT*WT_VEL)/W
        W_OMG = (WA*WA_OMG + WT*WT_OMG)/W

        A = BETA - ATAN2(WA,WT)
        A_PSI = (-WT*WA_PSI + WA*WT_PSI)/WSQ
        A_VEL = (-WT*WA_VEL + WA*WT_VEL)/WSQ
        A_OMG = (-WT*WA_OMG + WA*WT_OMG)/WSQ
        A_BE  = 1.0

        MSQ = WSQ / VSO**2
        MSQ_PSI = 2.0*W*W_PSI / VSO**2
        MSQ_VEL = 2.0*W*W_VEL / VSO**2
        MSQ_OMG = 2.0*W*W_OMG / VSO**2
        IF(MSQ .GT. MSQMAX) THEN
            MSQ = MSQMAX
            MSQ_PSI = 0.
            MSQ_VEL = 0.
            MSQ_OMG = 0.
        ENDIF

        PG = 1.0 / SQRT(1.0 - MSQ)
        PG_MSQ = 0.5*PG / (1.0 - MSQ)

        PG_PSI = PG_MSQ*MSQ_PSI
        PG_VEL = PG_MSQ*MSQ_VEL
        PG_OMG = PG_MSQ*MSQ_OMG

        CL     = (DCLDA*A + CL0)*PG
        CL_PSI =  DCLDA*A_PSI   *PG + (DCLDA*A + CL0)*PG_PSI 
        CL_VEL =  DCLDA*A_VEL   *PG + (DCLDA*A + CL0)*PG_VEL 
        CL_OMG =  DCLDA*A_OMG   *PG + (DCLDA*A + CL0)*PG_OMG 
        CL_BE  =  DCLDA*A_BE    *PG   

        STALL = .FALSE.
        IF    (CL.GT.CLMAX) THEN
            STALL = .TRUE.
            ACL0 = CL0/DCLDA

            CL   =  CLMAX*COS(A-ACL0)
            CL_A = -CLMAX*SIN(A-ACL0)

            CL_PSI = CL_A*A_PSI
            CL_VEL = CL_A*A_VEL
            CL_OMG = CL_A*A_OMG
            CL_BE  = CL_A*A_BE
        ELSEIF(CL.LT.CLMIN) THEN
            STALL = .TRUE.
            ACL0 = CL0/DCLDA

            CL   =  CLMIN*COS(A-ACL0)
            CL_A = -CLMIN*SIN(A-ACL0)

            CL_PSI = CL_A*A_PSI
            CL_VEL = CL_A*A_VEL
            CL_OMG = CL_A*A_OMG
            CL_BE  = CL_A*A_BE
        ENDIF


        RES     = GAM     - 0.5*CHORD* CL*W
        RES_PSI = GAM_PSI - 0.5*CHORD*(CL*W_PSI + CL_PSI*W)
        RES_VEL = GAM_VEL - 0.5*CHORD*(CL*W_VEL + CL_VEL*W)
        RES_OMG = GAM_OMG - 0.5*CHORD*(CL*W_OMG + CL_OMG*W)
        RES_CH  =         - 0.5*       CL*W
        RES_BE  =         - 0.5*CHORD*            CL_BE *W

        DPSI = -RES/RES_PSI
        DPSI = MAX( -0.1 , MIN ( 0.1 , DPSI ) )

        IF(ABS(DPSI) .LT. EPS) THEN
            LCONV = .TRUE.
            GO TO 50
        ENDIF


        PSI = PSI + DPSI
    ENDDO
    WRITE(*,*) 'GVCALC: Not converged.  Res a CL =', RES, A, CL

    50   CONTINUE
    PSI_VEL = -RES_VEL/RES_PSI
    PSI_OMG = -RES_OMG/RES_PSI
    PSI_CH  = -RES_CH /RES_PSI
    PSI_BE  = -RES_BE /RES_PSI


    GAM_VEL = GAM_VEL + GAM_PSI*PSI_VEL
    GAM_OMG = GAM_OMG + GAM_PSI*PSI_OMG
    GAM_CH  =           GAM_PSI*PSI_CH
    GAM_BE  =           GAM_PSI*PSI_BE

    VA_VEL = VA_VEL + VA_PSI*PSI_VEL
    VA_OMG = VA_OMG + VA_PSI*PSI_OMG
    VA_CH  =          VA_PSI*PSI_CH
    VA_BE  =          VA_PSI*PSI_BE

    VT_VEL = VT_VEL + VT_PSI*PSI_VEL
    VT_OMG = VT_OMG + VT_PSI*PSI_OMG
    VT_CH  =          VT_PSI*PSI_CH
    VT_BE  =          VT_PSI*PSI_BE

    CL_VEL = CL_VEL + CL_PSI*PSI_VEL
    CL_OMG = CL_OMG + CL_PSI*PSI_OMG
    CL_CH  =          CL_PSI*PSI_CH
    CL_BE  = CL_BE  + CL_PSI*PSI_BE

    RETURN
END ! GVCALC

