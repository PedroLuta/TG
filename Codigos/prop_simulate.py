# from importing import *
import math
import numpy as np
import xfoil_interface
import pandas as pd

euler = np.e
pi = np.pi

def qprop_fixed_pitch(vi, radps, Blades, R, r_vector, Beta_dist, chord_dist, airfoil = 'airfoils\\airfoil.txt', rho = 1.225, dvisc = 1.8/100000, alphas = [-5, 10, 1]):
    #CHORD DISTRIBUTION TAKEN AS INPUT
    a1, a2, astep = alphas[0], alphas[1], alphas[2]
    kvisc = dvisc/rho

    dT_vector = []
    dQ_vector = []
    Re_vector = []
    WA_vector = []
    WT_vector = []
    Cl_vector = []
    Cd_vector = []

    for i in range(len(r_vector)):
        rr = r_vector[i]
        Beta = Beta_dist[i]
        chord = chord_dist[i]

        Vr = radps*rr
        V = ((Vr**2)+(vi**2))**0.5
        Re = ((V*chord)/kvisc) 

        alpha_c, Cl_c, Cd_c = xfoil_interface.get_curve_com_default(Re, a1, a2, astep, afile = airfoil)
        WA, WT, Cl, Cd = induction_qprop_fixed_pitch(radps, rr, Blades, alpha_c, Cl_c, Cd_c, Beta, R, chord, vi)
        W = (WA**2 + WT**2)**0.5
        phi = math.atan(WA/WT)
        dT = (rho*Blades*chord)*(W**2)*(Cl*math.cos(phi) - Cd*math.sin(phi))/2
        dQ = (rho*Blades*chord*rr)*(W**2)*(Cl*math.sin(phi) + Cd*math.cos(phi))/2


        dQ_vector.append(dQ)
        dT_vector.append(dT)
        Re_vector.append(Re)
        WA_vector.append(WA)
        WT_vector.append(WT)
        Cl_vector.append(Cl)
        Cd_vector.append(Cd)
    return dT_vector, dQ_vector, r_vector, Re_vector, WA_vector, Cl_vector, Cd_vector

def momentum_Ftip_fixed_pitch(vi, radps, Blades, R, r_vector, Beta_dist, chord_dist, airfoil = 'airfoils\\airfoil.txt', rho = 1.225, dvisc = 1.8/100000, alphas = [-5, 10, 1]):
    a1, a2, astep = alphas[0], alphas[1], alphas[2]
    kvisc = dvisc/rho

    dT_vector = []
    dQ_vector = []

    for i in range(len(r_vector)):
        rr = r_vector[i]
        chord = chord_dist[i]
        Beta = Beta_dist[i]

        Vr = radps*rr
        V = ((Vr**2)+(vi**2))**0.5
        Re = ((V*chord)/kvisc)

        alpha_c, Cl_c, Cd_c = xfoil_interface.get_curve_com_default(Re, a1, a2, astep, afile = airfoil)

        dT, dQ = induction_momentum_Ftip_fixed_pitch(radps, rr, Cl_c, Cd_c, alpha_c, Beta, Blades, rho, R, chord, vi) 
        dQ_vector.append(dQ)
        dT_vector.append(dT)

    return dT_vector, dQ_vector

def BEMT_PrePolars(vi, radps, Blades, R, r_vector, Beta_dist, chord_dist, CLPolar = 'TestCLPolar.dat', CDPolar = 'TestCDPolar.dat', rho = 1.225, dvisc = 1.8/100000):
    kvisc = dvisc/rho

    dT_vector = []
    dQ_vector = []

    for i in range(len(r_vector)):
        rr = r_vector[i]
        chord = chord_dist[i]
        Beta = Beta_dist[i]

        Vr = radps*rr
        V = ((Vr**2)+(vi**2))**0.5
        Re = ((V*chord)/kvisc)

        CLPolars = pd.read_table(CLPolar)
        CDPolars = pd.read_table(CDPolar)

        alpha_c, Cl_c = GetInterpolatedPolarFromPolars(CLPolars, Re)
        alpha_c, Cd_c = GetInterpolatedPolarFromPolars(CDPolars, Re)

        # alpha_c, Cl_c, Cd_c = xfoil_interface.get_curve_com_default(Re, a1, a2, astep, afile = airfoil)

        dT, dQ = induction_momentum_Ftip_fixed_pitch(radps, rr, Cl_c, Cd_c, alpha_c, Beta, Blades, rho, R, chord, vi) 
        dQ_vector.append(dQ)
        dT_vector.append(dT)

    return dT_vector, dQ_vector

def qprop_PrePolars(vi, radps, Blades, R, r_vector, Beta_dist, chord_dist, CLPolar = 'TestCLPolar.dat', CDPolar = 'TestCDPolar.dat', rho = 1.225, dvisc = 1.8/100000):
    #CHORD DISTRIBUTION TAKEN AS INPUT
    kvisc = dvisc/rho

    dT_vector = []
    dQ_vector = []
    Re_vector = []
    WA_vector = []
    WT_vector = []
    Cl_vector = []
    Cd_vector = []

    for i in range(len(r_vector)):
        rr = r_vector[i]
        Beta = Beta_dist[i]
        chord = chord_dist[i]

        Vr = radps*rr
        V = ((Vr**2)+(vi**2))**0.5
        Re = ((V*chord)/kvisc) 

        CLPolars = pd.read_table(CLPolar)
        CDPolars = pd.read_table(CDPolar)

        alpha_c, Cl_c = GetInterpolatedPolarFromPolars(CLPolars, Re)
        alpha_c, Cd_c = GetInterpolatedPolarFromPolars(CDPolars, Re)

        WA, WT, Cl, Cd = induction_qprop_fixed_pitch(radps, rr, Blades, alpha_c, Cl_c, Cd_c, Beta, R, chord, vi)
        W = (WA**2 + WT**2)**0.5
        phi = math.atan(WA/WT)
        dT = (rho*Blades*chord)*(W**2)*(Cl*math.cos(phi) - Cd*math.sin(phi))/2
        dQ = (rho*Blades*chord*rr)*(W**2)*(Cl*math.sin(phi) + Cd*math.cos(phi))/2


        dQ_vector.append(dQ)
        dT_vector.append(dT)
        Re_vector.append(Re)
        WA_vector.append(WA)
        WT_vector.append(WT)
        Cl_vector.append(Cl)
        Cd_vector.append(Cd)
    return dT_vector, dQ_vector, r_vector, Re_vector, WA_vector, Cl_vector, Cd_vector






#Induction methods
def induction_qprop_fixed_pitch(OMG, rr, BLDS, a_list, CL_list, CD_list, Beta, RAD, CHORD, VEL):
    EPS = 1E-06
    UA     = VEL   
    UT     = OMG*rr 

    WZ = (UA**2 + UT**2)**0.5

    PSImid = 0
    PSIup = math.radians(90)
    PSIlo = math.radians(-90)

    first = True

    while True:
        if first:
            RESup, _, _, _, _ = calculate_residual_fixed_pitch(UA, UT, WZ, Beta, PSIup, CHORD, a_list, CL_list, CD_list, rr, BLDS, RAD)
            RESlo, _, _, _, _ = calculate_residual_fixed_pitch(UA, UT, WZ, Beta, PSIlo, CHORD, a_list, CL_list, CD_list, rr, BLDS, RAD)
            first = False
        else:
            PSImid = (PSIup + PSIlo)/2

        RESmid, WAmid, WTmid, CL, CD = calculate_residual_fixed_pitch(UA, UT, WZ, Beta, PSImid, CHORD, a_list, CL_list, CD_list, rr, BLDS, RAD)

        if(abs(PSIup - PSIlo) < EPS):
            return WAmid, WTmid, CL, CD

        if RESup*RESmid < 0:
            RESlo = RESmid
            PSIlo = PSImid
        elif RESlo*RESmid < 0:
            RESup = RESmid
            PSIup = PSImid
        else:
            #print(f"Induction failed, section at radial position {(rr/RAD)*100}% will be assumed as simple flow")
            CL, CD = find_alpha_interval_return_CL_CD(a_list, CL_list, CD_list, math.degrees(math.atan(UA/UT)) - Beta)
            return UA, UT, CL, CD

def induction_momentum_Ftip_fixed_pitch(radps, rr, Cl_c, Cd_c, alpha_c, Beta, Blades, rho, R, chord, Vax_before):
    #pi = nup.pi
    #euler = nup.e
    check = 0
    ai = 0.1
    ai0 = 0.01
    ai_new = ai
    ai0_new = ai0

    while True:
        Vr = radps*rr*(1-ai0)
        Vax = Vax_before*(1+ai)
        V = ((Vr**2)+(Vax**2))**0.5

        phi = math.atan(Vax/Vr)
        alpha = Beta - math.degrees(phi)
        Cl, Cd = find_alpha_interval_return_CL_CD(alpha_c, Cl_c, Cd_c, alpha)

        if Vax == 0:
            F_tip = 1
        else:
            exp_func = (-Blades/2)*((R-rr)/rr)*(V/Vax)
            F_tip = (2/pi)*math.acos(euler**exp_func)

        dT = (rho*(V**2)/2)*(Cl*math.cos(phi)-Cd*math.sin(phi))*(Blades*chord)
        dQ = (rho*(V**2)/2)*(Cl*math.sin(phi)+Cd*math.cos(phi))*(Blades*chord)*rr

        if Vax == 0:
            ai_new += 0.01
            ai0_new += 0.001
        else:
            ai_new = dT/(4*pi*rr*rho*Vax*Vax_before*F_tip) #
            ai0_new = dQ/(4*pi*rr*rho*Vax*(radps*rr)*F_tip*rr) #

        ai_middle = (ai_new + ai)/2
        ai0_middle = (ai0_new + ai0)/2
        print(check)

        if ((abs(ai_middle - ai) < 1/100000) and (abs(ai0_middle - ai0) < 1/100000)) or check > 500:
            return dT, dQ

        ai = ai_middle
        ai0 = ai0_middle
        check += 1





#Residuals
def calculate_residual_fixed_pitch(UA, UT, WZ, Beta, PSI, CHORD, a_list, CL_list, CD_list, rr, BLDS, RAD):
    COSP = math.cos(PSI)
    SINP = math.sin(PSI)
    WA     = 0.5*UA     + 0.5*WZ    *SINP
    WT     = 0.5*UT     + 0.5*WZ    *COSP
    PHI = math.degrees(math.atan(WA/WT))
    alpha = Beta - PHI
    CL, CD = find_alpha_interval_return_CL_CD(a_list, CL_list, CD_list, alpha)
    if (WA <= 0.0):
        F     = 1.0
        ADW     = 0
    else:
        TSR = WT/WA * RAD/rr
        FARG     = 0.5*BLDS*(1.0-rr/RAD)*TSR
        FARG = min(FARG, 20.0 )   
        FEXP = euler**(-FARG) 
        if FEXP > 1 or FEXP < -1:
            F = 1
        else: 
            F = (2.0/pi) * math.acos(FEXP)
        ADW     =  1.0    /TSR
    VT     = UT     - WT
    QBI = 4.0/BLDS
    PIR = ((pi*rr)**2 + (QBI*RAD*ADW)**2)**0.5
    GAM     = QBI* F*VT                *PIR
    W = (WA**2 + WT**2)**0.5
    RES     = GAM     - 0.5*CHORD* CL*W
    return RES, WA, WT, CL, CD



#Auxiliary
def find_alpha_interval_return_CL_CD(a_list, cl_list, CD_list, alpha):
    #Exception cases
    if len(a_list) == 0:
        #print("Exception case 1: alpha list empty")
        return 0, 1
    if alpha < min(a_list):
        #print("Exception case 2: queried alpha is smaller than the minimum alpha -> returning values for lowest alpha")
        return cl_list[0], CD_list[0]
    if alpha > max(a_list):
        #print("Exception case 3: queried alpha is bigger than the maximum alpha -> returning values for highest alpha")
        return cl_list[-1], CD_list[-1]


    a_list_deducted = [x - alpha for x in a_list]
    for i in range(len(a_list) - 1):
        if a_list_deducted[i]*a_list_deducted[i + 1] < 0: 
            return linear_interpolate(a_list[i], a_list[i + 1], cl_list[i], cl_list[i + 1], alpha), linear_interpolate(a_list[i], a_list[i + 1], CD_list[i], CD_list[i + 1], alpha)
    #print("alpha interval not found")
    return 0, 1

def linear_interpolate(x0, x1, y0, y1, x):
    if x1 - x0 == 0:
        return (y0 + y1)/2
    return y0 + ((x - x0)*(y1 - y0)/(x1 - x0))

def InterpolateCurves(ValueCurve1, ValueCurve2, Curve1, Curve2, ValueWanted):
    Curve3 = []
    for i in range(len(Curve1)):
        Curve3.append(linear_interpolate(ValueCurve1, ValueCurve2, Curve1[i], Curve2[i], ValueWanted))
    return Curve3

def GetInterpolatedPolarFromPolars(PolarsDF, WantReynolds):
    if WantReynolds < int(PolarsDF.columns[1]):
        Polar = PolarsDF[PolarsDF.columns[1]].tolist()
    elif WantReynolds > int(PolarsDF.columns[-1]):
        Polar = PolarsDF[PolarsDF.columns[-1]].tolist()
    else:
        for i in range(len(PolarsDF.columns) - 2):
            key1 = PolarsDF.columns[i + 1]
            key2 = PolarsDF.columns[i + 2]
            if int(key1) < WantReynolds and int(key2) > WantReynolds: 
                Polar = InterpolateCurves(int(key1), int(key2), PolarsDF[key1], PolarsDF[key2], WantReynolds)
                break
            elif int(key1) == WantReynolds:
                Polar = PolarsDF[key1].tolist()
                break
            elif int(key2) == WantReynolds:
                Polar = PolarsDF[key2].tolist()
                break
    alphas = PolarsDF["alpha"].tolist()
    return alphas, Polar

def GeneratePolars(Airfoil, ReynoldsList, LowerAlpha, UpperAlpha, AlphaStep):
    AlphaCurvesList = []
    ClCurvesList = []
    CdCurvesList = []
    OutReynoldsList = []
    for Reynolds in ReynoldsList:
        a, cl, cd = xfoil_interface.get_curve_com_default(Reynolds, LowerAlpha, UpperAlpha, AlphaStep, afile = Airfoil)
        if len(a) == 0:
            continue
        else:
            AlphaCurvesList.append(a)
            ClCurvesList.append(cl)
            CdCurvesList.append(cd)
            OutReynoldsList.append(Reynolds)

    MaxAlphaLower = -math.inf
    MinAlphaUpper = math.inf
    for AlphaCurve in AlphaCurvesList:
        if AlphaCurve[0] > MaxAlphaLower:
            MaxAlphaLower = AlphaCurve[0]
        if AlphaCurve[-1] < MinAlphaUpper:
            MinAlphaUpper = AlphaCurve[-1]

    for i in range(len(AlphaCurvesList)):
        j = 0
        while j < len(AlphaCurvesList[i]) - 1:
            if abs(AlphaCurvesList[i][j] - AlphaCurvesList[i][j + 1]) > AlphaStep:
                ClCurvesList[i].insert(j + 1, linear_interpolate(AlphaCurvesList[i][j], AlphaCurvesList[i][j + 1], ClCurvesList[i][j], ClCurvesList[i][j + 1], AlphaCurvesList[i][j] + AlphaStep))
                CdCurvesList[i].insert(j + 1, linear_interpolate(AlphaCurvesList[i][j], AlphaCurvesList[i][j + 1], CdCurvesList[i][j], CdCurvesList[i][j + 1], AlphaCurvesList[i][j] + AlphaStep))
                AlphaCurvesList[i].insert(j + 1, AlphaCurvesList[i][j] + AlphaStep)
            if (AlphaCurvesList[i][j] > MinAlphaUpper) or (AlphaCurvesList[i][j] < MaxAlphaLower):
                ClCurvesList[i].pop(j)
                CdCurvesList[i].pop(j)
                AlphaCurvesList[i].pop(j)
                j -= 1
            if (AlphaCurvesList[i][j + 1] > MinAlphaUpper) or (AlphaCurvesList[i][j + 1] < MaxAlphaLower):
                ClCurvesList[i].pop(j + 1)
                CdCurvesList[i].pop(j + 1)
                AlphaCurvesList[i].pop(j + 1)
                j -= 1
            j += 1
    return AlphaCurvesList, ClCurvesList, CdCurvesList, OutReynoldsList

def WritePolar(PolarFileName, ReynoldsList, AlphaCurvesList, PolarCurvesList):
    with open(PolarFileName, 'w') as OutFile:
        ReynoldsOutput = ''
        for i in range(len(ReynoldsList)):
            ReynoldsOutput += f'{round(ReynoldsList[i], 0)}\t'
        OutFile.write(f"alpha\t{ReynoldsOutput}\n")
        for i in range(len(AlphaCurvesList[0])):
            first = True
            Output = ''
            for j in range(len(AlphaCurvesList)):
                if first:
                    Output += f'{AlphaCurvesList[j][i]}\t'
                    first = False
                Output += f'{round(PolarCurvesList[j][i], 5)}\t'
            OutFile.write(f'{Output}\n')

def calculate_most_eff_alpha(a_list, cl_list, cd_list):
    a, cl, cd = 0, 0, 1
    clcd_remember = 0
    for i in range(len(a_list)):
        if cd_list[i] == 0:
            continue
        clcd_try = cl_list[i]/cd_list[i]
        if clcd_try > clcd_remember:
            a, cl, cd = a_list[i], cl_list[i], cd_list[i]
            clcd_remember = clcd_try
    return a, cl, cd

# BEMT_PrePolars(0.00001, 4000, 2, 0.3, [0.25, 0.5, 0.75, 0.99], [30, 20, 10, 0], [0.3*0.1, 0.3*0.2, 0.3*0.2, 0.3*0.1])
# dT_vector, dQ_vector, r_vector, Re_vector, WA_vector, Cl_vector, Cd_vector = qprop_PrePolars(0, 2000, 2, 0.3, [0.25*0.3, 0.5*0.3, 0.75*0.3, 0.99*0.3], [30, 20, 10, 0], [0.3*0.1, 0.3*0.2, 0.3*0.2, 0.3*0.1])

# x = np.trapz(dT_vector, [0.25*0.3, 0.5*0.3, 0.75*0.3, 0.99*0.3])
# print(x)

# ReynoldsList = [50000*i for i in range(2, 6)]
# ReynoldsList.extend([250000*i for i in range(2, 5)])
# ReynoldsList.extend([1000000*i for i in range(2, 5)])

# AlphaCurvesList, ClCurvesList, CdCurvesList, OutReynoldsList = GeneratePolars(Airfoil = "clarky.dat", ReynoldsList = ReynoldsList, LowerAlpha = -10, UpperAlpha = 15, AlphaStep = 1)

# WritePolar("ClarkYAutoClPolar3.dat", OutReynoldsList, AlphaCurvesList, ClCurvesList)
# WritePolar("ClarkYAutoCdPolar3.dat", OutReynoldsList, AlphaCurvesList, CdCurvesList)

