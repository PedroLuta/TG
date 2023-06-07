import prop_simulate
import numpy as np
pi = np.pi
import xfoil_interface
import math
import matplotlib.pyplot as plt


def linear_interpolate(x0, x1, y0, y1, x):
    if x1 - x0 == 0:
        return (y0 + y1)/2
    return y0 + ((x - x0)*(y1 - y0)/(x1 - x0))

AxialVelocity_m_s = 10
RPM = 2000
Omega_rad_s = RPM*2*pi/60
Radius_m = 0.5
DiscretizedRadius_m = np.linspace(0.2*Radius_m, 0.99*Radius_m, 21, endpoint = True)
DiscretizedBeta_deg = [20]*len(DiscretizedRadius_m)
DiscretizedChord_m = [0.2*Radius_m]*len(DiscretizedRadius_m)


# dT_vector, dQ_vector, r_vector, Re_vector, WA_vector, Cl_vector, Cd_vector = prop_simulate.qprop_fixed_pitch(AxialVelocity_m_s, Omega_rad_s, 2, Radius_m, DiscretizedRadius_m, DiscretizedBeta_deg, DiscretizedChord_m, airfoil = 'clarky.dat', alphas = [-10, 15, 1])
# x = np.trapz(dT_vector, r_vector)

# dT_vector, dQ_vector, r_vector, Re_vector, WA_vector, Cl_vector, Cd_vector = prop_simulate.qprop_PrePolars(AxialVelocity_m_s, Omega_rad_s, 2, Radius_m, DiscretizedRadius_m, DiscretizedBeta_deg, DiscretizedChord_m, CLPolar = "ClarkYCLPolar.dat", CDPolar = "ClarkYCDPolar.dat")
# x2 = np.trapz(dT_vector, r_vector)
# print(x)
# print(x2)
# print(Re_vector)
    

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

ReynoldsList = [50000*i for i in range(2, 6)]
ReynoldsList.extend([250000*i for i in range(2, 5)])
ReynoldsList.extend([1000000*i for i in range(2, 5)])

AlphaCurvesList, ClCurvesList, CdCurvesList, OutReynoldsList = GeneratePolars(Airfoil = "clarky.dat", ReynoldsList = ReynoldsList, LowerAlpha = -10, UpperAlpha = 15, AlphaStep = 1)

WritePolar("ClarkYAutoClPolar3.dat", OutReynoldsList, AlphaCurvesList, ClCurvesList)
WritePolar("ClarkYAutoCdPolar3.dat", OutReynoldsList, AlphaCurvesList, CdCurvesList)

# for i in range(len(AlphaCurvesList)):
#     plt.plot(AlphaCurvesList[i], CdCurvesList[i], label = OutReynoldsList[i])
# plt.legend()
# plt.show()