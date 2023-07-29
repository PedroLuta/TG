import os
import pandas as pd
import subprocess as sp

def GetXfoilCurves(Reynolds, Mach, Ncrit, InitialAlpha_deg, FinalAlpha_deg, AlphaStep_deg, AirfoilFile):
    Timeout_s = 25
    OutputFile = "XfoilOutput.txt"

    try:
        os.remove(OutputFile)
    except:
        pass
    
    if (InitialAlpha_deg*FinalAlpha_deg < 0):
        AlphaList1, ClList1, CdList1, CmList1 = GetXfoilCurves(Reynolds, Mach, Ncrit, 0, InitialAlpha_deg, -AlphaStep_deg, AirfoilFile)
        AlphaList2, ClList2, CdList2, CmList2 = GetXfoilCurves(Reynolds, Mach, Ncrit, 0, FinalAlpha_deg, AlphaStep_deg, AirfoilFile)
        if (len(AlphaList1) > 0) and (len(AlphaList2) > 0) and (AlphaList1[0] == AlphaList2[0]):
            AlphaList1.pop(0)
            ClList1.pop(0)
            CdList1.pop(0)
            CmList1.pop(0)
        AlphaList1.reverse()
        ClList1.reverse()
        CdList1.reverse()
        CmList1.reverse()
        AlphaList1.extend(AlphaList2)
        ClList1.extend(ClList2)
        CdList1.extend(CdList2)
        CmList1.extend(CmList2)
        return AlphaList1, ClList1, CdList1, CmList1
    
    XfoilCommand = f'plop\ng f\n\nload {AirfoilFile}\n\noper\nvpar\nn {Ncrit}\n\niter 250\nvisc {Reynolds}\nM {Mach}\n'
    if InitialAlpha_deg != 0:
        XfoilCommand += f'aseq 0 {InitialAlpha_deg} 1\n'
    XfoilCommand += f'pacc\n{OutputFile}\n\naseq {InitialAlpha_deg} {FinalAlpha_deg} {AlphaStep_deg}\n\n\n\nquit\n'
    
    SubProcess = sp.Popen("xfoil.exe", universal_newlines = True, stdin = sp.PIPE, stdout = sp.PIPE)
    try:
        Output, _ = SubProcess.communicate(XfoilCommand, timeout = Timeout_s)
    except:
        pass
    SubProcess.kill()
    Dataframe = pd.read_csv(OutputFile, header=5, delim_whitespace=True)

    AlphaList = [float(Placeholder) for Placeholder in list(Dataframe['alpha'])[1:]]
    ClList = [float(Placeholder) for Placeholder in list(Dataframe['CL'])[1:]]
    CdList = [float(Placeholder) for Placeholder in list(Dataframe['CD'])[1:]]
    CmList = [float(Placeholder) for Placeholder in list(Dataframe['CM'])[1:]]

    try:
        os.remove(OutputFile)
    except:
        pass

    return AlphaList, ClList, CdList, CmList

if (__name__ == '__main__'):
    Airfoil = "clarky.dat"
    ResultsFolder = f'Results'
    AlphaInicial = -25
    AlphaFinal = 30
    AlphaStep = 1
    for Mach in [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6]:
        for Reynolds in [100000, 150000, 200000, 250000, 500000, 750000, 1000000, 2000000, 3000000, 4000000]:
            AlphaList, ClList, CdList, CmList = GetXfoilCurves(Reynolds, Mach, 9, AlphaInicial, AlphaFinal, AlphaStep, Airfoil)
            with open(f'{ResultsFolder}\\CL{Airfoil}_Reynolds{round(Reynolds, 0)}_Mach{Mach}.txt', 'w') as Output:
                Output.write(f'Alpha,Cl\n')
                for i in range(len(AlphaList)):
                    Output.write(f'{AlphaList[i]},{ClList[i]}\n')
            with open(f'{ResultsFolder}\\CD{Airfoil}_Reynolds{round(Reynolds, 0)}_Mach{Mach}.txt', 'w') as Output:
                Output.write(f'Alpha,Cd\n')
                for i in range(len(AlphaList)):
                    Output.write(f'{AlphaList[i]},{CdList[i]}\n')
                


# if (__name__ == '__main__'):
#     AirfoilsFolder = "RunAirfoils_Paper"
#     AirfoilFiles = [file for file in os.listdir(AirfoilsFolder) if os.path.isfile(os.path.join(AirfoilsFolder,file))]
#     ResultsFolder = f'{AirfoilsFolder}\\Results2'
#     KnownMachs = [0.29845, 0.30048, 0.32705, 0.35606, 0.39017, 0.42339, 0.44631, 0.47241, 0.50769, 0.53656, 0.56453, 0.59241, 0.62328, 0.64598, 0.66706, 0.68855, 0.704, 0.72438, 0.74301, 0.76278, 0.78951, 0.81616, 0.84147, 0.86043, 0.8835, 0.89897]
#     KnownReynolds = [0.99044, 0.99748, 1.07209, 1.15375, 1.24578, 1.3283, 1.38677, 1.44651, 1.52327, 1.58097, 1.63288, 1.67694, 1.71855, 1.74563, 1.76567, 1.78447, 1.79702, 1.81085, 1.82384, 1.83644, 1.85199, 1.86217, 1.8711, 1.87749, 1.88433, 1.88903]
    
#     for Ncrit in [4, 0.1]:
#         for Mach in [0.3, 0.4, 0.5, 0.6, 0.7]:
#             for i in range(len(KnownMachs) - 1):
#                 if (Mach > KnownMachs[i]) and (Mach < KnownMachs[i+1]):
#                     Reynolds = KnownReynolds[i] + ((Mach - KnownMachs[i])*(KnownReynolds[i+1] - KnownReynolds[i])/(KnownMachs[i+1] - KnownMachs[i]))
#                     break
#             for AirfoilFile in AirfoilFiles:
#                 AirfoilName = AirfoilFile.split('.')[0]
#                 AlphaInicial = -25
#                 AlphaFinal = 30
#                 AlphaStep = 1
#                 # if AirfoilName == "NACA0015":
#                 #     AlphaFinal = 14
#                 # elif AirfoilName == "NACA4415":
#                 #     AlphaFinal = 10
#                 # elif AirfoilName == "NACA23015":
#                 #     AlphaFinal = 16
#                 # elif AirfoilName == "NACA63-206":
#                 #     AlphaFinal = 12
#                 # elif AirfoilName == "NACA63-212":
#                 #     AlphaFinal = 12
#                 AirfoilPath = f'{AirfoilsFolder}\\{AirfoilFile}'
#                 AlphaList, ClList, CdList, CmList = GetXfoilCurves(Reynolds*(10**6), Mach, Ncrit, AlphaInicial, AlphaFinal, AlphaStep, AirfoilPath)
#                 with open(f'{ResultsFolder}\\{AirfoilName}_Reynolds{round(Reynolds, 4)}_Mach{Mach}_Ncrit{Ncrit}.txt', 'w') as Output:
#                     Output.write(f'Alpha,Cl,Cd,Cm\n')
#                     for i in range(len(AlphaList)):
#                         Output.write(f'{AlphaList[i]},{ClList[i]},{CdList[i]},{CmList[i]}\n')