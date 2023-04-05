import math

def broadband_noise(TotalBladeArea_m2, AverageBladeCL_adim, TotalThrust_N, TipSpeed_m_s, DistanceToObserver_m, AngleNegativeThrustToObserverPositionVector_deg):
    PeakFrequency = (-240*math.log10(TotalThrust_N)) + (2.448*TipSpeed_m_s) + 942

    #ONLY WORKS FOR AVERAGE CL < 0.48
    LiftCoefficientFunction = 10*math.log10(AverageBladeCL_adim/0.4) 

    #c0 is the speed of sound at sea level standard conditions m/sec
    SpeedSoundSeaLevelISA_m_s = 344

    SoundPressureLevelAtOneThirdBandBeforeCorrection = (20*(math.log10((TipSpeed_m_s/SpeedSoundSeaLevelISA_m_s)**3))) \
                                                     + (10*math.log10(TotalBladeArea_m2*((math.cos(math.radians(AngleNegativeThrustToObserverPositionVector_deg))**2) + 0.1)/(DistanceToObserver_m**2))) \
                                                     + LiftCoefficientFunction + 130

    # [min, max, center, preferred]
    StandardOneThirdOctaveBands = [[   13.9,    17.5,    15.6,    16.0], [   17.5,    22.1,    19.7,    20.0], [   22.1,    27.8,    24.8,    25.0], \
                                   [   27.8,    35.1,    31.3,    31.5], [   35.1,    44.2,    39.4,    40.0], [   44.2,    55.7,    49.6,    50.0], \
                                   [   55.7,    70.2,    62.5,    63.0], [   70.2,    88.4,    78.7,    80.0], [   88.4,   111.4,    99.2,   100.0], \
                                   [  111.4,   140.3,   125.0,   125.0], [  140.3,   176.8,   157.5,   160.0], [  176.8,   222.7,   198.4,   200.0], \
                                   [  222.7,   280.6,   250.0,   250.0], [  280.6,   353.6,   315.0,   315.0], [  356.6,   445.4,   396.9,   400.0], \
                                   [  445.4,   561.2,   500.0,   500.0], [  561.2,   707.1,   630.0,   630.0], [  707.1,   890.9,   793.7,   800.0], \
                                   [  890.9,  1122.5,  1000.0,  1000.0], [ 1122.5,  1414.2,  1259.9,  1250.0], [ 1414.2,  1781.8,  1587.4,  1600.0], \
                                   [ 1781.8,  2244.9,  2000.0,  2000.0], [ 2244.9,  2828.4,  2519.8,  2500.0], [ 2828.4,  3563.6,  3174.8,  3150.0], \
                                   [ 3563.6,  4489.8,  4000.0,  4000.0], [ 4489.8,  5656.9,  5039.7,  5000.0], [ 5656.9,  7127.2,  6349.6,  6300.0], \
                                   [ 7127.2,  8979.7,  8000.0,  8000.0], [ 8979.7, 11313.7, 10079.4, 10000.0], [11313.7, 14254.4, 12699.2, 12500.0], \
                                   [14254.4, 17959.4, 16000.0, 16000.0], [17959.4, 22627.4, 20158.7, 20000.0]]
    
    i = 0
    while i < len(StandardOneThirdOctaveBands):
        if (PeakFrequency > StandardOneThirdOctaveBands[i][0]) and (PeakFrequency < StandardOneThirdOctaveBands[i][1]):
            break
        i += 1
    BandsToEvaluate = [i - 15, i - 12, i - 9, i - 6, i - 3, i, i + 3, i + 6, i + 9, i + 12, i + 15, i + 18, i + 21]
    S13Ref = [-29.0, -24.5, -19.5, -15.3, -11.7, -7.5, -11.5, -12.1, -16.5, -17.0, -21.8, -26.4, -30.0]

    BandsEvaluated = []
    SoundPressureLevelAtOneThirdBandAfterCorrection = []
    i = 0
    while i < len(BandsToEvaluate):
        BandNumber = BandsToEvaluate[i]
        if BandNumber < 0:
            i += 1
            continue
        if BandNumber >= len(StandardOneThirdOctaveBands):
            break
        BandsEvaluated.append(StandardOneThirdOctaveBands[BandNumber][3])
        SoundPressureLevelAtOneThirdBandAfterCorrection.append(SoundPressureLevelAtOneThirdBandBeforeCorrection + S13Ref[i])
        i += 1
    return BandsEvaluated, SoundPressureLevelAtOneThirdBandAfterCorrection

def SumSoundPressureLevels(SoundPressureLevel1_dB, SoundPressureLevel2_dB):
    # Method based on https://personalpages.manchester.ac.uk/staff/richard.baker/BasicAcoustics/index.html
    Intensity1_W_m2 = (10**(-12))*(10**(SoundPressureLevel1_dB/10))
    Intensity2_W_m2 = (10**(-12))*(10**(SoundPressureLevel2_dB/10))

    IntensitySum_W_m2 = Intensity1_W_m2 + Intensity2_W_m2

    SoundPressureLevelSum_dB = 10*math.log10(IntensitySum_W_m2/(10**(-12)))

    return(SoundPressureLevelSum_dB)

def SumMultipleSoundPressureLevels(SoundPressureLevelVec_dB):
    # Method based on https://personalpages.manchester.ac.uk/staff/richard.baker/BasicAcoustics/index.html
    IntensitiesSum = 0
    for i in range(len(SoundPressureLevelVec_dB)):
        IntensitiesSum += 10**(SoundPressureLevelVec_dB[i]/10)

    return 10*math.log10(IntensitiesSum)

def CalculatePerceivedNoiseLevelToneCorrectedFromSoundPressureLevelDistribution(SPLFullDistribution_dB_Hz):
    # Only consider the bands from 50 (index 5) to 10K (index 28) Hz
    NoyTable = [[ 1,    50,      91.0,   64,   52,   49,   55, 0.043478, 0.030103, 0.079520, 0.058098],\
                [ 2,    63,      85.9,   60,   51,   44,   51, 0.040570, 0.030103, 0.068160, 0.058098],\
                [ 3,    80,      87.3,   56,   49,   39,   46, 0.036831, 0.030103, 0.068160, 0.052288],\
                [ 4,   100,      79.9,   53,   47,   34,   42, 0.036831, 0.030103, 0.059640, 0.047534],\
                [ 5,   125,      79.8,   51,   46,   30,   39, 0.035336, 0.030103, 0.053013, 0.043573],\
                [ 6,   160,      76.0,   48,   45,   27,   36, 0.033333, 0.030103, 0.053013, 0.043573],\
                [ 7,   200,      74.0,   46,   43,   24,   33, 0.033333, 0.030103, 0.053013, 0.040221],\
                [ 8,   250,      74.9,   44,   42,   21,   30, 0.032051, 0.030103, 0.053013, 0.037349],\
                [ 9,   315,      94.6,   42,   41,   18,   27, 0.030675, 0.030103, 0.053013, 0.034859],\
                [10,   400,  math.inf,   40,   40,   16,   25, 0.030103,      -1., 0.053013, 0.034859],\
                [11,   500,  math.inf,   40,   40,   16,   25, 0.030103,      -1., 0.053013, 0.034859],\
                [12,   630,  math.inf,   40,   40,   16,   25, 0.030103,      -1., 0.053013, 0.034859],\
                [13,   800,  math.inf,   40,   40,   16,   25, 0.030103,      -1., 0.053013, 0.034859],\
                [14,  1000,  math.inf,   40,   40,   16,   25, 0.030103,      -1., 0.053013, 0.034859],\
                [15,  1250,  math.inf,   38,   38,   15,   23, 0.030103,      -1., 0.059640, 0.034859],\
                [16,  1600,  math.inf,   34,   34,   12,   21, 0.029960,      -1., 0.053013, 0.040221],\
                [17,  2000,  math.inf,   32,   32,    9,   18, 0.029960,      -1., 0.053013, 0.037349],\
                [18,  2500,  math.inf,   30,   30,    5,   15, 0.029960,      -1., 0.047712, 0.034859],\
                [19,  3150,  math.inf,   29,   29,    4,   14, 0.029960,      -1., 0.047712, 0.034859],\
                [20,  4000,  math.inf,   29,   29,    5,   14, 0.029960,      -1., 0.053013, 0.034859],\
                [21,  5000,  math.inf,   30,   30,    6,   15, 0.029960,      -1., 0.053013, 0.034859],\
                [22,  6300,  math.inf,   31,   31,   10,   17, 0.029960,      -1., 0.068160, 0.037349],\
                [23,  8000,      44.3,   37,   34,   17,   23, 0.042285, 0.029960, 0.079520, 0.037349],\
                [24, 10000,      50.7,   41,   37,   21,   29, 0.042285, 0.029960, 0.059640, 0.043573]]
    
    PerceivedNoisiness = []
    for i in range(5, 29):  # index from full 32 Band SPL
        j = i - 5           # index from 24 Band SPL
        if (SPLFullDistribution_dB_Hz[i] >= NoyTable[j][2]): 
            #SPLa
            Noy = 10**(NoyTable[j][8]*(SPLFullDistribution_dB_Hz[i] - NoyTable[j][4]))
        elif ((SPLFullDistribution_dB_Hz[i] < NoyTable[j][2]) and (SPLFullDistribution_dB_Hz[i] >= NoyTable[j][3])):
            #SPLa and SPLb
            Noy = 10**(NoyTable[j][7]*(SPLFullDistribution_dB_Hz[i] - NoyTable[j][3]))
        elif ((SPLFullDistribution_dB_Hz[i] < NoyTable[j][3]) and (SPLFullDistribution_dB_Hz[i] >= NoyTable[j][6])):
            #SPLb and SPLe
            Noy = 0.3*(10**(NoyTable[j][10]*(SPLFullDistribution_dB_Hz[i] - NoyTable[j][6])))
        elif ((SPLFullDistribution_dB_Hz[i] < NoyTable[j][6]) and (SPLFullDistribution_dB_Hz[i] >= NoyTable[j][5])):
            #SPLe and SPLd
            Noy = 0.1*(10**(NoyTable[j][9]*(SPLFullDistribution_dB_Hz[i] - NoyTable[j][5])))
        else:
            Noy = SPLFullDistribution_dB_Hz[i]
        PerceivedNoisiness.append(Noy)
    
    TotalPerceivedNoisiness = 0.85*max(PerceivedNoisiness)
    for PN in PerceivedNoisiness:
        TotalPerceivedNoisiness += 0.15*PN

    PerceivedNoiseLevel = 40.0 + ((10*math.log10(TotalPerceivedNoisiness))/math.log10(2))

    return PerceivedNoiseLevel

    #   j + 1,    Hz, SPLa, SPLb, SPLc, SPLd, SPLe,       Mb,       Mc,       Md,       Me
    #    [[ 1,    50, 91.0,   64,   52,   49,   55, 0.043478, 0.030103, 0.079520, 0.058098],
    #     [ 2,    63, 85.9,   60,   51,   44,   51, 0.040570, 0.030103, 0.068160, 0.058098],
    #     [ 3,    80, 87.3,   56,   49,   39,   46, 0.036831, 0.030103, 0.068160, 0.052288],
    #     [ 4,   100, 79.9,   53,   47,   34,   42, 0.036831, 0.030103, 0.059640, 0.047534],
    #     [ 5,   125, 79.8,   51,   46,   30,   39, 0.035336, 0.030103, 0.053013, 0.043573],
    #     [ 6,   160, 76.0,   48,   45,   27,   36, 0.033333, 0.030103, 0.053013, 0.043573],
    #     [ 7,   200, 74.0,   46,   43,   24,   33, 0.033333, 0.030103, 0.053013, 0.040221],
    #     [ 8,   250, 74.9,   44,   42,   21,   30, 0.032051, 0.030103, 0.053013, 0.037349],
    #     [ 9,   315, 94.6,   42,   41,   18,   27, 0.030675, 0.030103, 0.053013, 0.034859],
    #     [10,   400,  inf,   40,   40,   16,   25, 0.030103,      N/A, 0.053013, 0.034859],
    #     [11,   500,  inf,   40,   40,   16,   25, 0.030103,      N/A, 0.053013, 0.034859],
    #     [12,   630,  inf,   40,   40,   16,   25, 0.030103,      N/A, 0.053013, 0.034859],
    #     [13,   800,  inf,   40,   40,   16,   25, 0.030103,      N/A, 0.053013, 0.034859],
    #     [14,  1000,  inf,   40,   40,   16,   25, 0.030103,      N/A, 0.053013, 0.034859],
    #     [15,  1250,  inf,   38,   38,   15,   23, 0.030103,      N/A, 0.059640, 0.034859],
    #     [16,  1600,  inf,   34,   34,   12,   21, 0.029960,      N/A, 0.053013, 0.040221],
    #     [17,  2000,  inf,   32,   32,    9,   18, 0.029960,      N/A, 0.053013, 0.037349],
    #     [18,  2500,  inf,   30,   30,    5,   15, 0.029960,      N/A, 0.047712, 0.034859],
    #     [19,  3150,  inf,   29,   29,    4,   14, 0.029960,      N/A, 0.047712, 0.034859],
    #     [20,  4000,  inf,   29,   29,    5,   14, 0.029960,      N/A, 0.053013, 0.034859],
    #     [21,  5000,  inf,   30,   30,    6,   15, 0.029960,      N/A, 0.053013, 0.034859],
    #     [22,  6300,  inf,   31,   31,   10,   17, 0.029960,      N/A, 0.068160, 0.037349],
    #     [23,  8000, 44.3,   37,   34,   17,   23, 0.042285, 0.029960, 0.079520, 0.037349],
    #     [24, 10000, 50.7,   41,   37,   21,   29, 0.042285, 0.029960, 0.059640, 0.043573]]

bands, spl = broadband_noise(18.6, 0.438, 69420, 208, 61.6, 85) #Example 1 main rotor on tm-80200
print(CalculatePerceivedNoiseLevelToneCorrectedFromSoundPressureLevelDistribution([80]*32))
# print(broadband_noise(3.44, 0.182, 5206, 202, 62.2, 10)) #Example 1 tail rotor on tm-80200

#print(SumSoundPressureLevels(80, 80))
#print(SumMultipleSoundPressureLevels([80, 80, 80, 80]))


