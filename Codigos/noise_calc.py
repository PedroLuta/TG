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

def CalculateToneCorrection(SPLDistribution_dB_Hz):
    EncircledSlopeIndexes = []
    EncircledSPLIndexes = []
    Slope = [math.nan]*len(SPLDistribution_dB_Hz)
    for i in range(3, len(SPLDistribution_dB_Hz)):
        Slope[i] = SPLDistribution_dB_Hz[i] - SPLDistribution_dB_Hz[i - 1]
    
    for i in range(4, len(Slope)):
        if (abs(Slope[i] - Slope[i - 1]) > 5):
            EncircledSlopeIndexes.append(i)
    
    for i in EncircledSlopeIndexes:
        if ((Slope[i] > 0) and (Slope[i] > Slope[i - 1])):
            EncircledSPLIndexes.append(i)
        elif ((Slope[i] <= 0) and (Slope[i - 1] > 0)):
            EncircledSPLIndexes.append(i - 1)

    CorrectedSPLDistribution_dB_Hz = []

    for i in range(len(SPLDistribution_dB_Hz)):
        if (i not in EncircledSlopeIndexes):
            CorrectedSPLDistribution_dB_Hz.append(SPLDistribution_dB_Hz[i])
        elif (i != len(SPLDistribution_dB_Hz) - 1):
            CorrectedSPLDistribution_dB_Hz.append(0.5*(SPLDistribution_dB_Hz[i - 1] + SPLDistribution_dB_Hz[i + 1]))
        else:
            CorrectedSPLDistribution_dB_Hz.append(SPLDistribution_dB_Hz[i - 1] + Slope[i - 1])

    Slope2 = [math.nan]*(len(SPLDistribution_dB_Hz) + 1)
    for i in range(3, len(CorrectedSPLDistribution_dB_Hz)):
        Slope2[i] = CorrectedSPLDistribution_dB_Hz[i] - CorrectedSPLDistribution_dB_Hz[i - 1]
    Slope2[2] = Slope2[3]
    Slope2[-1] = Slope2[-2]

    Slope2Average = [math.nan]*(len(SPLDistribution_dB_Hz) + 1)
    for i in range(2, len(Slope2) - 2):
        Slope2Average[i] = (Slope2[i] + Slope2[i + 1] + Slope2[i + 2])/3
    
    FinalCorrectedSPLDistribution_dB_Hz = CorrectedSPLDistribution_dB_Hz
    for i in range(3, len(CorrectedSPLDistribution_dB_Hz)):
        FinalCorrectedSPLDistribution_dB_Hz[i] = FinalCorrectedSPLDistribution_dB_Hz[i - 1] + Slope2Average[i - 1]
    
    Differences = []
    for i in range(len(FinalCorrectedSPLDistribution_dB_Hz)):
        Differences.append(SPLDistribution_dB_Hz[i] - FinalCorrectedSPLDistribution_dB_Hz[i])


    ToneCorrection = []
    for i in range(len(Differences)):
        #0 (50) -> 9 (400) (incluindo 9)
        if (i <= 9):
            if (Differences[i] < 1.5):
                ToneCorrection.append(0)
            elif (Differences[i] < 3):
                ToneCorrection.append((Differences[i]/3) - 0.5)
            elif (Differences[i] < 20):
                ToneCorrection.append(Differences[i]/6)
            else:
                ToneCorrection.append(10/3)
        #10 (500) -> 20 (5000) (incluindo 20)
        elif (i <= 20):
            if (Differences[i] < 1.5):
                ToneCorrection.append(0)
            elif (Differences[i] < 3):
                ToneCorrection.append((2*Differences[i]/3) - 1)
            elif (Differences[i] < 20):
                ToneCorrection.append(Differences[i]/3)
            else:
                ToneCorrection.append(20/3)
        #21 (6300) -> 23 (10000)
        else:
            if (Differences[i] < 1.5):
                ToneCorrection.append(0)
            elif (Differences[i] < 3):
                ToneCorrection.append((Differences[i]/3) - 0.5)
            elif (Differences[i] < 20):
                ToneCorrection.append(Differences[i]/6)
            else:
                ToneCorrection.append(10/3)

    FinalToneCorrection = max(ToneCorrection)

    return FinalToneCorrection



def CalculatePNL(SPLDistribution_dB_Hz):
    # It is expected the SPL values for 50Hz to 10kHz 
          #   i + 1,    Hz,      SPLa, SPLb, SPLc, SPLd, SPLe,       Mb,       Mc,       Md,       Me
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
    for i in range(len(SPLDistribution_dB_Hz)):
        if (SPLDistribution_dB_Hz[i] >= NoyTable[i][2]): 
            #SPLa
            Noy = 10**(NoyTable[i][8]*(SPLDistribution_dB_Hz[i] - NoyTable[i][4]))
        elif ((SPLDistribution_dB_Hz[i] < NoyTable[i][2]) and (SPLDistribution_dB_Hz[i] >= NoyTable[i][3])):
            #SPLa and SPLb
            Noy = 10**(NoyTable[i][7]*(SPLDistribution_dB_Hz[i] - NoyTable[i][3]))
        elif ((SPLDistribution_dB_Hz[i] < NoyTable[i][3]) and (SPLDistribution_dB_Hz[i] >= NoyTable[i][6])):
            #SPLb and SPLe
            Noy = 0.3*(10**(NoyTable[i][10]*(SPLDistribution_dB_Hz[i] - NoyTable[i][6])))
        elif ((SPLDistribution_dB_Hz[i] < NoyTable[i][6]) and (SPLDistribution_dB_Hz[i] >= NoyTable[i][5])):
            #SPLe and SPLd
            Noy = 0.1*(10**(NoyTable[i][9]*(SPLDistribution_dB_Hz[i] - NoyTable[i][5])))
        else:
            Noy = 0
        PerceivedNoisiness.append(Noy)
    
    TotalPerceivedNoisiness = 0.85*max(PerceivedNoisiness)
    for PN in PerceivedNoisiness:
        TotalPerceivedNoisiness += 0.15*PN

    print(TotalPerceivedNoisiness)

    PerceivedNoiseLevel = 40.0 + ((10*math.log10(TotalPerceivedNoisiness))/math.log10(2))

    return PerceivedNoiseLevel

def CalculatePNLT(SPLDistribution_dB_Hz):
    # It is expected the SPL values for 50Hz to 10kHz 
    
    PNL = CalculatePNL(SPLDistribution_dB_Hz)
    C = CalculateToneCorrection(SPLDistribution_dB_Hz)

    return PNL + C
    #SPLWorkingDistribution_dB_Hz = SPLFullDistribution_dB_Hz[5:29]


#bands, spl = broadband_noise(18.6, 0.438, 69420, 208, 61.6, 85) #Example 1 main rotor on tm-80200
print(CalculatePNLT([int(input("SPL: "))]*24))
# print(CalculatePNLT([16.0, 20.0, 25.0, 31.5, 40.0, 50.0, 63.0, 80.0, 100.0, 125.0, 160.0, 200.0, 250.0, 315.0, 400.0, 500.0, 630.0, 800.0, 1000.0, 1250.0, 1600.0, 2000.0, 2500.0, 3150.0, 4000.0, 5000.0, 6300.0, 8000.0, 10000.0, 12500.0, 16000.0, 20000.0]))
# print(broadband_noise(3.44, 0.182, 5206, 202, 62.2, 10)) #Example 1 tail rotor on tm-80200

#print(SumSoundPressureLevels(80, 80))
#print(SumMultipleSoundPressureLevels([80, 80, 80, 80]))


