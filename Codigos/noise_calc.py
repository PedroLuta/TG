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

def CalculatePNLTFromSPLDistribution(SPLFullDistribution_dB_Hz):
    for i in range(5, 29):
        pass

# broadband_noise(18.6, 0.438, 69420, 208, 61.6, 85) #Example 1 main rotor on tm-80200
# print(broadband_noise(3.44, 0.182, 5206, 202, 62.2, 10)) #Example 1 tail rotor on tm-80200

print(SumSoundPressureLevels(80, 80))
print(SumMultipleSoundPressureLevels([80, 80, 80, 80]))
