import math
import numpy as np
import matplotlib.pyplot as plt

class OneThirdSpectrum:
    def __init__(self, Bands_Hz = [], SoundPressureLevels_dB = []):
        # [min, max, center, preferred] SOURCE: https://apmr.matelys.com/Standards/OctaveBands.html
        self.StandardOneThirdOctaveBands = [[   13.9,    17.5,    15.6,    16.0], \
                                            [   17.5,    22.1,    19.7,    20.0], \
                                            [   22.1,    27.8,    24.8,    25.0], \
                                            [   27.8,    35.1,    31.3,    31.5], \
                                            [   35.1,    44.2,    39.4,    40.0], \
                                            [   44.2,    55.7,    49.6,    50.0], \
                                            [   55.7,    70.2,    62.5,    63.0], \
                                            [   70.2,    88.4,    78.7,    80.0], \
                                            [   88.4,   111.4,    99.2,   100.0], \
                                            [  111.4,   140.3,   125.0,   125.0], \
                                            [  140.3,   176.8,   157.5,   160.0], \
                                            [  176.8,   222.7,   198.4,   200.0], \
                                            [  222.7,   280.6,   250.0,   250.0], \
                                            [  280.6,   356.6,   315.0,   315.0], \
                                            [  356.6,   445.4,   396.9,   400.0], \
                                            [  445.4,   561.2,   500.0,   500.0], \
                                            [  561.2,   707.1,   630.0,   630.0], \
                                            [  707.1,   890.9,   793.7,   800.0], \
                                            [  890.9,  1122.5,  1000.0,  1000.0], \
                                            [ 1122.5,  1414.2,  1259.9,  1250.0], \
                                            [ 1414.2,  1781.8,  1587.4,  1600.0], \
                                            [ 1781.8,  2244.9,  2000.0,  2000.0], \
                                            [ 2244.9,  2828.4,  2519.8,  2500.0], \
                                            [ 2828.4,  3563.6,  3174.8,  3150.0], \
                                            [ 3563.6,  4489.8,  4000.0,  4000.0], \
                                            [ 4489.8,  5656.9,  5039.7,  5000.0], \
                                            [ 5656.9,  7127.2,  6349.6,  6300.0], \
                                            [ 7127.2,  8979.7,  8000.0,  8000.0], \
                                            [ 8979.7, 11313.7, 10079.4, 10000.0], \
                                            [11313.7, 14254.4, 12699.2, 12500.0], \
                                            [14254.4, 17959.4, 16000.0, 16000.0], \
                                            [17959.4, 22627.4, 20158.7, 20000.0]]
        self.CentralBands_Hz = [   16.0, \
                                   20.0, \
                                   25.0, \
                                   31.5, \
                                   40.0, \
                                   50.0, \
                                   63.0, \
                                   80.0, \
                                  100.0, \
                                  125.0, \
                                  160.0, \
                                  200.0, \
                                  250.0, \
                                  315.0, \
                                  400.0, \
                                  500.0, \
                                  630.0, \
                                  800.0, \
                                 1000.0, \
                                 1250.0, \
                                 1600.0, \
                                 2000.0, \
                                 2500.0, \
                                 3150.0, \
                                 4000.0, \
                                 5000.0, \
                                 6300.0, \
                                 8000.0, \
                                10000.0, \
                                12500.0, \
                                16000.0, \
                                20000.0]
        self.Spectrum_dB = [math.nan]*32
        for i in range(len(Bands_Hz)):
            if (Bands_Hz[i] in self.CentralBands_Hz):
                self.Spectrum_dB[i] = SoundPressureLevels_dB[i]

    def SumToSpectrum(self, Bands_Hz, SoundPressureLevels_dB):
        for i in range(len(Bands_Hz)):
            if (Bands_Hz[i] in self.CentralBands_Hz):
                index = self.CentralBands_Hz.index(Bands_Hz[i])
            else:
                j = 0
                while j < len(self.StandardOneThirdOctaveBands):
                    if (Bands_Hz[i] > self.StandardOneThirdOctaveBands[j][0]) and (Bands_Hz[i] <= self.StandardOneThirdOctaveBands[j][1]):
                        break
                    j += 1
                CentralBand = self.StandardOneThirdOctaveBands[j][3]
                index = self.CentralBands_Hz.index(CentralBand)
            if (np.isnan(self.Spectrum_dB[index])):
                self.Spectrum_dB[index] = SoundPressureLevels_dB[i]
            else:
                self.Spectrum_dB[index] = SumSoundPressureLevels(self.Spectrum_dB[index], SoundPressureLevels_dB[i])

    def PNL(self):
        SPLDistribution_dB = self.Spectrum_dB[5:29].copy()
        for i in range(len(SPLDistribution_dB)):
            if (np.isnan(SPLDistribution_dB[i])):
                SPLDistribution_dB[i] = 0

        # It is expected the SPL values for 50Hz to 10kHz 
          #       i + 1,    Hz,      SPLa, SPLb, SPLc, SPLd, SPLe,       Mb,       Mc,       Md,       Me
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
        for i in range(len(SPLDistribution_dB)):
            if (SPLDistribution_dB[i] >= NoyTable[i][2]): 
                #SPLa
                Noy = 10**(NoyTable[i][8]*(SPLDistribution_dB[i] - NoyTable[i][4]))
            elif ((SPLDistribution_dB[i] < NoyTable[i][2]) and (SPLDistribution_dB[i] >= NoyTable[i][3])):
                #SPLa and SPLb
                Noy = 10**(NoyTable[i][7]*(SPLDistribution_dB[i] - NoyTable[i][3]))
            elif ((SPLDistribution_dB[i] < NoyTable[i][3]) and (SPLDistribution_dB[i] >= NoyTable[i][6])):
                #SPLb and SPLe
                Noy = 0.3*(10**(NoyTable[i][10]*(SPLDistribution_dB[i] - NoyTable[i][6])))
            elif ((SPLDistribution_dB[i] < NoyTable[i][6]) and (SPLDistribution_dB[i] >= NoyTable[i][5])):
                #SPLe and SPLd
                Noy = 0.1*(10**(NoyTable[i][9]*(SPLDistribution_dB[i] - NoyTable[i][5])))
            else:
                Noy = 0
            PerceivedNoisiness.append(Noy)
        
        TotalPerceivedNoisiness = 0.85*max(PerceivedNoisiness)
        for PN in PerceivedNoisiness:
            TotalPerceivedNoisiness += 0.15*PN
    
        PerceivedNoiseLevel = 40.0 + ((10*math.log10(TotalPerceivedNoisiness))/math.log10(2))
    
        return PerceivedNoiseLevel
    
    def ToneCorrection(self):
        SPLDistribution_dB = self.Spectrum_dB[5:29].copy()
        for i in range(len(SPLDistribution_dB)):
            if (np.isnan(SPLDistribution_dB[i])):
                SPLDistribution_dB[i] = 0

        EncircledSlopeIndexes = []
        EncircledSPLIndexes = []
        Slope = [math.nan]*len(SPLDistribution_dB)
        for i in range(3, len(SPLDistribution_dB)):
            Slope[i] = SPLDistribution_dB[i] - SPLDistribution_dB[i - 1]

        for i in range(4, len(Slope)):
            if (abs(Slope[i] - Slope[i - 1]) > 5):
                EncircledSlopeIndexes.append(i)

        for i in EncircledSlopeIndexes:
            if ((Slope[i] > 0) and (Slope[i] > Slope[i - 1])):
                EncircledSPLIndexes.append(i)
            elif ((Slope[i] <= 0) and (Slope[i - 1] > 0)):
                EncircledSPLIndexes.append(i - 1)

        CorrectedSPLDistribution_dB_Hz = []

        for i in range(len(SPLDistribution_dB)):
            if (i not in EncircledSlopeIndexes):
                CorrectedSPLDistribution_dB_Hz.append(SPLDistribution_dB[i])
            elif (i != len(SPLDistribution_dB) - 1):
                CorrectedSPLDistribution_dB_Hz.append(0.5*(SPLDistribution_dB[i - 1] + SPLDistribution_dB[i + 1]))
            else:
                CorrectedSPLDistribution_dB_Hz.append(SPLDistribution_dB[i - 1] + Slope[i - 1])

        Slope2 = [math.nan]*(len(SPLDistribution_dB) + 1)
        for i in range(3, len(CorrectedSPLDistribution_dB_Hz)):
            Slope2[i] = CorrectedSPLDistribution_dB_Hz[i] - CorrectedSPLDistribution_dB_Hz[i - 1]
        Slope2[2] = Slope2[3]
        Slope2[-1] = Slope2[-2]

        Slope2Average = [math.nan]*(len(SPLDistribution_dB) + 1)
        for i in range(2, len(Slope2) - 2):
            Slope2Average[i] = (Slope2[i] + Slope2[i + 1] + Slope2[i + 2])/3

        FinalCorrectedSPLDistribution_dB_Hz = CorrectedSPLDistribution_dB_Hz
        for i in range(3, len(CorrectedSPLDistribution_dB_Hz)):
            FinalCorrectedSPLDistribution_dB_Hz[i] = FinalCorrectedSPLDistribution_dB_Hz[i - 1] + Slope2Average[i - 1]

        Differences = []
        for i in range(len(FinalCorrectedSPLDistribution_dB_Hz)):
            Differences.append(SPLDistribution_dB[i] - FinalCorrectedSPLDistribution_dB_Hz[i])


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

    def PNLT(self):
        return self.PNL() + self.ToneCorrection()

    def AWeigthed(self):
      Ra = []
      Ra1k = (12194**2)*(1000**4)/(((1000**2) + (20.6**2))*((((1000**2) + (107.7**2))*((1000**2) + (737.9**2)))**0.5)*((1000**2) + (12194**2)))
      for Band_Hz in self.CentralBands_Hz:
        Ra.append((12194**2)*(Band_Hz**4)/(((Band_Hz**2) + (20.6**2))*((((Band_Hz**2) + (107.7**2))*((Band_Hz**2) + (737.9**2)))**0.5)*((Band_Hz**2) + (12194**2))))
      
      Gain_dB = []
      for RaValue in Ra:
        Gain_dB.append((20*math.log10(RaValue)) - (20*math.log10(Ra1k)))
      
      AWeightedSPL_dB = []
      for i in range(len(self.Spectrum_dB)):
        if (np.isnan(self.Spectrum_dB[i])):
          AWeightedSPL_dB.append(max([Gain_dB[i], 0]))
        else:
          AWeightedSPL_dB.append(max([self.Spectrum_dB[i] + Gain_dB[i], 0]))
      
      return AWeightedSPL_dB
    
    def AWeightedOASPL(self):
        AWeightedSpectrum = self.AWeigthed()
        IntensitiesSum = 0
        for i in range(len(AWeightedSpectrum)):
            if (not np.isnan(self.Spectrum_dB[i])):
                IntensitiesSum += 10**(AWeightedSpectrum[i]/10)
        return 10*math.log10(IntensitiesSum)
    
    def OASPL(self):
        IntensitiesSum = 0
        for i in range(len(self.Spectrum_dB)):
            if (not np.isnan(self.Spectrum_dB[i])):
                IntensitiesSum += 10**(self.Spectrum_dB[i]/10)
        return 10*math.log10(IntensitiesSum)

    def plot(self):
      WidthVec = []
      CentralBandsVec_Hz = []
      PreferedCentralBandsVec_Hz = []
      for i in range(len(self.StandardOneThirdOctaveBands)):
        CentralBand_Hz = self.StandardOneThirdOctaveBands[i][2]
        PreferedCentralBand_Hz = self.StandardOneThirdOctaveBands[i][3]
        Width = self.StandardOneThirdOctaveBands[i][1] - self.StandardOneThirdOctaveBands[i][0]
        WidthVec.append(Width)
        CentralBandsVec_Hz.append(CentralBand_Hz)
        PreferedCentralBandsVec_Hz.append(PreferedCentralBand_Hz)
    #   plt.grid()
      plt.bar(CentralBandsVec_Hz, height = self.Spectrum_dB, width = WidthVec, edgecolor = 'black', alpha = 0.5)#, color = 'none')
      plt.xscale("log")
      plt.xlabel("freq (Hz)")
      plt.ylabel("SPL (dB)")
      plt.xlim([10, 50000])
      # plt.xticks(PreferedCentralBandsVec_Hz)
    #   plt.show()
    
    def plotAWeightedComparison(self):
        WidthVec = []
        CentralBandsVec_Hz = []
        PreferedCentralBandsVec_Hz = []
        for i in range(len(self.StandardOneThirdOctaveBands)):
            CentralBand_Hz = self.StandardOneThirdOctaveBands[i][2]
            PreferedCentralBand_Hz = self.StandardOneThirdOctaveBands[i][3]
            Width = self.StandardOneThirdOctaveBands[i][1] - self.StandardOneThirdOctaveBands[i][0]
            WidthVec.append(Width)
            CentralBandsVec_Hz.append(CentralBand_Hz)
            PreferedCentralBandsVec_Hz.append(PreferedCentralBand_Hz)
        plt.bar(CentralBandsVec_Hz, height = self.Spectrum_dB, width = WidthVec)
        plt.bar(CentralBandsVec_Hz, height = self.AWeigthed(), width = WidthVec)
        plt.xscale("log")
        # plt.xticks(PreferedCentralBandsVec_Hz)
        plt.show()


def BroadbandNoise(TotalBladeArea_m2, AverageBladeCL_adim, TotalThrust_N, TipSpeed_m_s, DistanceToObserver_m, AngleNegativeThrustToObserverPositionVector_deg):
    PeakFrequency = (-240*math.log10(TotalThrust_N)) + (2.448*TipSpeed_m_s) + 942

    #ONLY WORKS FOR AVERAGE CL < 0.48
    LiftCoefficientFunction = 10*math.log10(AverageBladeCL_adim/0.4) 

    #c0 is the speed of sound at sea level standard conditions m/sec
    SpeedSoundSeaLevelISA_m_s = 344

    SoundPressureLevelAtOneThirdBandBeforeCorrection = (20*(math.log10((TipSpeed_m_s/SpeedSoundSeaLevelISA_m_s)**3))) \
                                                     + (10*math.log10(TotalBladeArea_m2*((math.cos(math.radians(AngleNegativeThrustToObserverPositionVector_deg))**2) + 0.1)/(DistanceToObserver_m**2))) \
                                                     + LiftCoefficientFunction + 130

    # [min, max, center, preferred] SOURCE: https://apmr.matelys.com/Standards/OctaveBands.html
    StandardOneThirdOctaveBands = [ [   13.9,    17.5,    15.6,    16.0], \
                                    [   17.5,    22.1,    19.7,    20.0], \
                                    [   22.1,    27.8,    24.8,    25.0], \
                                    [   27.8,    35.1,    31.3,    31.5], \
                                    [   35.1,    44.2,    39.4,    40.0], \
                                    [   44.2,    55.7,    49.6,    50.0], \
                                    [   55.7,    70.2,    62.5,    63.0], \
                                    [   70.2,    88.4,    78.7,    80.0], \
                                    [   88.4,   111.4,    99.2,   100.0], \
                                    [  111.4,   140.3,   125.0,   125.0], \
                                    [  140.3,   176.8,   157.5,   160.0], \
                                    [  176.8,   222.7,   198.4,   200.0], \
                                    [  222.7,   280.6,   250.0,   250.0], \
                                    [  280.6,   356.6,   315.0,   315.0], \
                                    [  356.6,   445.4,   396.9,   400.0], \
                                    [  445.4,   561.2,   500.0,   500.0], \
                                    [  561.2,   707.1,   630.0,   630.0], \
                                    [  707.1,   890.9,   793.7,   800.0], \
                                    [  890.9,  1122.5,  1000.0,  1000.0], \
                                    [ 1122.5,  1414.2,  1259.9,  1250.0], \
                                    [ 1414.2,  1781.8,  1587.4,  1600.0], \
                                    [ 1781.8,  2244.9,  2000.0,  2000.0], \
                                    [ 2244.9,  2828.4,  2519.8,  2500.0], \
                                    [ 2828.4,  3563.6,  3174.8,  3150.0], \
                                    [ 3563.6,  4489.8,  4000.0,  4000.0], \
                                    [ 4489.8,  5656.9,  5039.7,  5000.0], \
                                    [ 5656.9,  7127.2,  6349.6,  6300.0], \
                                    [ 7127.2,  8979.7,  8000.0,  8000.0], \
                                    [ 8979.7, 11313.7, 10079.4, 10000.0], \
                                    [11313.7, 14254.4, 12699.2, 12500.0], \
                                    [14254.4, 17959.4, 16000.0, 16000.0], \
                                    [17959.4, 22627.4, 20158.7, 20000.0]]
    
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

def RotationalNoiseSteadyUnsteadyLoading(NumberOfBlades, ForwardMach, BladeTipMach, RotorSpeed_1_s, DistanceToObserver_m, RotorRadius_m, EffectiveRotorRadius_m, TotalThrust_N, AngleRotorPlaneToObserverPositionVector_deg, RotorAzimuth_deg):
    SpeedSoundSeaLevelISA_m_s = 344
    AirDensityAtSeaLevel_Kg_m3 = 1.225
    
    if ((AngleRotorPlaneToObserverPositionVector_deg < 0) or (AngleRotorPlaneToObserverPositionVector_deg > 80)):
        print("Angle to observer out of bounds")

    HarmonicValuesUsed_1_s = []
    Harmonics = range(1, 32)
    HarmonicsUsed = []
    for Harmonic in Harmonics:
        HarmonicValue_1_s = Harmonic*NumberOfBlades*RotorSpeed_1_s/(1 - (ForwardMach*math.cos(math.radians(AngleRotorPlaneToObserverPositionVector_deg))))
        if (HarmonicValue_1_s < 13.9): #lowest frequency supported by one-third octave bands
            continue
        
        if (HarmonicValue_1_s > 22627.4): #highest frequency supported by one-third octave bands
            break
        HarmonicValuesUsed_1_s.append(HarmonicValue_1_s)
        HarmonicsUsed.append(Harmonic)
    # print(HarmonicsUsed)

    Mach05Curves = [[0, 10, 20, 30, 40, 50, 60, 70, 80], [[(2.9893,148.04), (4.5824,145.097), (6.4132,142.43), (8.6354,139.917), (10.8338,138.035), (13.1468,136.569), (15.2288,135.626), (18.0942,134.497), (21.1283,133.522), (23.8398,132.639), (26.4204,132.034), (29.0164,131.398), (31.5509,130.84), (33.8779,130.42), (36.5199,129.876), (38.8393,129.41), (41.4966,128.913), (43.5855,128.585), (46.6728,128.026), (49.9135,127.573)],      \
                                                          [(3.0644,151.886), (4.4933,149.672), (6.0803,147.739), (8.0939,145.756), (9.973,144.079), (11.8821,142.829), (13.864,141.812), (15.8822,140.989), (18.0343,140.264), (20.1438,139.54), (22.485,138.778), (24.4236,138.2), (26.8011,137.646), (29.0381,137.227), (31.6105,136.696), (33.9573,136.289), (36.231,135.857), (38.5167,135.547), (41.54,135.065), (49.9167,133.784)],           \
                                                          [(3.0261,155.289), (4.9337,152.577), (6.7637,150.557), (8.547,149.061), (10.6064,147.764), (12.7883,146.713), (14.824,145.924), (17.0824,145.104), (19.2793,144.346), (22.0828,143.602), (24.7711,142.858), (27.5131,142.191), (30.1013,141.648), (32.5897,141.074), (35.1088,140.546), (37.5664,140.079), (40.1547,139.505), (42.7582,139.069), (46.5136,138.4), (49.8236,137.856)],     \
                                                          [(3.0545,157.067), (5.5231,153.992), (7.8676,151.979), (10.1767,150.551), (12.7389,149.299), (14.8554,148.426), (16.795,147.745), (19.1339,147.002), (21.2307,146.444), (23.2699,145.947), (26.2689,145.192), (29.2834,144.333), (31.901,143.688), (34.6008,143.072), (36.8119,142.593), (39.5987,142.055), (42.2693,141.536), (44.6738,141.105), (47.3637,140.644), (49.8214,140.271)],  \
                                                          [(3.115,157.829), (4.8298,155.825), (6.5131,154.39), (8.1885,153.187), (10.0711,152.075), (11.9073,151.132), (13.9126,150.174), (16.0636,149.278), (18.1531,148.443), (20.2502,147.715), (22.6083,146.987), (24.3672,146.492), (26.6791,145.856), (28.6914,145.406), (30.7498,144.879), (33.7145,144.166), (35.9802,143.653), (38.5377,143.079), (43.5299,141.962), (49.8199,140.749)],   \
                                                          [(3.0991,158.104), (5.5705,155.595), (7.9433,153.844), (9.9742,152.704), (12.1695,151.576), (14.0109,150.779), (15.9621,149.944), (18.8216,148.925), (20.9006,148.225), (23.2294,147.573), (25.4667,146.946), (27.7466,146.441), (30.5872,145.837), (32.4648,145.369), (34.5922,144.925), (36.7379,144.494), (39.225,143.976), (41.9619,143.507), (45.1377,142.927), (49.7274,142.258)],  \
                                                          [(3.0987,158.447), (5.8262,155.778), (8.3024,154.222), (10.4308,153.033), (13.8333,151.463), (16.4186,150.42), (19.6988,149.204), (22.4788,148.368), (24.9904,147.741), (27.7275,147.039), (29.9952,146.497), (32.653,145.906), (35.1217,145.462), (37.371,145.079), (39.7178,144.61), (41.4856,144.241), (43.4727,143.895), (45.466,143.525), (47.7213,143.142), (49.7327,142.845)],     \
                                                          [(3.0987,158.447), (5.8262,155.778), (8.3024,154.222), (10.4308,153.033), (13.8333,151.463), (16.4186,150.42), (19.6988,149.204), (22.4788,148.368), (24.9904,147.741), (27.7275,147.039), (29.9952,146.497), (32.653,145.906), (35.1217,145.462), (37.371,145.079), (39.7178,144.61), (41.4856,144.241), (43.4727,143.895), (45.466,143.525), (47.7213,143.142), (49.7327,142.845)],     \
                                                          [(3.0987,158.447), (5.8262,155.778), (8.3024,154.222), (10.4308,153.033), (13.8333,151.463), (16.4186,150.42), (19.6988,149.204), (22.4788,148.368), (24.9904,147.741), (27.7275,147.039), (29.9952,146.497), (32.653,145.906), (35.1217,145.462), (37.371,145.079), (39.7178,144.61), (41.4856,144.241), (43.4727,143.895), (45.466,143.525), (47.7213,143.142), (49.7327,142.845)],     \
                                                         ]]
    Mach06Curves = [[0, 10, 20, 30, 40, 50, 80],         [[(2.9188,151.241), (5.6034,148.273), (8.2538,145.838), (10.9725,143.597), (13.6404,141.447), (16.2117,139.945), (18.6769,138.756), (21.5085,137.576), (24.4394,136.577), (26.5503,135.889), (29.2128,135.239), (31.8285,134.649), (33.9065,134.284), (36.0531,133.823), (38.4522,133.417), (41.1722,133.009), (43.5123,132.699), (45.5802,132.391), (47.3856,132.087), (48.8452,131.9)],  \
                                                          [(2.9474,155.321), (4.7188,153.388), (6.4186,151.809), (8.3112,150.251), (10.362,148.688), (12.2689,147.3), (14.0204,146.1), (15.6344,145.284), (16.9965,144.654), (19.2283,143.761), (21.4634,142.953), (23.7749,142.285), (25.9663,141.677), (28.358,141.135), (30.664,140.723), (33.0666,140.282), (35.0831,139.9), (37.6437,139.487), (41.0811,138.925), (48.8667,137.772)],          \
                                                          [(2.9039,158.213), (5.5666,155.509), (8.0157,153.67), (10.4324,152.125), (12.6022,150.881), (14.8573,149.7), (17.1674,148.544), (19.6421,147.516), (22.0432,146.498), (23.6623,146.147), (25.6873,145.524), (27.8475,145.039), (30.1035,144.602), (32.9151,144.164), (35.827,143.728), (37.974,143.443), (39.6964,143.193), (42.1287,142.931), (45.758,142.544), (49.2632,142.248)],      \
                                                          [(3.1004,160.121), (5.6503,157.363), (8.3373,155.343), (11.6902,153.481), (14.3373,152.204), (16.6504,151.274), (18.7388,150.478), (20.6876,149.801), (23.8573,148.862), (25.9521,148.298), (29.2367,147.485), (31.895,146.873), (34.615,146.278), (36.6239,145.866), (38.5945,145.406), (41.1134,144.883), (43.8334,144.288), (46.2359,143.87), (48.1671,143.533), (49.4884,143.258)],   \
                                                          [(3.2185,160.713), (6.5651,157.399), (9.1755,155.672), (11.1562,154.535), (13.5153,153.524), (15.6598,152.625), (18.4376,151.528), (21.1853,150.566), (23.0654,149.971), (25.6556,149.317), (27.9342,148.752), (30.5737,147.982), (33.154,147.367), (36.1722,146.684), (38.7429,146.049), (40.7972,145.576), (42.9782,145.067), (45.1003,144.654), (47.1153,144.257), (49.2569,143.825)], \
                                                          [(3.2509,160.933), (5.8044,158.37), (8.0798,156.754), (10.0027,155.537), (11.9251,154.399), (13.7874,153.551), (15.767,152.589), (17.9699,151.691), (19.9775,150.924), (22.9972,149.989), (25.8306,149.322), (28.712,148.753), (31.2337,148.174), (33.8528,147.56), (36.4519,147.042), (38.9146,146.579), (41.6598,146.007), (44.463,145.514), (46.9936,145.052), (49.3099,144.683)],     \
                                                          [(3.1233,160.352), (5.1951,158.124), (7.4719,156.416), (9.7961,154.955), (11.8609,153.854), (14.1457,152.833), (16.1232,151.976), (18.2225,151.22), (20.4205,150.369), (22.7643,149.693), (25.4027,148.926), (27.9547,148.28), (30.0526,147.745), (32.4939,147.169), (34.9597,146.57), (37.5236,145.973), (39.4498,145.482), (41.6208,145.022), (45.1046,144.228), (49.2009,143.425)],    \
                                                         ]]
    Mach07Curves = [[0, 10, 20, 30, 40, 50, 80],         [[(2.9784,153.415), (5.0102,153.286), (6.6284,152.662), (8.2189,151.758), (9.4656,150.896), (11.6642,149.168), (13.591,147.724), (15.4803,146.351), (17.6433,145.075), (20.3317,143.758), (23.0119,142.624), (25.5622,141.645), (27.4427,141.056), (29.023,140.578), (31.0688,139.947), (33.0833,139.388), (35.8314,138.735), (38.4084,138.1), (44.0748,136.849), (49.7633,135.808)],      \
                                                          [(3.0221,158.774), (4.5049,158.561), (5.4208,158.157), (6.5462,157.591), (7.9603,156.65), (9.077,155.829), (10.8326,154.26), (13.3681,152.082), (14.9588,150.823), (17.5941,149.343), (20.9893,147.653), (23.0879,146.708), (25.0002,146.257), (28.2834,145.296), (32.1465,144.404), (35.9683,143.757), (39.0029,143.264), (44.1755,142.551), (47.6107,142.101), (50.0941,141.847)],      \
                                                          [(3.0699,161.709), (4.507,161.117), (6.4046,159.924), (8.0423,158.598), (10.151,157.09), (12.2057,155.611), (14.721,154.195), (17.7051,152.813), (20.1845,151.86), (22.8247,150.974), (26.2965,149.997), (29.3112,149.312), (33.1352,148.472), (35.8022,147.942), (38.2299,147.481), (40.3338,147.099), (42.7147,146.698), (45.034,146.309), (46.7831,146.006), (49.2025,145.606)],       \
                                                          [(2.9624,163.081), (4.6202,162.214), (6.1654,161.049), (7.7483,159.787), (9.6892,158.171), (11.7514,156.634), (13.2024,155.698), (16.2573,154.358), (19.2665,153.286), (22.4342,152.22), (25.9369,151.229), (29.1739,150.521), (31.9945,149.932), (34.3563,149.436), (36.5332,149.091), (38.4236,148.723), (41.3606,148.112), (44.0122,147.582), (47.2721,146.983), (50.3852,146.439)],   \
                                                          [(3.0303,163.537), (4.9148,161.393), (6.7152,159.829), (8.5311,158.615), (10.0917,157.703), (11.8072,156.797), (14.1836,155.722), (16.3257,154.794), (18.3886,154.018), (20.9565,153.145), (23.7753,152.36), (26.3414,151.643), (29.1198,151.011), (31.5102,150.404), (33.7836,149.87), (35.8615,149.483), (38.1736,148.951), (40.7961,148.391), (44.4281,147.676), (50.4092,146.586)],   \
                                                          [(3.0303,163.537), (4.9148,161.393), (6.7152,159.829), (8.5311,158.615), (10.0917,157.703), (11.8072,156.797), (14.1836,155.722), (16.3257,154.794), (18.3886,154.018), (20.9565,153.145), (23.7753,152.36), (26.3414,151.643), (29.1198,151.011), (31.5102,150.404), (33.7836,149.87), (35.8615,149.483), (38.1736,148.951), (40.7961,148.391), (44.4281,147.676), (50.4092,146.586)],   \
                                                          [(3.1264,162.159), (5.2604,159.772), (7.6063,157.701), (9.5584,156.45), (11.2624,155.373), (13.4905,154.194), (15.3151,153.338), (17.2627,152.488), (19.7037,151.532), (23.5934,150.325), (26.5249,149.512), (29.1931,148.875), (32.1229,148.217), (35.2841,147.505), (37.9204,146.959), (40.402,146.5), (42.3752,146.113), (44.5332,145.703), (46.9833,145.304), (49.1256,144.924)],     \
                                                         ]]
    Mach09Curves = [[0, 10, 20, 30, 40, 50, 60, 70, 80], [[(3.0839,157.788), (3.8546,158.588), (5.0648,159.553), (7.105,160.799), (8.5214,161.415), (9.7662,161.762), (10.8281,162.088), (12.0359,162.288), (13.3289,162.437), (16.2319,162.729), (19.6098,162.791), (25.9872,162.772), (28.3277,162.532), (31.7163,162.124), (35.6902,161.681), (38.3594,161.287), (40.907,160.968), (43.4301,160.651), (46.4893,160.175), (50.036,159.616)],      \
                                                          [(2.8955,163.937), (3.6788,164.86), (4.6572,165.805), (6.1604,166.912), (7.6872,167.748), (8.6151,168.101), (9.9085,168.374), (11.1037,168.451), (13.2499,168.485), (15.774,168.464), (19.0172,168.33), (21.7847,168.181), (24.9176,167.876), (27.8186,167.502), (30.7802,167.004), (34.9119,166.336), (38.2511,165.707), (42.2361,164.942), (46.0265,164.305), (49.6704,163.646)],           \
                                                          [(2.9959,167.522), (4.1587,168.465), (5.6981,169.432), (7.0595,170.029), (8.3279,170.316), (9.5111,170.388), (11.0856,170.312), (13.1126,169.995), (15.2699,169.583), (18.3946,169.012), (22.5015,168.097), (25.8635,167.274), (28.7494,166.49), (31.4356,165.725), (33.1241,165.228), (36.2171,164.363), (38.6424,163.681), (41.221,162.887), (44.5748,161.862), (50.1544,160.236)],     \
                                                          [(3.0236,169.045), (3.9853,169.51), (5.1539,169.846), (6.537,169.96), (7.804,169.813), (9.1783,169.585), (10.5598,169.171), (12.5468,168.358), (14.4565,167.374), (16.9806,166.333), (19.7038,165.101), (22.381,163.901), (24.3292,162.948), (26.4082,162.039), (29.1398,161.056), (31.4958,160.298), (33.6679,159.714), (37.1838,158.981), (42.4428,158.014), (49.967,156.772)],  \
                                                          [(3.0236,169.045), (4.1449,168.947), (5.3038,168.615), (6.3242,168.207), (7.3825,167.628), (8.3486,167.066), (9.6903,166.202), (11.5147,164.956), (13.3163,163.788), (16.706,161.969), (18.6774,161.062), (21.2167,159.989), (24.0178,159.098), (26.8882,158.252), (30.3195,157.474), (33.252,156.861), (37.0832,156.215), (40.6225,155.591), (45.437,154.974), (50.0212,154.392)],   \
                                                          [(2.9903,168.206), (4.1311,166.957), (5.326,165.862), (6.8744,164.761), (8.4457,163.66), (10.1169,162.556), (12.2028,161.398), (14.2503,160.257), (16.3294,159.395), (18.2553,158.691), (20.4196,158.045), (23.5277,157.086), (26.5978,156.268), (30.2597,155.532), (34.6053,154.799), (38.4518,154.138), (41.7762,153.548), (44.8013,153.088), (47.4504,152.729), (50.1379,152.369)],  \
                                                          [(3.0533,166.199), (4.3551,164.87), (6.5771,163.025), (8.4781,161.684), (10.4561,160.435), (12.5111,159.247), (14.2749,158.282), (16.899,157.27), (19.0631,156.577), (21.9795,155.715), (24.4815,155.016), (27.3832,154.372), (30.2544,153.791), (33.4633,153.141), (36.4269,152.62), (39.2216,152.165), (41.9395,151.71), (44.458,151.353), (47.3759,150.989), (50.0787,150.597)],     \
                                                          [(2.8512,165.394), (3.8109,164.216), (5.0021,162.96), (6.8507,161.396), (9.2723,159.747), (11.6096,158.495), (14.0445,157.216), (16.0417,156.365), (18.7091,155.378), (22.2788,154.375), (25.5564,153.525), (28.5786,152.927), (31.8809,152.2), (35.5492,151.515), (38.4007,150.92), (41.143,150.475), (43.032,150.144), (45.2504,149.832), (47.7855,149.44), (50.1743,149.05 )],     \
                                                          [(2.7969,164.915), (3.5196,163.883), (4.532,162.65), (5.7284,161.433), (7.5433,159.93), (9.0983,158.883), (11.1951,157.787), (12.9733,156.931), (15.8733,155.801), (17.8358,155.079), (19.8274,154.377), (22.2054,153.451), (24.9706,152.558), (28.2103,151.715), (31.7791,150.905), (35.3196,150.331), (39.0342,149.754), (42.865,149.154), (46.6187,148.655), (50.2368,148.138)],     \
                                                         ]]

    EffectiveTipMach = (BladeTipMach + (ForwardMach*math.sin(math.radians(RotorAzimuth_deg))))/(1 - (ForwardMach*math.cos(math.radians(AngleRotorPlaneToObserverPositionVector_deg))))
    # print(EffectiveTipMach)
    if EffectiveTipMach < 0.5:
        LowerMachCurves = Mach05Curves
        UpperMachCurves = Mach05Curves
        LowerMach = 0.5
        UpperMach = 0.5
        # print("Effective Mach out of bounds (< 0.5)")
    elif EffectiveTipMach < 0.6:
        LowerMachCurves = Mach05Curves
        UpperMachCurves = Mach06Curves
        LowerMach = 0.5
        UpperMach = 0.6
    elif EffectiveTipMach < 0.7:
        LowerMachCurves = Mach06Curves
        UpperMachCurves = Mach07Curves
        LowerMach = 0.6
        UpperMach = 0.7
    elif EffectiveTipMach <= 0.9:
        LowerMachCurves = Mach07Curves
        UpperMachCurves = Mach09Curves
        LowerMach = 0.7
        UpperMach = 0.9
    else:
        LowerMachCurves = Mach09Curves
        UpperMachCurves = Mach09Curves
        LowerMach = 0.9
        UpperMach = 0.9
        # print("Effective Mach out of bounds (> 0.9)")

    for i in range(len(LowerMachCurves[0]) - 1):
        if (AngleRotorPlaneToObserverPositionVector_deg >= LowerMachCurves[0][i]) and (AngleRotorPlaneToObserverPositionVector_deg < LowerMachCurves[0][i + 1]):
            LowerBetaLowerMachCurve = LowerMachCurves[1][i]
            UpperBetaLowerMachCurve = LowerMachCurves[1][i + 1]
            LowerBetaLowerMach = LowerMachCurves[0][i]
            UpperBetaLowerMach = LowerMachCurves[0][i + 1]
    
    for i in range(len(UpperMachCurves[0]) - 1):
        if (AngleRotorPlaneToObserverPositionVector_deg >= UpperMachCurves[0][i]) and (AngleRotorPlaneToObserverPositionVector_deg < UpperMachCurves[0][i + 1]):
            LowerBetaUpperMachCurve = UpperMachCurves[1][i]
            UpperBetaUpperMachCurve = UpperMachCurves[1][i + 1]
            LowerBetaUpperMach = UpperMachCurves[0][i]
            UpperBetaUpperMach = UpperMachCurves[0][i + 1]

    SoundPressureLevelAtOneThirdBand = []
    for Harmonic in HarmonicsUsed:
        HarmonicBladePassage = Harmonic*NumberOfBlades
        # print(HarmonicBladePassage)
        for i in range(len(LowerBetaLowerMachCurve) - 1):
            if HarmonicBladePassage < LowerBetaLowerMachCurve[0][0]:
                LowerBetaLowerMachValue = LinearInterpolate(LowerBetaLowerMachCurve[0][0], LowerBetaLowerMachCurve[1][0], LowerBetaLowerMachCurve[0][1], LowerBetaLowerMachCurve[1][1], HarmonicBladePassage)
            elif HarmonicBladePassage >= LowerBetaLowerMachCurve[-1][0]:
                LowerBetaLowerMachValue = LinearInterpolate(LowerBetaLowerMachCurve[-2][0], LowerBetaLowerMachCurve[-1][0], LowerBetaLowerMachCurve[-2][1], LowerBetaLowerMachCurve[-1][1], HarmonicBladePassage)
            elif (HarmonicBladePassage >= LowerBetaLowerMachCurve[i][0]) and (HarmonicBladePassage < LowerBetaLowerMachCurve[i + 1][0]):
                LowerBetaLowerMachValue = LinearInterpolate(LowerBetaLowerMachCurve[i][0], LowerBetaLowerMachCurve[i + 1][0], LowerBetaLowerMachCurve[i][1], LowerBetaLowerMachCurve[i + 1][1], HarmonicBladePassage)
        for i in range(len(LowerBetaUpperMachCurve) - 1):
            if HarmonicBladePassage < LowerBetaUpperMachCurve[0][0]:
                LowerBetaUpperMachValue = LinearInterpolate(LowerBetaUpperMachCurve[0][0], LowerBetaUpperMachCurve[1][0], LowerBetaUpperMachCurve[0][1], LowerBetaUpperMachCurve[1][1], HarmonicBladePassage)
            elif HarmonicBladePassage >= LowerBetaUpperMachCurve[-1][0]:
                LowerBetaUpperMachValue = LinearInterpolate(LowerBetaUpperMachCurve[-2][0], LowerBetaUpperMachCurve[-1][0], LowerBetaUpperMachCurve[-2][1], LowerBetaUpperMachCurve[-1][1], HarmonicBladePassage)
            elif (HarmonicBladePassage >= LowerBetaUpperMachCurve[i][0]) and (HarmonicBladePassage < LowerBetaUpperMachCurve[i + 1][0]):
                LowerBetaUpperMachValue = LinearInterpolate(LowerBetaUpperMachCurve[i][0], LowerBetaUpperMachCurve[i + 1][0], LowerBetaUpperMachCurve[i][1], LowerBetaUpperMachCurve[i + 1][1], HarmonicBladePassage)
        for i in range(len(UpperBetaLowerMachCurve) - 1):
            if HarmonicBladePassage < UpperBetaLowerMachCurve[0][0]:
                UpperBetaLowerMachValue = LinearInterpolate(UpperBetaLowerMachCurve[0][0], UpperBetaLowerMachCurve[1][0], UpperBetaLowerMachCurve[0][1], UpperBetaLowerMachCurve[1][1], HarmonicBladePassage)
            elif HarmonicBladePassage >= UpperBetaLowerMachCurve[-1][0]:
                UpperBetaLowerMachValue = LinearInterpolate(UpperBetaLowerMachCurve[-2][0], UpperBetaLowerMachCurve[-1][0], UpperBetaLowerMachCurve[-2][1], UpperBetaLowerMachCurve[-1][1], HarmonicBladePassage)
            elif (HarmonicBladePassage >= UpperBetaLowerMachCurve[i][0]) and (HarmonicBladePassage < UpperBetaLowerMachCurve[i + 1][0]):
                UpperBetaLowerMachValue = LinearInterpolate(UpperBetaLowerMachCurve[i][0], UpperBetaLowerMachCurve[i + 1][0], UpperBetaLowerMachCurve[i][1], UpperBetaLowerMachCurve[i + 1][1], HarmonicBladePassage)
        for i in range(len(UpperBetaUpperMachCurve) - 1):
            if HarmonicBladePassage < UpperBetaUpperMachCurve[0][0]:
                UpperBetaUpperMachValue = LinearInterpolate(UpperBetaUpperMachCurve[0][0], UpperBetaUpperMachCurve[1][0], UpperBetaUpperMachCurve[0][1], UpperBetaUpperMachCurve[1][1], HarmonicBladePassage)
            elif HarmonicBladePassage >= UpperBetaUpperMachCurve[-1][0]:
                UpperBetaUpperMachValue = LinearInterpolate(UpperBetaUpperMachCurve[-2][0], UpperBetaUpperMachCurve[-1][0], UpperBetaUpperMachCurve[-2][1], UpperBetaUpperMachCurve[-1][1], HarmonicBladePassage)
            elif (HarmonicBladePassage >= UpperBetaUpperMachCurve[i][0]) and (HarmonicBladePassage < UpperBetaUpperMachCurve[i + 1][0]):
                UpperBetaUpperMachValue = LinearInterpolate(UpperBetaUpperMachCurve[i][0], UpperBetaUpperMachCurve[i + 1][0], UpperBetaUpperMachCurve[i][1], UpperBetaUpperMachCurve[i + 1][1], HarmonicBladePassage)

        LowerMachValue = LinearInterpolate(LowerBetaLowerMach, UpperBetaLowerMach, LowerBetaLowerMachValue, LowerBetaLowerMachValue, AngleRotorPlaneToObserverPositionVector_deg)
        UpperMachValue = LinearInterpolate(LowerBetaUpperMach, UpperBetaUpperMach, LowerBetaUpperMachValue, UpperBetaUpperMachValue, AngleRotorPlaneToObserverPositionVector_deg)

        FinalValue = LinearInterpolate(LowerMach, UpperMach, LowerMachValue, UpperMachValue, EffectiveTipMach)

        DeltaSPL = FinalValue
        # print(DeltaSPL)
        SoundPressureLevelAtOneThirdBand.append((20*math.log10(EffectiveRotorRadius_m/DistanceToObserver_m)) \
                                              + (20*math.log10(TotalThrust_N/(AirDensityAtSeaLevel_Kg_m3*(SpeedSoundSeaLevelISA_m_s**2)*(RotorRadius_m**2)))) \
                                              + DeltaSPL)

    return HarmonicValuesUsed_1_s, SoundPressureLevelAtOneThirdBand
    


def CalculateRotorEffectiveRadius(ThrustCoefficient, RotorRadius_m):
    #Method Based on Helicopter Performance, Stability and Control - RAYMOND W. PROUlY (1986), pgs. 34 and 71
    if (ThrustCoefficient <= 0.006):
        return RotorRadius_m*(1 - (0.06/RotorRadius_m))
    else:
        return RotorRadius_m*(1 - ((((2.27*ThrustCoefficient) - 0.01)**0.5)/RotorRadius_m))

def LinearInterpolate(x0, x1, y0, y1, x):
    if x1 - x0 == 0:
        return (y0 + y1)/2
    return y0 + ((x - x0)*(y1 - y0)/(x1 - x0))


def SumSoundPressureLevels(SoundPressureLevel1_dB, SoundPressureLevel2_dB):
    # Method based on https://personalpages.manchester.ac.uk/staff/richard.baker/BasicAcoustics/index.html
    Intensity1_W_m2 = (10**(-12))*(10**(SoundPressureLevel1_dB/10))
    Intensity2_W_m2 = (10**(-12))*(10**(SoundPressureLevel2_dB/10))

    IntensitySum_W_m2 = Intensity1_W_m2 + Intensity2_W_m2

    SoundPressureLevelSum_dB = 10*math.log10(IntensitySum_W_m2/(10**(-12)))

    return(SoundPressureLevelSum_dB)

# x, y = RotationalNoiseSteadyUnsteadyLoading(2, 0, 0.9, 3.14, 47.2, 1.05, .934, 485, 0, 0)
# x, y = RotationalNoiseSteadyUnsteadyLoading(5, 0, 0.61, 3.5, 61.6, 9.45, 8.5, 69420, 5, 0)
# (NumberOfBlades, ForwardMach, BladeTipMach, RotorSpeed_1_s, DistanceToObserver_m, RotorRadius_m, EffectiveRotorRadius_m, TotalThrust_N, AngleRotorPlaneToObserverPositionVector_deg, RotorAzimuth_deg)
# print(x[:4])
# print(y[:4])

# def SumMultipleSoundPressureLevels(SoundPressureLevelVec_dB):
#     # Method based on https://personalpages.manchester.ac.uk/staff/richard.baker/BasicAcoustics/index.html
#     IntensitiesSum = 0
#     for i in range(len(SoundPressureLevelVec_dB)):
#         IntensitiesSum += 10**(SoundPressureLevelVec_dB[i]/10)

#     return 10*math.log10(IntensitiesSum)


#bands, spl = BroadbandNoise(18.6, 0.438, 69420, 208, 61.6, 85) #Example 1 main rotor on tm-80200
# print(CalculatePNLT([int(input("SPL: "))]*24))
# print(CalculatePNLT([16.0, 20.0, 25.0, 31.5, 40.0, 50.0, 63.0, 80.0, 100.0, 125.0, 160.0, 200.0, 250.0, 315.0, 400.0, 500.0, 630.0, 800.0, 1000.0, 1250.0, 1600.0, 2000.0, 2500.0, 3150.0, 4000.0, 5000.0, 6300.0, 8000.0, 10000.0, 12500.0, 16000.0, 20000.0]))
# print(BroadbandNoise(3.44, 0.182, 5206, 202, 62.2, 10)) #Example 1 tail rotor on tm-80200

#print(SumSoundPressureLevels(80, 80))
#print(SumMultipleSoundPressureLevels([80, 80, 80, 80]))


# Spectre = OneThirdSpectrum()
# print(Spectre.Spectrum_dB)
# Spectre.SumToSpectrum([20, 125, 10000], [65, 70, 68])
# print(Spectre.Spectrum_dB)
# Spectre.SumToSpectrum([20, 125], [80, 80])
# print(Spectre.Spectrum_dB)
# print(Spectre.PNL())
# print(Spectre.ToneCorrection())
# print(Spectre.PNLT())
# print(Spectre.OASPL())
# print(10*math.log10(1))

if __name__ == "__main__":
    Spectre = OneThirdSpectrum()
    bands, spl = BroadbandNoise(18.6, 0.438, 69420, 208, 61.6, 85) #Example 1 main rotor on tm-80200
    Spectre.SumToSpectrum(bands, spl)
    bands, spl = RotationalNoiseSteadyUnsteadyLoading(2, 0, 0.9, 3.14, 47.2, 1.05, .934, 485, 0, 0)
    Spectre.SumToSpectrum(bands, spl)
    print(Spectre.CentralBands_Hz)
    print(Spectre.Spectrum_dB)
    Spectre.plotAWeightedComparison()