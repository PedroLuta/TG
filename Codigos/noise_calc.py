import math

def broadband_noise(TotalBladeArea_m2, AverageBladeCL_adim, TotalThrust_N, TipSpeed_m_s, DistanceToObserver_m, AngleNegativeThrustToObserverPositionVector_deg):
    PeakFrequency = (-240*math.log10(TotalThrust_N)) + (2.448*TipSpeed_m_s) + 942
    #CHECAAR SE ESSE É O LOG CORRETO TENTANDO BATER COM O EXEMPLO
    
    StandardOneThirdOctaveBands = []
    
    S13 = 0

    LiftCoefficientFunction = 10*math.log10(AverageBladeCL_adim/0.4) #ONLY WORKS FOR AVERAGE CL < 0.48
    
    SpeedSoundSeaLevelISA_m_s = 340
    SoundPressureLevelAtOneThirdBand = (20*(math.log10(TipSpeed_m_s/SpeedSoundSeaLevelISA_m_s)**3)) + (10*math.log10(TotalBladeArea_m2/(DistanceToObserver_m**2))*((math.cos(math.radians(AngleNegativeThrustToObserverPositionVector_deg)) + 0.1))) + S13 + LiftCoefficientFunction + 130
    #c0 is the speed of sound at sea level standard conditions m/sec

    # One-Third Octave Bands
    # Min - Max           -> Center (preferred)
    # 13.9 - 17.5         -> 15.6 (16)
    # 17.5 - 22.1         -> 19.7 (20)
    # 22.1 - 27.8         -> 24.8 (25)
    # 27.8 - 35.1         -> 31.3 (31.5)
    # 35.1 - 44.2         -> 39.4 (40)
    # 44.2 - 55.7         -> 49.6 (50)
    # 55.7 - 70.2         -> 62.5 (63)
    # 70.2 - 88.4         -> 78.7 (80)
    # 88.4 - 111.4        -> 99.2 (100)
    # 111.4 - 140.3       -> 125
    # 140.3 - 176.8       -> 157.5 (160)
    # 176.8 - 222.7       -> 198.4 (200)
    # 222.7 - 280.6       -> 250
    # 280.6 - 353.6       -> 315
    # 356.6 - 445.4       -> 396.9 (400)
    # 445.4 - 561.2       -> 500
    # 561.2 - 707.1       -> 630
    # 707.1 - 890.9       -> 793.7 (800)
    # 890.9 - 1122.5      -> 1000
    # 1122.5 - 1414.2     -> 1259.9 (1250)
    # 1414.2 - 1781.8     -> 1587.4 (1600)
    # 1781.8 - 2244.9     -> 2000
    # 2244.9 - 2828.4     -> 2519.8 (2500)
    # 2828.4 - 3563.6     -> 3174.8 (3150)
    # 3563.6 - 4489.8     -> 4000
    # 4489.8 - 5656.9     -> 5039.7 (5000)
    # 5656.9 - 7127.2     -> 6349.6 (6300)
    # 7127.2 - 8979.7     -> 8000
    # 8979.7 - 11313.7    -> 10079.4 (10000)
    # 11313.7 - 14254.4   -> 12699.2 (12500)
    # 14254.4 - 17959.4   -> 16000
    # 17959.4 - 22627.4   -> 20158.7 (20000)