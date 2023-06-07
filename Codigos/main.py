import noise_calc
import prop_simulate
import prop_design
import optimization_NSGA2
import xfoil_interface
import numpy as np
import matplotlib.pyplot as plt

def Evaluation(Chromossome):

    Diameter_m = 3
    SpinnerCutoff = 0.2
    NumberOfBlades = 2
    ObjectiveThrust_N = 4000
    AxialVelocity_m_s = 0
    ClPolar = "ClarkYAutoClPolar4.dat"
    CdPolar = "ClarkYAutoCdPolar4.dat"
    NumberOfStations = 11

    # Diameter_m = Chromossome[0]
    Croot_adim = Chromossome[0]
    Cmax_adim = Chromossome[1]
    smax_adim = Chromossome[2]
    Ctip_adim = Chromossome[3]
    Pitch_m = Chromossome[4]
    Colective_deg = Chromossome[5]


    #c_R = (a*r**3 + b*r**2 + c*r + d)**0.5
    a = -(2*Cmax_adim**2*smax_adim - Cmax_adim**2 + Croot_adim**2*smax_adim**2 - 2*Croot_adim**2*smax_adim + Croot_adim**2 - Ctip_adim**2*smax_adim**2)/(smax_adim**2*(smax_adim - 1)**2)
    b = -(- 3*Cmax_adim**2*smax_adim**2 + Cmax_adim**2 - 2*Croot_adim**2*smax_adim**3 + 3*Croot_adim**2*smax_adim**2 - Croot_adim**2 + 2*Ctip_adim**2*smax_adim**3)/(smax_adim**2*(smax_adim - 1)**2)
    c = -(3*Cmax_adim**2*smax_adim - 2*Cmax_adim**2 + Croot_adim**2*smax_adim**3 - 3*Croot_adim**2*smax_adim + 2*Croot_adim**2 - Ctip_adim**2*smax_adim**3)/(smax_adim*(smax_adim - 1)**2)
    d = Croot_adim**2

    S = np.linspace(0, 1, NumberOfStations, endpoint = True)
    RadialStations_adim = [SpinnerCutoff + ((1 - SpinnerCutoff)*s) for s in S]
    RadialStations_m = [r*Diameter_m/2 for r in RadialStations_adim]

    ChordDistribution_m = [((a*s**3 + b*s**2 + c*s + d)**0.5)*Diameter_m/2 for s in S]
    plt.plot(S, ChordDistribution_m)
    plt.plot(RadialStations_adim, ChordDistribution_m)
    plt.ylim([0, 1])
    plt.show()

    TwistDistribution_deg = prop_design.simple_pitch(RadialStations_m, Pitch_m)
    TwistDistributionCollective_deg = [twist + Colective_deg for twist in TwistDistribution_deg]

    Omega_rad_s = 200
    Converged = False
    for _ in range(500):
        dT_vector, dQ_vector, r_vector, Re_vector, WA_vector, Cl_vector, Cd_vector = prop_simulate.qprop_PrePolars(AxialVelocity_m_s, Omega_rad_s, NumberOfBlades, Diameter_m/2, RadialStations_m, TwistDistributionCollective_deg, ChordDistribution_m, CLPolar = ClPolar, CDPolar = CdPolar)
        
        if True:
            Converged = True
            break
    
    if not Converged:
        return []
    return [1]
    
Evaluation([0.1, 0.2, 0.5, 0.1, 1.5, 1])

def Validation(Chromossome):
    Diameter_m = Chromossome[0]
    Croot_adim = Chromossome[1]
    Cmax_adim = Chromossome[2]
    smax_adim = Chromossome[3]
    Ctip_adim = Chromossome[4]
    Pitch_m = Chromossome[5]
    Colective_deg = Chromossome[6]

    #c_R = (a*r**3 + b*r**2 + c*r + d)**0.5
    a = -(2*Cmax_adim**2*smax_adim - Cmax_adim**2 + Croot_adim**2*smax_adim**2 - 2*Croot_adim**2*smax_adim + Croot_adim**2 - Ctip_adim**2*smax_adim**2)/(smax_adim**2*(smax_adim - 1)**2)
    b = -(- 3*Cmax_adim**2*smax_adim**2 + Cmax_adim**2 - 2*Croot_adim**2*smax_adim**3 + 3*Croot_adim**2*smax_adim**2 - Croot_adim**2 + 2*Ctip_adim**2*smax_adim**3)/(smax_adim**2*(smax_adim - 1)**2)
    c = -(3*Cmax_adim**2*smax_adim - 2*Cmax_adim**2 + Croot_adim**2*smax_adim**3 - 3*Croot_adim**2*smax_adim + 2*Croot_adim**2 - Ctip_adim**2*smax_adim**3)/(smax_adim*(smax_adim - 1)**2)
    d = Croot_adim**2

    DerivativeZero = c/(2*(d**0.5))
    if DerivativeZero < 0:
        return False
    DerivativeOne = (3*a + 2*b + c)/(2*((a + b + c + d)**0.5))
    if DerivativeOne > 0:
        return False
