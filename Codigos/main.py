import noise_calc
import prop_simulate
import prop_design
import optimization_NSGA2
import xfoil_interface
import numpy as np
import matplotlib.pyplot as plt
import time

Diameter_m = 3;
SpinnerCutoff = 0.2
NumberOfBlades = 2
ObjectiveThrust_N = 4000
AxialVelocity_m_s = 0
ClPolar = "ClarkYAutoClPolar4.dat"
CdPolar = "ClarkYAutoCdPolar4.dat"
NumberOfStations = 11
Convergence = 0.1 #Newton

def Evaluation(Chromossome):

    Croot_adim = Chromossome["Croot"]
    Cmax_adim = Chromossome["Cmax"]
    Smax_adim = Chromossome["Smax"]
    Ctip_adim = Chromossome["Ctip"]
    Pitch_m = Chromossome["Pitch"]
    Colective_deg = Chromossome["Collective"]


    #c_R = (a*r**3 + b*r**2 + c*r + d)**0.5
    a = -(2*Cmax_adim**2*Smax_adim - Cmax_adim**2 + Croot_adim**2*Smax_adim**2 - 2*Croot_adim**2*Smax_adim + Croot_adim**2 - Ctip_adim**2*Smax_adim**2)/(Smax_adim**2*(Smax_adim - 1)**2)
    b = -(- 3*Cmax_adim**2*Smax_adim**2 + Cmax_adim**2 - 2*Croot_adim**2*Smax_adim**3 + 3*Croot_adim**2*Smax_adim**2 - Croot_adim**2 + 2*Ctip_adim**2*Smax_adim**3)/(Smax_adim**2*(Smax_adim - 1)**2)
    c = -(3*Cmax_adim**2*Smax_adim - 2*Cmax_adim**2 + Croot_adim**2*Smax_adim**3 - 3*Croot_adim**2*Smax_adim + 2*Croot_adim**2 - Ctip_adim**2*Smax_adim**3)/(Smax_adim*(Smax_adim - 1)**2)
    d = Croot_adim**2

    S = np.linspace(0, 1, NumberOfStations, endpoint = True)
    RadialStations_adim = [SpinnerCutoff + ((1 - SpinnerCutoff)*s) for s in S]
    RadialStations_m = [r*Diameter_m/2 for r in RadialStations_adim]

    ChordDistribution_m = [((a*s**3 + b*s**2 + c*s + d)**0.5)*Diameter_m/2 for s in S]
    # plt.plot(S, ChordDistribution_m)
    # plt.plot(RadialStations_adim, ChordDistribution_m)
    # plt.ylim([0, 1])
    # plt.show()

    TwistDistribution_deg = prop_design.simple_pitch(RadialStations_m, Pitch_m)
    TwistDistributionCollective_deg = [twist + Colective_deg for twist in TwistDistribution_deg]

    UpperOmega_rad_s = 250
    LowerOmega_rad_s = 10

    dT_vector, dQ_vector, r_vector, Re_vector, WA_vector, Cl_vector, Cd_vector = prop_simulate.qprop_PrePolars(AxialVelocity_m_s, UpperOmega_rad_s, NumberOfBlades, Diameter_m/2, RadialStations_m, TwistDistributionCollective_deg, ChordDistribution_m, CLPolar = ClPolar, CDPolar = CdPolar)
    UpperResidue = ObjectiveThrust_N - np.trapz(dT_vector, r_vector)
    dT_vector, dQ_vector, r_vector, Re_vector, WA_vector, Cl_vector, Cd_vector = prop_simulate.qprop_PrePolars(AxialVelocity_m_s, LowerOmega_rad_s, NumberOfBlades, Diameter_m/2, RadialStations_m, TwistDistributionCollective_deg, ChordDistribution_m, CLPolar = ClPolar, CDPolar = CdPolar)
    LowerResidue = ObjectiveThrust_N - np.trapz(dT_vector, r_vector)

    # CurrentOmega_rad_s = 200
    Converged = False
    for _ in range(500):
        if abs(UpperOmega_rad_s - LowerOmega_rad_s) < Convergence:
            Converged = True
            # print("===================================================================================================")
            break

        MidOmega_rad_s = (UpperOmega_rad_s + LowerOmega_rad_s)/2
        dT_vector, dQ_vector, r_vector, Re_vector, WA_vector, Cl_vector, Cd_vector = prop_simulate.qprop_PrePolars(AxialVelocity_m_s, MidOmega_rad_s, NumberOfBlades, Diameter_m/2, RadialStations_m, TwistDistributionCollective_deg, ChordDistribution_m, CLPolar = ClPolar, CDPolar = CdPolar)
        MidResidue = ObjectiveThrust_N - np.trapz(dT_vector, r_vector)

        if UpperResidue*MidResidue < 0:
            LowerResidue = MidResidue
            LowerOmega_rad_s = MidOmega_rad_s
        elif LowerResidue*MidResidue < 0:
            UpperResidue = MidResidue
            UpperOmega_rad_s = MidOmega_rad_s
        else:
            print("Convergence Failed")
            break

        # dT_vector, dQ_vector, r_vector, Re_vector, WA_vector, Cl_vector, Cd_vector = prop_simulate.qprop_PrePolars(AxialVelocity_m_s, CurrentOmega_rad_s, NumberOfBlades, Diameter_m/2, RadialStations_m, TwistDistributionCollective_deg, ChordDistribution_m, CLPolar = ClPolar, CDPolar = CdPolar)
        
        # CurrentThrust_N = np.trapz(dT_vector, r_vector)
        # NewOmega_rad_s = ObjectiveThrust_N*CurrentOmega_rad_s/CurrentThrust_N
        # DeltaOmega_rad_s = NewOmega_rad_s - CurrentOmega_rad_s

        # RelaxationFactor = 1
        # Relaxed = False
        # if RelaxationFactor*DeltaOmega_rad_s > CurrentOmega_rad_s:
        #     RelaxationFactor = CurrentOmega_rad_s/DeltaOmega_rad_s
        #     Relaxed = True
        # if RelaxationFactor*DeltaOmega_rad_s < -0.5*CurrentOmega_rad_s:
        #     RelaxationFactor = -0.5*(CurrentOmega_rad_s/DeltaOmega_rad_s)
        #     Relaxed = True

        # DeltaOmega_rad_s *= RelaxationFactor

        # if (abs(DeltaOmega_rad_s) < Convergence) and not Relaxed:
        #     Converged = True
        #     print("===================================================================================================")
        #     break
        
        # print(MidOmega_rad_s)
        # CurrentOmega_rad_s += DeltaOmega_rad_s
    
    if not Converged:
        return [0.0001, 1]
    return [-np.trapz(dT_vector, r_vector), np.trapz(dQ_vector, r_vector)]

# StartTime = time.time()
# Evaluation([0.1, 0.2, 0.5, 0.1, 1.5, 1])
# EndTime = time.time()
# print(f"Execution Time = {EndTime - StartTime} seconds")

def Validation(Chromossome):
    
    Croot_adim = Chromossome["Croot"]
    Cmax_adim = Chromossome["Cmax"]
    Smax_adim = Chromossome["Smax"]
    Ctip_adim = Chromossome["Ctip"]
    Pitch_m = Chromossome["Pitch"]
    Colective_deg = Chromossome["Collective"]

    #c_R = (a*r**3 + b*r**2 + c*r + d)**0.5
    a = -(2*Cmax_adim**2*Smax_adim - Cmax_adim**2 + Croot_adim**2*Smax_adim**2 - 2*Croot_adim**2*Smax_adim + Croot_adim**2 - Ctip_adim**2*Smax_adim**2)/(Smax_adim**2*(Smax_adim - 1)**2)
    b = -(- 3*Cmax_adim**2*Smax_adim**2 + Cmax_adim**2 - 2*Croot_adim**2*Smax_adim**3 + 3*Croot_adim**2*Smax_adim**2 - Croot_adim**2 + 2*Ctip_adim**2*Smax_adim**3)/(Smax_adim**2*(Smax_adim - 1)**2)
    c = -(3*Cmax_adim**2*Smax_adim - 2*Cmax_adim**2 + Croot_adim**2*Smax_adim**3 - 3*Croot_adim**2*Smax_adim + 2*Croot_adim**2 - Ctip_adim**2*Smax_adim**3)/(Smax_adim*(Smax_adim - 1)**2)
    d = Croot_adim**2

    DerivativeZero = c/(2*(d**0.5))
    if DerivativeZero < 0:
        return False
    DerivativeOne = (3*a + 2*b + c)/(2*((a + b + c + d)**0.5))
    if DerivativeOne > 0:
        return False
    return True


OptimizationObject = optimization_NSGA2.NSGA2_v2(n_ind = 50, mut_rate = 0.2, t_size = 3, DecimalPoints = 4, convergence = 10, ma_len = 10, ma_tol = 0.005)
OptimizationObject.set_functions(Evaluation, Validation)
OptimizationObject.set_population_limits({"Croot": [0.02, 0.3], "Cmax": [0.02, 0.4], "Smax": [0.0001, 0.9999], "Ctip": [0.02, 0.4], "Pitch": [0.1*Diameter_m, 2*Diameter_m], "Collective": [0, 30]})
OptimizationObject.run()

for individual in OptimizationObject.current_pop:
    funcs = Evaluation(individual.get_chrom())
    plt.plot(funcs[0], funcs[1], 'bo')
plt.show()