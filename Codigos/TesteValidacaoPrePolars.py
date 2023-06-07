import prop_simulate
import numpy as np
pi = np.pi

AxialVelocity_m_s = 0
RPM = 1500
Omega_rad_s = RPM*2*pi/60
Radius_m = 1.5
DiscretizedRadius_m = np.linspace(0.2*Radius_m, 0.99*Radius_m, 21, endpoint = True)
DiscretizedBeta_deg = [20]*len(DiscretizedRadius_m)
DiscretizedChord_m = [0.2*Radius_m]*len(DiscretizedRadius_m)


dT_vector, dQ_vector, r_vector, Re_vector, WA_vector, Cl_vector, Cd_vector = prop_simulate.qprop_fixed_pitch(AxialVelocity_m_s, Omega_rad_s, 2, Radius_m, DiscretizedRadius_m, DiscretizedBeta_deg, DiscretizedChord_m, airfoil = 'clarky.dat', alphas = [-10, 15, 1])
x = np.trapz(dT_vector, r_vector)
print(x)


dT_vector, dQ_vector, r_vector, Re_vector, WA_vector, Cl_vector, Cd_vector = prop_simulate.qprop_PrePolars(AxialVelocity_m_s, Omega_rad_s, 2, Radius_m, DiscretizedRadius_m, DiscretizedBeta_deg, DiscretizedChord_m, CLPolar = "ClarkYCLPolar.dat", CDPolar = "ClarkYCDPolar.dat")
x2 = np.trapz(dT_vector, r_vector)
print(x2)
