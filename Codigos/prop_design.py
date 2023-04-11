import xfoil_interface
import Auxiliary
import math
import numpy as np
pi = np.pi
euler = np.e

def determine_section_Cl(W_times_chord_times_Cl, kvisc, alphas, airfoil, M, Cl_ini = 1):
    #print("Determining section Cl")
    Cl_new = 0
    tolerance = 0.05
    tries = 0
    a1, a2, astep = alphas[0], alphas[1], alphas[2]
    Cl_ref = Cl_ini
    while tries < 20:
        W_times_chord = W_times_chord_times_Cl/Cl_ref
        Re = ((W_times_chord)/kvisc)
        if Re == 0:
            return 0, 0, 1, False
        alpha_c, Cl_c, Cd_c = xfoil_interface.get_curve_com_default(Re, a1, a2, astep, afile = airfoil, M = M)
        alpha, Cl_new, Cd = Auxiliary.calculate_most_eff_alpha(alpha_c, Cl_c, Cd_c)
        if (abs(Cl_ref - Cl_new) < tolerance):
            return alpha, Cl_new, Cd, False
        tries += 1
        #print(f'Cl_ref:{Cl_ref} Cl_new:{Cl_new}')
        Cl_ref = (Cl_new + Cl_ref*3)/4
    print("failed Cl convergence")
    return 0, 0, 1, True
    



def blade_design_vortex_v1_1_1(vi, radps, Blades, R, r_vector, airfoil = 'airfoils\\airfoil.txt', rho = 1.225, dvisc = 1.8/100000, alphas = [0, 8, 0.25], speed_sound = 340, Prescribed_power = 520, Prescribed_thrust = 50, init_disp = 0):
    #From Design of Optimum Propellers, Charles N. Adkins*, Falls Church, Virginia 22042 and Robert H. Liebeckt, Douglas Aircraft Company, Long Beach, California 90846
    
    #With an iterative process to find section Cl

    kvisc = dvisc/rho
    lmb = vi/(radps*R)
    Pc = 2*Prescribed_power/(rho*(vi**3)*pi*(R**2)) 
    Tc = 2*Prescribed_thrust/(rho*(vi**2)*pi*(R**2))

    disp = init_disp
    tolerance_percentage = 0.005
    disp_new = disp
    disp_new2 = disp
    disp = disp_new #(disp_new + disp*2)/3
    while True:
        #disp = disp_new2 #(disp_new2 + disp*2)/3
        print(f"Try disp = {disp}")
        I1_vec = []
        I2_vec = []
        J1_vec = []
        J2_vec = []
        Beta_vector = []
        chord_vector = []
        #general_failed = False
        r_aux = r_vector.copy()

        phi_tip = math.atan(lmb*(1 + (disp/2)))
        list_to_delete = []
        for i in range(len(r_vector)): #rr in r_vector:
            rr = r_vector[i]
            #print("----NEW SECTION----")
            Csi = rr/R
            f = (Blades/2)*(1 - rr/R)/math.sin(phi_tip)
            F = (2/pi)*math.acos(euler**(-f))
            phi = math.atan(math.tan(phi_tip)/Csi)
            speed_ratio = radps*rr/vi
            G = F*math.cos(phi)*math.sin(phi)*speed_ratio
            placeholder_V = ((vi**2) + ((radps*rr)**2))**0.5
            M = placeholder_V/speed_sound
            W_times_chord_times_Cl = 4*pi*lmb*G*vi*R*disp/Blades
            alpha, Cl, Cd, failed = determine_section_Cl(W_times_chord_times_Cl, kvisc, alphas, airfoil, M) 
            if failed or Cl == 0:
                #general_failed = True
                list_to_delete.append(i)
            else:
                eps = Cd/Cl
                aa = (disp/2)*(math.cos(phi)**2)*(1 - eps*math.tan(phi)) 
                aa_ = (disp*lmb/2)*math.cos(phi)*math.sin(phi)*(1 + eps/math.tan(phi)) 
                W = vi*(1 + aa)/(math.sin(phi))
                chord = W_times_chord_times_Cl/(Cl*W)
                Beta = math.radians(alpha) + phi
                dI1 = 4*Csi*G*(1 - (eps*math.tan(phi)))
                dI2 = lmb*(dI1/(2*Csi))*(1 + (eps/math.tan(phi)))*math.sin(phi)*math.cos(phi)
                dJ1 = 4*Csi*G*(1 + (eps/math.tan(phi)))
                dJ2 = (dJ1/2)*(1 - (eps*math.tan(phi)))*(math.cos(phi)**2)

                Beta_vector.append(math.degrees(Beta))
                chord_vector.append(chord)
                I1_vec.append(dI1)
                I2_vec.append(dI2)
                J1_vec.append(dJ1)
                J2_vec.append(dJ2)
        r_aux = np.delete(r_aux, list_to_delete)
        I1 = Auxiliary.area_under_curve(r_aux, I1_vec)
        I2 = Auxiliary.area_under_curve(r_aux, I2_vec)
        J1 = Auxiliary.area_under_curve(r_aux, J1_vec)
        J2 = Auxiliary.area_under_curve(r_aux, J2_vec)

        disp_new = -(J1/(2*J2)) + (((J1/(2*J2))**2) + (Pc/J2))**0.5
        # disp_new2 = (I1/(2*I2)) - ((((I1/(2*I2))**2) - (Tc/I2))**0.5)
        # Tc_after = (I1*disp) - (I2*(disp**2))
        # Pc_after = (J1*disp) + (J2*(disp**2))

        if abs((disp/disp_new) - 1) < tolerance_percentage:
            print(f"Final disp: {disp}")
            if len(list_to_delete) != 0:
                for i in list_to_delete:
                    try:
                        mean_chord = (chord_vector[i - 1] + chord_vector[i])/2
                    except:
                        mean_chord = 0.002
                    chord_vector.insert(i, mean_chord)
            #print(f"Thrust gotten: {Tc_after*(rho*(vi**2)*pi*(R**2))/2}")
            #print(f"Power needed: {Pc_after*(rho*(vi**3)*pi*(R**2))/2}")
            break
        # if general_failed:
        #     disp = (disp_new + disp*9)/10
        #     continue
        #disp = (disp_new + disp*2)/3
        disp = (disp_new + disp)/2
        #disp = disp_new

    return chord_vector, Beta_vector

def blade_design_vortex_v1_2_1(vi, radps, Blades, R, r_vector, Cl_ref = 1, a_ref = 5, Cd_ref = 0.01, rho = 1.225, dvisc = 1.8/100000, Prescribed_power = 520, Prescribed_thrust = 50, init_disp = 0):
    #From Design of Optimum Propellers, Charles N. Adkins*, Falls Church, Virginia 22042 and Robert H. Liebeckt, Douglas Aircraft Company, Long Beach, California 90846
    
    #With fixed alpha, Cl and Cd defined by user 

    kvisc = dvisc/rho
    lmb = vi/(radps*R)
    Pc = 2*Prescribed_power/(rho*(vi**3)*pi*(R**2)) 
    Tc = 2*Prescribed_thrust/(rho*(vi**2)*pi*(R**2))

    disp = init_disp
    tolerance_percentage = 0.00000001
    disp_new = disp
    disp_new2 = disp
    while True:
        disp = disp_new #(disp_new + disp*2)/3
        #disp = disp_new2 #(disp_new2 + disp*2)/3
        print(f"Try disp = {disp}")
        I1_vec = []
        I2_vec = []
        J1_vec = []
        J2_vec = []
        Beta_vector = []
        chord_vector = []

        phi_tip = math.atan(lmb*(1 + (disp/2)))
        for rr in r_vector:
            #print("----NEW SECTION----")
            Csi = rr/R
            f = (Blades/2)*(1 - Csi)/math.sin(phi_tip)
            F = (2/pi)*math.acos(euler**(-f))
            phi = math.atan(math.tan(phi_tip)/Csi)
            speed_ratio = radps*rr/vi
            G = F*math.cos(phi)*math.sin(phi)*speed_ratio #CHECK PAPER
            W_times_chord = 4*pi*lmb*G*vi*R*disp/(Cl_ref*Blades)
            Re = ((W_times_chord)/kvisc)
            eps = Cd_ref/Cl_ref
            aa = (disp/2)*(math.cos(phi)**2)*(1 - eps*math.tan(phi)) 
            aa_ = (disp*lmb/2)*math.cos(phi)*math.sin(phi)*(1 + eps/math.tan(phi)) 
            W = vi*(1 + aa)/(math.sin(phi))
            chord = W_times_chord/W
            Beta = math.radians(a_ref) + phi
            dI1 = 4*Csi*G*(1 - (eps*math.tan(phi)))
            dI2 = lmb*(dI1/(2*rr/R))*(1 + (eps/math.tan(phi)))*math.sin(phi)*math.cos(phi)
            dJ1 = 4*Csi*G*(1 + (eps/math.tan(phi)))
            dJ2 = (dJ1/2)*(1 - (eps*math.tan(phi)))*(math.cos(phi)**2)

            Beta_vector.append(math.degrees(Beta))
            chord_vector.append(chord)
            I1_vec.append(dI1)
            I2_vec.append(dI2)
            J1_vec.append(dJ1)
            J2_vec.append(dJ2)
        I1 = Auxiliary.area_under_curve(r_vector, I1_vec)
        I2 = Auxiliary.area_under_curve(r_vector, I2_vec)
        J1 = Auxiliary.area_under_curve(r_vector, J1_vec)
        J2 = Auxiliary.area_under_curve(r_vector, J2_vec)

        disp_new = -(J1/(2*J2)) + ((((J1/(2*J2))**2) + (Pc/J2))**0.5)
        disp_new2 = (I1/(2*I2)) - ((((I1/(2*I2))**2) - (Tc/I2))**0.5)
        Tc_after = (I1*disp) - (I2*(disp**2))
        Pc_after = (J1*disp) + (J2*(disp**2))

        if abs((disp/disp_new) - 1) < tolerance_percentage:
            print(f"Final disp: {disp}")
            print(f"Thrust gotten: {Tc_after*(rho*(vi**2)*pi*(R**2))/2}")
            #print(f"Power needed: {Pc_after*(rho*(vi**3)*pi*(R**2))/2}")
            break

    return chord_vector, Beta_vector





def simple_pitch_inches1(r_vector_inches, Pitch_inches): #r_vector and pitch in inches
    r_vector = [rr*0.0254 for rr in r_vector_inches]
    Pitch = Pitch_inches*0.0254
    return simple_pitch(r_vector, Pitch)

def simple_pitch_inches2(r_vector, Pitch_inches): #only pitch in inches
    Pitch = Pitch_inches*0.0254
    return simple_pitch(r_vector, Pitch)

def simple_pitch(r_vector, Pitch): #All values in meters
    Beta_dist = []
    for rr in r_vector:
        Beta_dist.append(math.degrees(math.atan(Pitch/(2*pi*rr))))
    return Beta_dist

def ApplyColective(PitchVec, Collective):
    #It is expected that the Pitch vector values and Collective value are given in the same dimension
    CopyPitchVec = PitchVec.copy()
    for i in range(len(CopyPitchVec)):
        CopyPitchVec[i] = CopyPitchVec[i] + Collective
    return CopyPitchVec



def compatibility_function(point1, point2, value1, value2, der1, der2):
    M = np.zeros((4, 4))
    M[0] = [point2**3, point2**2, point2, 1]
    M[1] = [point1**3, point1**2, point1, 1]
    M[2] = [3*(point2**2), 2*point2, 1, 0]
    M[3] = [3*(point1**2), 2*point1, 1, 0]
    
    rhs = np.array(([value2], [value1], [der2], [der1]))

    coeffs = np.squeeze(np.transpose(np.matmul(np.linalg.inv(M), rhs)))

    return coeffs

#print(compatibility_function(0.1, 0.3, 0, 30, 0, 0))