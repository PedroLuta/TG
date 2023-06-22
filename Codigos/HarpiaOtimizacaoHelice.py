import prop_simulate
import prop_design
import optimization_NSGA2
import numpy as np

VelocityVector_m_s = [0, 3, 6, 9, 12, 15, 18]
MotorKV_rpm_V = 450
MotorNoLoadCurrent_A = 1.4
MaxCurrent_A = 54
MotorResistance_ohm = 0.032
BatteryVoltage_V = 22.2 #DEIXAR VOLTAGEM LIVRE -> SE PASSAR DE 22.2 DESCARTA
Power_W = 700

CurrentOutput_A = Power_W/BatteryVoltage_V

#Genes Contínuos -> Diametro, Croot, Cmax, rmax, Ctip, Passo, Coletivo
#Genes Discretos -> Numero de pas

def Evaluation(Chromossome):
    Diameter_m = Chromossome[0]
    Croot_adim = Chromossome[1]
    Cmax_adim = Chromossome[2]
    rmax_adim = Chromossome[3]
    Ctip_adim = Chromossome[4]
    Pitch_m = Chromossome[5]
    Colective_deg = Chromossome[6]

    #c_R = (a*r**3 + b*r**2 + c*r + d)**0.5
    a = -(2*Cmax_adim**2*rmax_adim - Cmax_adim**2 + Croot_adim**2*rmax_adim**2 - 2*Croot_adim**2*rmax_adim + Croot_adim**2 - Ctip_adim**2*rmax_adim**2)/(rmax_adim**2*(rmax_adim - 1)**2)
    b = -(- 3*Cmax_adim**2*rmax_adim**2 + Cmax_adim**2 - 2*Croot_adim**2*rmax_adim**3 + 3*Croot_adim**2*rmax_adim**2 - Croot_adim**2 + 2*Ctip_adim**2*rmax_adim**3)/(rmax_adim**2*(rmax_adim - 1)**2)
    c = -(3*Cmax_adim**2*rmax_adim - 2*Cmax_adim**2 + Croot_adim**2*rmax_adim**3 - 3*Croot_adim**2*rmax_adim + 2*Croot_adim**2 - Ctip_adim**2*rmax_adim**3)/(rmax_adim*(rmax_adim - 1)**2)
    d = Croot_adim**2

    r_R = np.linspace(0, 1, 11, endpoint = False)
    ChordDistribution_m = [((a*r**3 + b*r**2 + c*r + d)**0.5)*Diameter_m/2 for r in r_R]

    TwistDistribution_deg = prop_design.simple_pitch(r_R*Diameter_m/2, Pitch_m)
    TwistDistributionCollective_deg = [twist + Colective_deg for twist in TwistDistribution_deg]

    for AxialVelocity_m_s in [0, 3, 6, 9, 12, 15]:
        Omega_rad_s = 1
        PropellerTorque_Nm = 0
        #MotorVoltage(Omega_rad_s, TorquePropeller_Nm)
        for _ in range(50):
            #calcular torque requerido pela hélice no OMG atual
            #MotorTorque(Omega_rad_s, Voltage)
            #Resíduos
            #Se voltagem for maior que a da bateria, fazer algo
            #Iterar
            pass


    #CurrentOutput_A = (BatteryVoltage_V - (RPM/KV_rpm_V))/MotorResistance_ohm
    def MotorTorque(Omega_rad_s, Voltage_V):
        #inputs: Omega, Voltagem, Parametros do motor
        #outputs: Torque, Torque_OMG, Torque_VOLT, Current, Current_OMG, Current_VOLT
        MotorKV_rad_sV = MotorKV_rpm_V*2*np.pi/60
        MotorVoltage_V = Omega_rad_s/MotorKV_rad_sV
        dMotorVoltage_V__dOmega_rad_s = 1/MotorKV_rad_sV

        MotorCurrent_A = (Voltage_V - MotorVoltage_V)/MotorResistance_ohm
        dMotorCurrent_A__dOmega_rad_s = - dMotorVoltage_V__dOmega_rad_s/MotorResistance_ohm
        dMotorCurrent_A__dVoltage_V = 1/MotorResistance_ohm

        MotorTorque_Nm = (MotorCurrent_A - MotorNoLoadCurrent_A)/MotorKV_rad_sV
        dMotorTorque_Nm__dOmega_rad_s = dMotorCurrent_A__dOmega_rad_s/MotorKV_rad_sV
        dMotorTorque_Nm__dVoltage_V = dMotorCurrent_A__dVoltage_V/MotorKV_rad_sV
        
        return MotorTorque_Nm, dMotorTorque_Nm__dOmega_rad_s, dMotorTorque_Nm__dVoltage_V, MotorCurrent_A, dMotorCurrent_A__dOmega_rad_s, dMotorCurrent_A__dVoltage_V
    def MotorVoltage(Omega_rad_s, PropellerTorque_Nm):
        #inputs: Omega, Torque, Parametros do motor
        #outputs: Volt, Volt_OMG, Volt_TORQUE, Amp, Amp_OMG, Amp_TORQUE
        
        MotorKV_rad_sV = MotorKV_rpm_V*2*np.pi/60
        MotorCurrent_A = (PropellerTorque_Nm*MotorKV_rad_sV) + MotorNoLoadCurrent_A
        Voltage_V = (MotorCurrent_A*MotorResistance_ohm) + (Omega_rad_s/MotorKV_rad_sV)

        for _ in range(50):
            MotorTorque_Nm, dMotorTorque_Nm__dOmega_rad_s, dMotorTorque_Nm__dVoltage_V, MotorCurrent_A, dMotorCurrent_A__dOmega_rad_s, dMotorCurrent_A__dVoltage_V = MotorTorque(Omega_rad_s, Voltage_V)
            Residue1 = MotorTorque_Nm - PropellerTorque_Nm
            Residue1_VOLT = dMotorTorque_Nm__dVoltage_V
            dVoltage_V = -Residue1/Residue1_VOLT
            

def Validation(Chromossome):
    Diameter_m = Chromossome[0]
    Croot_adim = Chromossome[1]
    Cmax_adim = Chromossome[2]
    rmax_adim = Chromossome[3]
    Ctip_adim = Chromossome[4]
    Pitch_m = Chromossome[5]
    Colective_deg = Chromossome[6]

    #c_R = (a*r**3 + b*r**2 + c*r + d)**0.5
    a = -(2*Cmax_adim**2*rmax_adim - Cmax_adim**2 + Croot_adim**2*rmax_adim**2 - 2*Croot_adim**2*rmax_adim + Croot_adim**2 - Ctip_adim**2*rmax_adim**2)/(rmax_adim**2*(rmax_adim - 1)**2)
    b = -(- 3*Cmax_adim**2*rmax_adim**2 + Cmax_adim**2 - 2*Croot_adim**2*rmax_adim**3 + 3*Croot_adim**2*rmax_adim**2 - Croot_adim**2 + 2*Ctip_adim**2*rmax_adim**3)/(rmax_adim**2*(rmax_adim - 1)**2)
    c = -(3*Cmax_adim**2*rmax_adim - 2*Cmax_adim**2 + Croot_adim**2*rmax_adim**3 - 3*Croot_adim**2*rmax_adim + 2*Croot_adim**2 - Ctip_adim**2*rmax_adim**3)/(rmax_adim*(rmax_adim - 1)**2)
    d = Croot_adim**2

    DerivativeZero = c/(2*(d**0.5))
    if DerivativeZero < 0:
        return False
    DerivativeOne = (3*a + 2*b + c)/(2*((a + b + c + d)**0.5))
    if DerivativeOne > 0:
        return False
