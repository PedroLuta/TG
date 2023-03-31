import numpy as np
import matplotlib.pyplot as plt
from numpy.linalg.linalg import LinAlgError
import math
import random
random.seed()

class PFoil:
    def __init__(self, random = False, selig_file = "", plot_diff = False, params_vec = [], curves = False, x = [], y = []):
        if selig_file != "":
            self.load_from_selig(selig_file, plot_diff)
        elif curves:
            self.load_from_curves(x, y)
        elif random:
            self.set_params_vec(params_gen())
            self.calculate_coeffs()
        else:
            self.set_params_vec(params_vec)
            self.calculate_coeffs()

    def get_params_vec(self):
        return self.params_vec
    def set_params_vec(self, parvec):
        self.params_vec = parvec

    def calculate_coeffs(self):
        if len(self.get_params_vec()) == 0:
            self.aup, self.alo = [], []
        else:
            self.aup, self.alo = coeffs_from_params_vec(self.get_params_vec())
    def set_coeffs(self, aup, alo):
        self.aup = aup
        self.alo = alo
    def get_coeffs(self):
        return self.aup, self.alo

    def write_file(self, file = 'airfoils\\airfoil.txt', npTotal = 100, npT = 50, npB = 50):
        done = write_from_coeffs(self.aup, self.alo, file, npTotal = npTotal, npT = npT, npB = npB)
        return done

    def load_from_selig(self, file, plot = False):
        x, y = read_foil(file, header_lines = count_header_lines(file))
        x_lo, y_lo, x_u, y_u = split_upper_lower(x, y)
        self.aup, self.alo = coeffs_from_curves(x_lo, x_u, y_lo, y_u)
        parvec = parameters_from_coeffs(self.aup, self.alo)
        self.set_params_vec(parvec)
        self.calculate_coeffs()
        if plot:
            self.plot_diff(x_lo, y_lo, x_u, y_u)
    
    def load_from_curves(self, x, y, plot = False):
        x_lo, y_lo, x_u, y_u = split_upper_lower(x, y)
        self.aup, self.alo = coeffs_from_curves(x_lo, x_u, y_lo, y_u)
        parvec = parameters_from_coeffs(self.aup, self.alo)
        self.set_params_vec(parvec)
        self.calculate_coeffs()
        if plot:
            self.plot_diff(x_lo, y_lo, x_u, y_u)
            
    def plot(self):
        y_poli_up, x_dist_up = write_zcoor(self.aup)
        y_poli_lo, x_dist_lo = write_zcoor(self.alo)
        fig, ax = plt.subplots()
        ax.plot(x_dist_up, y_poli_up, 'r')
        ax.plot(x_dist_lo, y_poli_lo, 'r')
        ax.xaxis.grid(True, which='major')
        #plt.ylim(-0.5, 0.5)
        #plt.xlim(0, 1)
        plt.show()

    def dists(self):
        yup, xup = write_zcoor(self.aup)
        ylo, xlo = write_zcoor(self.alo)
        return xup, yup, xlo, ylo

    def plot_diff(self, x_lo, y_lo, x_u, y_u):
        y_poli_up, x_dist_up = write_zcoor(self.aup)
        y_poli_lo, x_dist_lo = write_zcoor(self.alo)
        _, ax = plt.subplots()
        ax.plot(x_u, y_u, 'r')
        ax.plot(x_lo, y_lo, 'r', label = "original")
        ax.plot(x_dist_up, y_poli_up, 'b--')
        ax.plot(x_dist_lo, y_poli_lo, 'b--', label = "PARSEC aproximation")
        ax.xaxis.grid(True, which='major')
        plt.ylim(-0.5, 0.5)
        plt.xlim(0, 1)
        ax.legend()
        plt.show()



#general functions
def main_func(ans, x):
    if len(ans) == 0:
        return
    return ans[0]*(x**0.5) + ans[1]*(x**1.5) + ans[2]*(x**2.5) + ans[3]*(x**3.5) + ans[4]*(x**4.5) + ans[5]*(x**5.5)

def derivative1(ans, x):
    if len(ans) == 0:
        return
    return 0.5*ans[0]*(x**(-0.5)) + 1.5*ans[1]*(x**0.5) + 2.5*ans[2]*(x**1.5) + 3.5*ans[3]*(x**2.5) + 4.5*ans[4]*(x**3.5) + 5.5*ans[5]*(x**4.5)

def derivative2(ans, x):
    if len(ans) == 0:
        return
    return -0.25*ans[0]*(x**(-1.5)) + 0.75*ans[1]*(x**(-0.5)) + 3.75*ans[2]*(x**0.5) + 8.75*ans[3]*(x**1.5) + 15.75*ans[4]*(x**2.5) + 24.75*ans[5]*(x**3.5)



#mostly used
def coeffs_from_params_vec(PARSEC_params):
    ate = PARSEC_params[0]
    bte = PARSEC_params[1]
    zte = PARSEC_params[2]
    dzte = PARSEC_params[3]
    rup = PARSEC_params[4]
    xup = PARSEC_params[5]
    zup = PARSEC_params[6]
    zxxup = PARSEC_params[7]
    rlo = PARSEC_params[8]
    xlo = PARSEC_params[9]
    zlo = PARSEC_params[10]
    zxxlo = PARSEC_params[11]

    Cup = np.zeros((6, 6))
    Cup[0] = [1, 1, 1, 1, 1, 1]
    Cup[1] = [xup**0.5, xup**1.5, xup**2.5, xup**3.5, xup**4.5, xup**5.5]
    Cup[2] = [0.5, 1.5, 2.5, 3.5, 4.5, 5.5]
    Cup[3] = [0.5*(xup**(-0.5)), 1.5*(xup**0.5), 2.5*(xup**1.5), 3.5*(xup**2.5), 4.5*(xup**3.5), 5.5*(xup**4.5)]
    Cup[4] = [-0.25*(xup**(-1.5)), 0.75*(xup**(-0.5)), 3.75*(xup**0.5), 8.75*(xup**1.5), 15.75*(xup**2.5), 24.75*(xup**3.5)]
    Cup[5] = [1, 0, 0, 0, 0, 0]

    bup = np.array(([zte + (dzte/2)], [zup], [np.tan(np.radians(ate - (bte/2)))], [0], [zxxup], [(2*rup)**0.5]))

    Clo = np.zeros((6, 6))
    Clo[0] = [1, 1, 1, 1, 1, 1]
    Clo[1] = [xlo**0.5, xlo**1.5, xlo**2.5, xlo**3.5, xlo**4.5, xlo**5.5]
    Clo[2] = [0.5, 1.5, 2.5, 3.5, 4.5, 5.5]
    Clo[3] = [0.5*(xlo**(-0.5)), 1.5*(xlo**0.5), 2.5*(xlo**1.5), 3.5*(xlo**2.5), 4.5*(xlo**3.5), 5.5*(xlo**4.5)]
    Clo[4] = [-0.25*(xlo**(-1.5)), 0.75*(xlo**(-0.5)), 3.75*(xlo**0.5), 8.75*(xlo**1.5), 15.75*(xlo**2.5), 24.75*(xlo**3.5)]
    Clo[5] = [1, 0, 0, 0, 0, 0]

    blo = np.array(([zte - (dzte/2)], [zlo], [np.tan(np.radians(ate + (bte/2)))], [0], [zxxlo], [-(2*rlo)**0.5]))
    try:
        aup = np.squeeze(np.transpose(np.matmul(np.linalg.inv(Cup), bup)))
        alo = np.squeeze(np.transpose(np.matmul(np.linalg.inv(Clo), blo)))
    except LinAlgError:
        return [1, 1, 1, 1, 1, 1], [1, 1, 1, 1, 1, 1]

    return aup, alo

def write_from_coeffs(aup, alo, coord_file, npTotal = 200, npT = 50, npB = 50):
    npT = round(npT*npTotal/100)
    npB = round(npB*npTotal/100)
    try:
        with open(coord_file, 'w') as inp:
            for i in range(1, npT + 1):
                theta = (180.0/npT)*(npT - i)
                x = 0.5 - (0.5*np.cos(math.radians(theta)))
                inp.write(f'{x} {main_func(aup, x)}\n')
            for i in range(npB, 0, -1):
                theta = (180.0/npB)*(npB - i)
                x = 0.5 - (0.5*np.cos(theta*np.pi/180.0))
                inp.write(f'{x} {main_func(alo, x)}\n')
        return True
    except:
        return False

def coeffs_from_curves(x_lo, x_u, y_lo, y_u):
    x_mat_lo = np.ones((x_lo.size, 6))
    for i in range(len(x_lo)):
        for ii in range(6):
            x_mat_lo[i][ii] = x_lo[i]**(ii + 0.5)
    x_mat_u = np.ones((x_u.size, 6))
    for i in range(len(x_u)):
        for ii in range(6):
            x_mat_u[i][ii] = x_u[i]**(ii + 0.5)
    y_mat_lo = np.transpose(y_lo)
    y_mat_u = np.transpose(y_u)
    x_mat_lot = np.transpose(x_mat_lo)
    x_mat_ut = np.transpose(x_mat_u)
    x_times_xt_lo = np.matmul(x_mat_lot, x_mat_lo)
    x_times_xt_u = np.matmul(x_mat_ut, x_mat_u)
    y_times_xt_lo = np.matmul(x_mat_lot, y_mat_lo)
    y_times_xt_u = np.matmul(x_mat_ut, y_mat_u)
    x_inverse_lo = np.linalg.inv(x_times_xt_lo)
    x_inverse_u = np.linalg.inv(x_times_xt_u)
    answer_lo = np.matmul(y_times_xt_lo, x_inverse_lo)
    answer_u = np.matmul(y_times_xt_u, x_inverse_u)
    return answer_u, answer_lo

def parameters_from_coeffs(answer_u, answer_lo):
    rlo = (answer_lo[0]**2)/2
    rup = (answer_u[0]**2)/2
    zte = (np.sum(answer_lo) + np.sum(answer_u))/2 
    dzte = np.sum(answer_u) - np.sum(answer_lo)
    xlo = newt_rhap_first(answer_lo, tolerance = 0.00000001, init = 0.0001)
    xup = newt_rhap_first(answer_u, tolerance = 0.00000001, init = 0.0001)
    zlo = main_func(answer_lo, xlo)
    zup = main_func(answer_u, xup)
    zxxlo = derivative2(answer_lo, xlo)
    zxxup = derivative2(answer_u, xup)
    ate = (np.degrees(np.arctan(derivative1(answer_u, 1))) + np.degrees(np.arctan(derivative1(answer_lo, 1))))/2
    bte = np.degrees(np.arctan(derivative1(answer_lo, 1))) - np.degrees(np.arctan(derivative1(answer_u, 1)))
    return [ate, bte, zte, dzte, rup, xup, zup, zxxup, rlo, xlo, zlo, zxxlo]


#auxiliary functions
def read_foil(airfoil_file, header_lines):
    x, y = np.loadtxt(airfoil_file, skiprows = header_lines, unpack = True)
    return x, y

def count_header_lines(file):
    with open(file, 'r') as inp:
        i = 0
        while True:
            try:
                content = inp.readline()
                cont = content.split()
                _ = float(cont[0])
                break
            except:
                i += 1
    return i

def split_upper_lower(x, y):
    i = 0
    diff_remember = 999
    while i < len(x):
        if x[i] > diff_remember:
            index0 = i - 1
            if len(x)%2 == 0:
                x_lower = x[i-1:]
                y_lower = y[i-1:]
                x_upper = np.flip(x[:i-1])
                y_upper = np.flip(y[:i-1])
                break
            else:
                x_lower = x[i-1:]
                y_lower = y[i-1:]
                x_upper = np.flip(x[:i])
                y_upper = np.flip(y[:i])
                break
        diff_remember = x[i]
        i += 1 
    delta = y[index0]
    y_upper = y_upper - delta
    y_lower = y_lower - delta
    delta = x[index0]
    x_upper = x_upper - delta
    x_lower = x_lower - delta
    return x_lower, y_lower, x_upper, y_upper

def newt_rhap_first(ans, tolerance = 0.0001, init = 0.01, cutbreak = False):
    xi = init
    retry = False
    while True:
        if xi < 0:
            retry = True
            break
        delta = derivative1(ans, xi)/derivative2(ans, xi)
        xn = xi - delta
        if abs(xn - xi) < tolerance:
            break
        xi = xn
    if retry:
        if cutbreak:
            return -1337
        xn = newt_rhap_first(ans, tolerance = tolerance, init = 0.3, cutbreak = True)
    return xn

def write_zcoor(ans, num_points = 50):
    y_poli = np.array([])
    x_dist = np.array([])
    
    for i in range(num_points, 0, -1):
        theta = (180.0/(num_points - 1))*(num_points - i)
        x = 0.5 - (0.5*np.cos(theta*np.pi/180.0))
        x_dist = np.append(x_dist, x)
        y_poli = np.append(y_poli, main_func(ans, x))
    return y_poli, x_dist












#validity checkers (these functions check if the airfoil generated is feasible on the validations considered)
def check_wavy(aup, alo, up_lim = 2, lo_lim = 3):
    zu_remember1 = 0
    zlo_remember1 = 0
    zu_remember2 = 0
    zlo_remember2 = 0
    lo_count = 0
    up_count = 0
    first = True
    for x in np.linspace(0, 1, 101, endpoint = False):
        zu = main_func(aup, x)
        zlo = main_func(alo, x)
        if x == 0:
            continue
        if first:
            zu_remember1 = zu_remember2
            zlo_remember1 = zlo_remember2
            zlo_remember2 = zlo
            zu_remember2 = zu
            first = False
            continue

        if (np.sign(zlo_remember2 - zlo_remember1) != np.sign(zlo - zlo_remember2)):
            lo_count += 1

        if (np.sign(zu_remember2 - zu_remember1) != np.sign(zu - zu_remember2)):
            up_count += 1

        zu_remember1 = zu_remember2
        zlo_remember1 = zlo_remember2
        zlo_remember2 = zlo
        zu_remember2 = zu

    if up_count > up_lim:
        return True
    if lo_count > lo_lim:
        return True
    return False

def check_wavy_der(aup, alo, up_lim = 2, lo_lim = 3):
    lo_count = 0
    up_count = 0
    x_remember = 0
    first = True
    for x in np.linspace(0, 1, 101, endpoint = False):
        if x == 0:
            continue
        if first:
            x_remember = x
            first = False
            continue
        if (np.sign(derivative1(alo, x_remember)) != np.sign(derivative1(alo, x))):
            lo_count += 1
        if (np.sign(derivative1(aup, x_remember)) != np.sign(derivative1(aup, x))):
            up_count += 1

        x_remember = x

    if up_count > up_lim:
        return True
    if lo_count > lo_lim:
        return True
    return False

def check_too_thin(aup, alo, thick_min = 0.02):
    for x in np.linspace(0.1, 0.9, 50):
        zu = main_func(aup, x)
        zlo = main_func(alo, x)
        if zu - zlo < thick_min:
            return True
    return False

def curve_cross(aup, alo):
    for x in np.linspace(0, 1, 101):
        zu = main_func(aup, x)
        zlo = main_func(alo, x)
        if zlo > zu:
            return True
    return False

def check_choke(aup, alo):
    delta_remember = 0
    changed = False
    for x in np.linspace(0, 1, 101):
        zu = main_func(aup, x)
        zlo = main_func(alo, x)
        delta = zu - zlo
        if (delta < delta_remember) and not changed:
            changed = True
        elif changed:
            if delta > delta_remember:
                return True
        delta_remember = delta
    return False

def check_max(aup, alo, xup, xlo):
    for i in np.linspace(0, 1, 26):
        if main_func(aup, i) > main_func(aup, xup):
            return True
        if main_func(alo, i) < main_func(alo, xlo):
            return True
    return False

def check_valid(aup, alo, params):
    xup = params[5]
    xlo = params[9]

    if all(xx == 1 for xx in aup) or all(xx == 1 for xx in alo):
        return False
    if all(xx == 1 for xx in params):
        return False
    if curve_cross(aup, alo):
        return False
    #if check_choke(aup, alo): #talvez tirar
        #return False
    if check_max(aup, alo, xup, xlo):
        return False
    if derivative2(aup, xup) > 0:
        return False
    if derivative2(alo, xlo) < 0:
        return False
    #if check_too_thin(aup, alo): #talvez tirar
        #return False
    #if check_wavy(aup, alo):
    #    return False
    return True

def limits_from_foil(foil_params, factor):
    limits = np.zeros((12, 2))
    i = 0
    while i < len(foil_params):
        limits[i][0] = foil_params[i] - (foil_params[i]*factor)
        limits[i][1] = foil_params[i] + (foil_params[i]*factor)
        i += 1
    return limits

def params_gen(limits = [[-30, 15], [0.0001, 45], [-0.05, 0.05], [0.0001, 0.05], [0.0001, 0.1], [0.0001, 0.8], [0.0001, 0.35], [-5, 0.0001], [0.0001, 0.1], [0.0001, 0.5], [-0.35, -0.0001], [0.0001, 10]]):
    #[ate, bte, zte, dzte, rup, xup, zup, zxxup, rlo, xlo, zlo, zxxlo]
    ate = random.uniform(limits[0][0], limits[0][1]) 
    bte = random.uniform(limits[1][0], limits[1][1]) 
    zte = random.uniform(limits[2][0], limits[2][1]) 
    dzte = random.uniform(limits[3][0], limits[3][1]) 
    rup = random.uniform(limits[4][0], limits[4][1]) 
    xup = random.uniform(limits[5][0], limits[5][1]) 
    zup = random.uniform(limits[6][0], limits[6][1]) 
    zxxup = random.uniform(limits[7][0], limits[7][1]) 
    rlo = random.uniform(limits[8][0], limits[8][1]) 
    xlo = random.uniform(limits[9][0], limits[9][1]) 
    zlo = random.uniform(limits[10][0], limits[10][1]) 
    zxxlo = random.uniform(limits[11][0], limits[11][1])
    final_vec = np.array([ate, bte, zte, dzte, rup, xup, zup, zxxup, rlo, xlo, zlo, zxxlo])
    return final_vec

