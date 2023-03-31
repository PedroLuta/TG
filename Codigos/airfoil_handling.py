import math
import numpy as np

def apply_thickness(y, thick):
    for i in range(len(y)):
        y[i] *= thick
    return y

def apply_chord(x, y, chord):
    x_copy = x[:]
    y_copy = y[:]
    for i in range(len(x)):
        x_copy[i] *= chord
        y_copy[i] *= chord
    return x_copy, y_copy

def apply_offset_x(x, offset):
    x_copy = x[:]
    for i in range(len(x)):
        x_copy[i] += offset
    return x_copy

def apply_offset_y(y, offset):
    for i in range(len(y)):
        y[i] += offset
    return y

def apply_rotation_deg(x, y, angle):
    return apply_rotation_rad(x, y, math.radians(angle))

def apply_rotation_rad(x, y, angle):
    rot_mat = [[math.cos(angle), math.sin(angle)], [-math.sin(angle), math.cos(angle)]]
    x_rot = np.array([])
    y_rot = np.array([])
    for i in range(len(x)):
        case = np.array([[x[i]], [y[i]]])
        rotated = np.matmul(rot_mat, case)
        x_rot = np.append(x_rot, rotated[0])
        y_rot = np.append(y_rot, rotated[1])
    return x_rot, y_rot



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

def split_upper_lower(xx, yy):
    x = xx.copy()
    y = yy.copy()
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

def calculate_camber(xl, yl, xu, yu):
    len_l = len(xl)
    len_u = len(xu)
    camber_line = []
    if len_l < len_u:
        for i in range(len_l):
            camber_line.append((yu[i] + yl[i])/2)
        return xl, camber_line
    else:
        for i in range(len_u):
            camber_line.append((yu[i] + yl[i])/2)
        return xu, camber_line

def apply_thickness_on_camber(yl, yu, camber, thickness):
    thick_u = []
    thick_l = []
    for i in range(len(camber)):
        thick_u.append(yu[i] - camber[i])
        thick_l.append(camber[i] - yl[i])
    thick_l_after = [y*thickness for y in thick_l]
    thick_u_after = [y*thickness for y in thick_u]
    yu_after = []
    yl_after = []
    for i in range(len(camber)):
        yl_after.append(camber[i] - thick_l_after[i])
        yu_after.append(camber[i] + thick_u_after[i])
    return yl_after, yu_after

def apply_thickness_on_camber_easy(x, y, thickness):
    xl, yl, xu, yu = split_upper_lower(x, y)
    xu, camber_line = calculate_camber(xl, yl, xu, yu)
    yl, yu = apply_thickness_on_camber(yl, yu, camber_line, thickness)
    xu_copy = xu.copy()
    xu_reverse = xu_copy[::-1]
    yu_reverse = yu[::-1]
    xu = np.delete(xu, 0)
    yl = np.delete(yl, 0)
    x = np.append(xu_reverse, xu)
    y = np.append(yu_reverse, yl)
    return x, y
        

# airfoil = 'airfoils\\NACA6407.txt'
# x_orig, y_orig = read_foil(airfoil, header_lines = count_header_lines(airfoil))
# plt.plot(x_orig, y_orig)
# xl, yl, xu, yu = split_upper_lower(x_orig, y_orig)
# x, camber = calculate_camber(xl, yl, xu, yu)
# plt.plot(x, camber)
# yl_t, yu_t = apply_thickness_on_camber(yl, yu, camber, 2)
# plt.plot(x, yu_t)
# plt.plot(x, yl_t)
# plt.show()




# max_thickness_location = 0.14
# chord = 0.4
# anchor = 0.1

# airfoil_file = r'airfoils\\sunnysky.dat'
# x, y =  read_foil(airfoil_file, header_lines = count_header_lines(airfoil_file))
# plt.plot(x, y)
# x = apply_offset_x(x, -anchor)
# x, y = apply_chord(x, y, chord)
# plt.plot(x, y)
# x, y = apply_rotation_deg(x, y, 30)
# plt.plot(x, y)
# plt.xlim([-0.5, 0.5])
# plt.ylim([-0.5, 0.5])
# plt.show()
