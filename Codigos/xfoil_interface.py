import subprocess as sp

def get_curve_com_default(Rey, a1, a2, astep, afile = 'airfoils\\airfoil.txt', timeout = 5, \
    xfoilpath = 'xfoil.exe', itr = 250, M = 0):

    if Rey == 0:
        return [], [], []
    
    if a1*a2 < 0:
        a_list1, cl_list1, cd_list1 = get_curve_com_default(Rey, 0, a1, -astep, afile = afile, timeout = timeout, xfoilpath = xfoilpath, itr = itr, M = M)
        a_list2, cl_list2, cd_list2 = get_curve_com_default(Rey, 0, a2, astep, afile = afile, timeout = timeout, xfoilpath = xfoilpath, itr = itr, M = M)
        if len(a_list1) > 0:
            a_list1.pop(0)
            cl_list1.pop(0)
            cd_list1.pop(0)
        a_list1.reverse()
        cl_list1.reverse()
        cd_list1.reverse()
        a_list1.extend(a_list2)
        cl_list1.extend(cl_list2)
        cd_list1.extend(cd_list2)
        return a_list1, cl_list1, cd_list1

    ps = sp.Popen(xfoilpath, universal_newlines = True, stdin = sp.PIPE, stdout = sp.PIPE)

    try:
        output, _ = ps.communicate(input = f'plop\ng f\n\nload {afile}\n\noper\niter {itr}\nvisc {Rey}\nM {M}\naseq {a1} {a2} {astep}\n\n\n\nquit \n', timeout = timeout) 
    except: 
        ps.kill()
        return [], [], []
    list = output.split()
    i = 0
    id_remember = 0
    cl_list = []
    cd_list = []
    a_list = []
    length = len(list)
    for i in range(length):
        if list[i].lower() == "cl":
            cl = list[i + 2]
        if list[i].lower() == "cd":
            cd = list[i + 2]
        if list[i].lower() == "a":
            a = list[i + 2]
        
        if id_remember < itr:
            skip = False

        if list[i].lower() == "rms:":
            if (id_remember >= itr) and (not skip):
                skip = True
            elif (int(list[i - 1]) < id_remember) and (not skip):
                try:
                    a_list.append(float(a))
                    cl_list.append(float(cl))
                    cd_list.append(float(cd))
                except:
                    pass
                skip = True
            id_remember = int(list[i - 1])
        
        if (i == len(list) - 1) and (id_remember < itr):
            try:
                a_list.append(float(a))
                cl_list.append(float(cl))
                cd_list.append(float(cd))
            except:
                pass
    try:
        ps.kill()
        return a_list, cl_list, cd_list
    except:
        ps.kill()
        return [], [], []

