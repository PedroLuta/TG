import optimization_NSGA2
import matplotlib.pyplot as plt

from pymoo.factory import get_problem
from pymoo.util.plotting import plot

def evaluate(chrom):
    x1, x2 = chrom['x1'], chrom['x2']
    f1 = 4*(x1**2) + 4*(x2**2)
    f2 = (x1 - 5)**2 + (x2 - 5)**2
    return [f1, f2]

def is_valid(chrom):
    x1, x2 = chrom['x1'], chrom['x2']
    C1 = (x1 - 5)**2 + x2**2 #≤ 25
    C2 = (x1 - 8)**2 + (x2 + 3)**2 #≥ 7.7
    if (C1 > 25) or C2 < 7.7:
        return False
    return True

absolute_limits = {'x1':[0, 5], 'x2':[0, 3]}

#optimization = optimization_NSGA2.NSGA2_v1(n_ind = 100, n_gen = 50, mut_rate = 0.05, t_size = 2, dp = 4)
optimization = optimization_NSGA2.NSGA2_v2(n_ind = 100, mut_rate = 0.05, t_size = 2, DecimalPoints = 4, convergence = 50, ma_len = 10, ma_tol = 0.005)
optimization.set_population_limits(absolute_limits)
optimization.set_functions(evaluate, is_valid)
optimization.run()

for individual in optimization.current_pop:
    funcs = evaluate(individual.get_chrom())
    plt.plot(funcs[0], funcs[1], 'bo')
    # print(individual.get_chrom())
plt.plot(funcs[0], funcs[1], 'bo', label = "Individual")
# plt.plot(funcs[0], funcs[1], 'r', label = "Pareto-front")
problem = get_problem("bnh")
plot(problem.pareto_front(), no_fill = True, labels = 'Pareto-front')