import matplotlib.pyplot as plt
import random
random.seed()
from math import inf as infinite
import Auxiliary



class Individual:
    def __init__(self, chrom = {}, front = -1, crowd = 0, domcount = infinite, ObjVal = [], valid = True, Information = {}):
        self.chrom = chrom
        self.front = front
        self.crowd = crowd
        self.domcount = domcount
        self.ObjVal = ObjVal
        self.valid = valid
        self.Information = Information

    def set_valid(self, boolean):
        self.valid = boolean
    def get_valid(self):
        return self.valid

    def set_chrom(self, chrom):
        self.chrom = chrom
    def get_chrom(self):
        return self.chrom

    def set_Front(self, front):
        self.front = front
    def get_Front(self):
        return self.front

    def set_Crowding(self, crowd):
        self.crowd = crowd
    def get_Crowding(self):
        return self.crowd

    def set_Dominated_counter(self, counter):
        self.domcount = counter
    def get_Dominated_counter(self):
        return self.domcount

    def set_ObjVal(self, value, index = -1):
        if index == -1:
            self.ObjVal = value
        else:
            self.ObjVal[index] = value
    def get_ObjVal(self, index = -1):
        if index == -1:
            return self.ObjVal
        return self.ObjVal[index]
    
    def SetInformation(self, Information):
        self.Information = Information
    def GetInformation(self):
        return self.Information

    def set_ID(self, ID):
        self.ID = ID
    def get_ID(self):
        return self.ID



class NSGA2_v1:
    def __init__(self, n_ind = 0, n_gen = 0, mut_rate = 0, t_size = 0, DecimalPoints = 4): 
        self.n_ind = n_ind
        self.n_gen = n_gen
        self.mut_rate = mut_rate
        self.t_size = t_size
        self.DecimalPoints = DecimalPoints

    def set_population_limits(self, limits): 
        y = {}
        for key in limits:
            y[key] = limits[key].copy()
        self.limits = y

    def set_functions(self, evaluate, validate):
        self.evaluate = evaluate
        self.validate = validate

    def run(self): #CHECKED - OK1
        print("Geração 1")
        self.create_first_gen()
        self.evaluate_population(self.current_pop)
        self.current_pop = assign_fronts(self.current_pop)
        self.current_pop = assign_crowding(self.current_pop)
        for generation in range(2, self.n_gen + 1): 
            print(f"Geração {generation}")
            self.create_offspring()
            self.reinsert()

    def create_first_gen(self): #CHECKED - OK1 - OK2
        individuals = 0 
        chrom_pop = []
        first_gen = []
        while individuals < self.n_ind:
            chromossome = self.generate_chrom()
            if self.validate(chromossome) and chromossome not in chrom_pop:
                chrom_pop.append(chromossome)
                individuals += 1
        for chrom in chrom_pop:
            first_gen.append(Individual(chrom = chrom))
        self.current_pop = first_gen

    def create_offspring(self): #CHECKED - OK1
        init_chrom_pop = []
        chrom_pop = []
        length = len(self.current_pop)
        for individual in self.current_pop:
            init_chrom_pop.append(individual.get_chrom())
        population = strip_rejected(self.current_pop)
        while len(chrom_pop) < length:
            mother = self.select_tournament(population)
            father = self.select_tournament(population)
            son_chrom = self.crossover(mother.get_chrom(), father.get_chrom())
            son_chrom = self.mutate(son_chrom)
            if self.validate(son_chrom) and (son_chrom not in init_chrom_pop):
                chrom_pop.append(son_chrom)
        offspring = []
        for chrom in chrom_pop:
            offspring.append(Individual(chrom = chrom))
        self.evaluate_population(offspring)
        self.offspring = offspring

    def reinsert(self): #CHECKED - OK1
        pop_ini = self.current_pop.copy()
        pop_new = self.offspring.copy()
        length = len(pop_ini)
        pop_ini.extend(pop_new)
        pop_ini = strip_rejected(pop_ini)
        pop_ini = strip_equal(pop_ini)
        pop_ini = assign_fronts(pop_ini)
        pop_ini = assign_crowding(pop_ini)
        pop_final = []
        while len(pop_final) < length and len(pop_ini) > 0:
            best = pop_ini[0]
            i = 1
            hold = 0
            while i < len(pop_ini):
                if pop_ini[i].get_Front() < best.get_Front():
                    best = pop_ini[i]
                    hold = i
                elif pop_ini[i].get_Front() == best.get_Front():
                    if pop_ini[i].get_Crowding() > best.get_Crowding():
                        best = pop_ini[i]
                        hold = i
                i += 1
            pop_final.append(best)
            pop_ini.pop(hold)
        self.current_pop = pop_final

    def evaluate_population(self, pop): #CHECKED - OK1 - OK2
        for individual in pop:
            valid = self.validate(individual.get_chrom())
            if not valid:
                individual.set_valid(False)
                continue
            obj_vals = self.evaluate(individual.get_chrom())
            individual.set_ObjVal(obj_vals)

    def generate_chrom(self): #CHECKED - OK1 - OK2
        chrom = {}
        for key in self.limits:
            parameter = random.uniform(self.limits[key][0], self.limits[key][1])
            param = round(parameter, self.DecimalPoints)
            chrom[key] = param
        return chrom

    def crossover(self, mother, father): #CHECKED - OK1 - IT WAS NOT OK
        son_genes = {}
        for key in mother:
            chosen = random.random()
            if chosen > 0.5:
                son_genes[key] = mother[key]
            else:
                son_genes[key] = father[key]
        return son_genes

    def mutate(self, chrom): #CHECKED - OK1
        copy = chrom.copy()
        for key in copy:
            mut = random.random()
            if mut < self.mut_rate:
                parameter = random.uniform(self.limits[key][0], self.limits[key][1])
                param = round(parameter, self.DecimalPoints)
                copy[key] = param
        return copy

    def select_tournament(self, population): #CHECKED - OK1
        index_list = []
        for _ in range(self.t_size):
            rand_index = random.randint(0, len(population) - 1)
            while rand_index in index_list:
                rand_index = random.randint(0, len(population) - 1)
            index_list.append(rand_index)
        best = population[index_list[0]]
        for i in range(1, self.t_size): 
            new = population[index_list[i]]
            best = self.battle(best, new)
        return best

    def battle(self, indA, indB): #CHECKED - OK1
        if indA.get_Front() < indB.get_Front():
            return indA
        elif indA.get_Front() == indB.get_Front():
            if indA.get_Crowding() > indB.get_Crowding():
                return indA
        return indB



class NSGA2_v2:
    def __init__(self, n_ind = 0, mut_rate = 0, t_size = 0, DecimalPoints = 4, convergence = 0, ma_len = 0, ma_tol = 0, MaxGenerations = 500): 
        self.n_ind = n_ind
        self.mut_rate = mut_rate
        self.t_size = t_size
        self.DecimalPoints = DecimalPoints
        self.convergence = convergence
        self.ma_len = ma_len
        self.ma_tol = ma_tol
        self.MaxGenerations = MaxGenerations

    def set_population_limits(self, limits): 
        y = {}
        for key in limits:
            y[key] = limits[key].copy()
        self.limits = y

    def set_functions(self, evaluate, validate):
        self.evaluate = evaluate
        self.validate = validate

    def plot_population(self):
        for individual in self.current_pop:
            plt.plot(individual.get_ObjVal(0), individual.get_ObjVal(1), 'bo')
        # plt.plot(individual.get_ObjVal(0), individual.get_ObjVal(1), 'bo', label = "Individual")

    def run(self): 
        print("Generation 1!")
        self.create_first_gen() 
        self.evaluate_population(self.current_pop)
        self.current_pop = assign_fronts(self.current_pop)
        self.current_pop = assign_crowding(self.current_pop)
        self.CurrentGeneration = 1
        #Plot works for 2 variables only
        Objective1Vec = [Ind.get_ObjVal(0) for Ind in self.current_pop]
        Objective2Vec = [Ind.get_ObjVal(1) for Ind in self.current_pop]
        MinMaxPlot = [[min(Objective1Vec), max(Objective1Vec)], [min(Objective2Vec), max(Objective2Vec)]]

        # plt.ion()
        # plt.plot(Objective1Vec, Objective2Vec, 'o')
        # # PlotLines = AnimatedPlot[0]
        # plt.xlim(MinMaxPlot[0])
        # plt.ylim(MinMaxPlot[1])
        # # print(self.current_pop[0].GetInformation())
        # plt.grid()
        # plt.xlabel("f1")
        # plt.ylabel("f2")
        # plt.draw()
        # plt.pause(1)

        area_vec = []
        area_vec.append(area_under_Front(self.current_pop))
        moving_average = sum(area_vec)
        stillness_count = 0
        generation = 2
        while stillness_count < self.convergence:
            self.CurrentGeneration = generation
            if generation > self.MaxGenerations:
                break
            print(f"Generation {generation}")
            self.create_offspring() 
            self.reinsert() 
            if len(area_vec) > self.ma_len:
                area_vec.pop(0)
            AreaBelowPareto = area_under_Front(self.current_pop)
            area_vec.append(AreaBelowPareto)
            new_moving_average = sum(area_vec)/len(area_vec)
            if abs(new_moving_average/moving_average - 1) < self.ma_tol:
                stillness_count += 1
            else:
                stillness_count = 0
            moving_average = new_moving_average

            print(f"The Pareto is in convergence state for {stillness_count} generations")
            # Objective1Vec = [Ind.get_ObjVal(0) for Ind in self.current_pop]
            # Objective2Vec = [Ind.get_ObjVal(1) for Ind in self.current_pop]
            # plt.plot(Objective1Vec, Objective2Vec, 'o')
            # if min(Objective1Vec) < MinMaxPlot[0][0]:
            #     MinMaxPlot[0][0] = min(Objective1Vec)
            # if min(Objective2Vec) < MinMaxPlot[1][0]:
            #     MinMaxPlot[1][0] = min(Objective2Vec)
            # if max(Objective1Vec) > MinMaxPlot[0][1]:
            #     MinMaxPlot[0][1] = max(Objective1Vec)
            # if max(Objective2Vec) > MinMaxPlot[1][1]:
            #     MinMaxPlot[1][1] = max(Objective2Vec)
            # plt.xlim(MinMaxPlot[0])
            # plt.ylim(MinMaxPlot[1])
            # plt.grid()
            # plt.xlabel("f1")
            # plt.ylabel("f2")
            # plt.draw()
            # plt.pause(1)
            generation += 1

    def create_first_gen(self): #CHECKED - OK1 - OK2
        individuals = 0 
        chrom_pop = []
        first_gen = []
        while individuals < self.n_ind:
            chromossome = self.generate_chrom()
            if self.validate(chromossome) and chromossome not in chrom_pop:
                chrom_pop.append(chromossome)
                individuals += 1
        for chrom in chrom_pop:
            first_gen.append(Individual(chrom = chrom))
        self.current_pop = first_gen

    def create_offspring(self): #CHECKED - OK1
        init_chrom_pop = []
        chrom_pop = []
        length = len(self.current_pop)
        for individual in self.current_pop:
            init_chrom_pop.append(individual.get_chrom())
        population = strip_rejected(self.current_pop)
        while len(chrom_pop) < length:
            mother = self.select_tournament(population)
            father = self.select_tournament(population)
            son_chrom = self.crossover(mother.get_chrom(), father.get_chrom())
            son_chrom = self.mutate(son_chrom)
            if self.validate(son_chrom) and (son_chrom not in init_chrom_pop):
                chrom_pop.append(son_chrom)
        offspring = []
        for chrom in chrom_pop:
            offspring.append(Individual(chrom = chrom))
        self.evaluate_population(offspring)
        self.offspring = offspring

    def reinsert(self): #CHECKED - OK1
        pop_ini = self.current_pop.copy()
        pop_new = self.offspring.copy()
        length = len(pop_ini)
        pop_ini.extend(pop_new)
        pop_ini = strip_rejected(pop_ini)
        pop_ini = strip_equal(pop_ini)
        pop_ini = assign_fronts(pop_ini)
        pop_ini = assign_crowding(pop_ini)
        pop_final = []
        while len(pop_final) < length and len(pop_ini) > 0:
            best = pop_ini[0]
            i = 1
            hold = 0
            while i < len(pop_ini):
                if pop_ini[i].get_Front() < best.get_Front():
                    best = pop_ini[i]
                    hold = i
                elif pop_ini[i].get_Front() == best.get_Front():
                    if pop_ini[i].get_Crowding() > best.get_Crowding():
                        best = pop_ini[i]
                        hold = i
                i += 1
            pop_final.append(best)
            pop_ini.pop(hold)
        self.current_pop = pop_final

    def evaluate_population(self, pop): #CHECKED - OK1 - OK2
        for individual in pop:
            valid = self.validate(individual.get_chrom())
            if not valid:
                individual.set_valid(False)
                continue
            obj_vals, Information = self.evaluate(individual.get_chrom())
            if type(obj_vals) == int:
                print(individual.get_chrom())
            individual.set_ObjVal(obj_vals)
            individual.SetInformation(Information)

    def generate_chrom(self): #CHECKED - OK1 - OK2
        chrom = {}
        for key in self.limits:
            parameter = random.uniform(self.limits[key][0], self.limits[key][1])
            param = round(parameter, self.DecimalPoints)
            chrom[key] = param
        return chrom

    def crossover(self, mother, father): #CHECKED - OK1 - IT WAS NOT OK
        son_genes = {}
        for key in mother:
            chosen = random.random()
            if chosen > 0.5:
                son_genes[key] = mother[key]
            else:
                son_genes[key] = father[key]
        return son_genes

    def mutate(self, chrom): #CHECKED - OK1
        copy = chrom.copy()
        for key in copy:
            mut = random.random()
            if mut < self.mut_rate:
                parameter = random.uniform(self.limits[key][0], self.limits[key][1])
                param = round(parameter, self.DecimalPoints)
                copy[key] = param
        return copy

    def select_tournament(self, population): #CHECKED - OK1
        index_list = []
        for _ in range(self.t_size):
            rand_index = random.randint(0, len(population) - 1)
            while rand_index in index_list:
                rand_index = random.randint(0, len(population) - 1)
            index_list.append(rand_index)
        best = population[index_list[0]]
        for i in range(1, self.t_size): 
            new = population[index_list[i]]
            best = self.battle(best, new)
        return best

    def battle(self, indA, indB): #CHECKED - OK1
        if indA.get_Front() < indB.get_Front():
            return indA
        elif indA.get_Front() == indB.get_Front():
            if indA.get_Crowding() > indB.get_Crowding():
                return indA
        return indB













#Convergence through Pareto accumulator
# def run_GA_convergence_v2(n_ind, gene_limits, evaluate_func, valid_func = Auxiliary.return_true, mut_rate = 0.0, t_size = 2, gwcfc = 10, archive_size = -1, decimal_places = 4):
#     if archive_size == -1:
#         archive_size == n_ind*2
#     print("Geração 1")
#     archive = []
#     current_pop = create_first_gen(n_ind, evaluate_func, valid_func, gene_limits, decimal_places = decimal_places)
#     counter = 0
#     generation = 2
#     while counter < gwcfc: 
#         print(f"Geração {generation}")
#         offspring = create_offspring(current_pop, gene_limits, evaluate_func, valid_func, mut_rate = mut_rate, t_size = t_size, decimal_places = decimal_places)
#         current_pop = reinsert(current_pop, offspring)
#         archive, counter = update_archive(archive, current_pop, archive_size, counter)
#         print(counter)
#         generation += 1
#     return archive



# def create_first_gen_new(n_ind, evaluate_func, valid_func, limits, decimal_places = 4):
#     individuals = 0 
#     first_gen = []
#     limits = nup.ndarray(limits)
#     sampling = LHS(xlimits = limits)
#     while individuals < n_ind:
#         placeholder = n_ind
#         chrom_pop = sampling(n_ind - individuals)
#         chromossome = generate_chrom(limits, decimal_places)
#         if valid_func(chromossome) and chromossome not in chrom_pop:
#             chrom_pop.append(chromossome)
#             individuals += 1
#     for chrom in chrom_pop:
#         first_gen.append(Individual(chrom = chrom))
#     evaluate_population(first_gen, evaluate_func, valid_func)
#     first_gen = assign_fronts(first_gen)
#     first_gen = assign_crowding(first_gen)
#     return first_gen
    
  


def update_archive(archive, pop_new, archive_size, counter):
    # if len(archive) + len(pop_new) <= archive_size:
    #     archive.extend(pop_new)
    #     return archive, 0
    init_archive = archive.copy()
    # for ind in archive:
    #     ind.set_ID(1)
    # for ind in pop_new:
    #     ind.set_ID(2)
    pop_final = []
    archive.extend(pop_new)
    archive = strip_rejected(archive)
    archive = strip_equal(archive)
    archive = assign_fronts(archive)
    archive = assign_crowding(archive)
    archive = strip_not_pareto(archive)
    while len(pop_final) < archive_size and len(archive) > 0:
        best = archive[0]
        i = 1
        hold = 0
        while i < len(archive):
            if archive[i].get_Front() < best.get_Front():
                best = archive[i]
                hold = i
            elif archive[i].get_Front() == best.get_Front():
                if archive[i].get_Crowding() > best.get_Crowding():
                    best = archive[i]
                    hold = i
            i += 1
        pop_final.append(best)
        archive.pop(hold)
    if compare_pops(init_archive, pop_final):
        counter += 1
    else:
        counter = 0
    return pop_final, counter


# #Auxiliary
# def write_pop(population, file_name):
#     pass

def strip_not_pareto(population):
    i = 0
    while i < len(population):
        if population[i].get_Front() != 1:
            population.pop(i)
        else:
            i += 1
    return population


def compare_pops(pop1, pop2): #returns False if they are different, True if they are equal
    pop1 = sort_ObjVal(pop1, 0)
    pop2 = sort_ObjVal(pop2, 0)
    chrom_pop1 = []
    chrom_pop2 = []
    for ind in pop1:
        chrom_pop1.append(ind.get_chrom())
    for ind in pop2:
        chrom_pop2.append(ind.get_chrom())
    for i in range(len(chrom_pop1)):
        if chrom_pop1[i] != chrom_pop2[i]:
            return False
    return True






















def assign_fronts(pop): #CHECKED - OK1
    copy = pop.copy()
    new_pop = []
    front = 1
    while len(copy) > 0:
        assign_domination(copy)
        i = 0
        while i < len(copy):
            if copy[i].get_Dominated_counter() == 0:
                copy[i].set_Front(front)
                new_pop.append(copy[i])
                copy.pop(i)
            else:
                i += 1
        front += 1
    return new_pop

def assign_domination(population): #CHECKED - OK1
    n_objvals = len(population[0].get_ObjVal())
    length = len(population)
    if length == 1:
        population[0].set_Dominated_counter(0)
        return
    if length == 0:
        return
    for i in range(length):
        dominated_counter = 0
        for j in range(length):
            if j == i:
                continue
            if calculate_domination(population[j], population[i], n_objvals):
                dominated_counter += 1
        population[i].set_Dominated_counter(dominated_counter)

def calculate_domination(indA, indB, n_objvals): #CHECKED - OK1
    boolA = indA.get_ObjVal(0) <= indB.get_ObjVal(0)
    boolB = indA.get_ObjVal(0) < indB.get_ObjVal(0)
    for i in range(1, n_objvals):
        boolA = boolA and (indA.get_ObjVal(i) <= indB.get_ObjVal(i))
        boolB = boolB or (indA.get_ObjVal(i) < indB.get_ObjVal(i))
    return (boolA and boolB) #if True, A dominates B, if False, A does not dominate B

def assign_crowding(pop): #CHECKED - OK1
    population = pop.copy()
    n_objvals = len(population[0].get_ObjVal())
    length = len(population)
    for ind in population:
        ind.set_Crowding(0)
    for i in range(n_objvals):
        copy = sort_ObjVal(population, i)
        copy[0].set_Crowding(infinite)
        copy[-1].set_Crowding(infinite)
        minval = copy[0].get_ObjVal(i)
        maxval = copy[-1].get_ObjVal(i)
        for j in range(1, length - 1):
            crowding = abs((copy[j + 1].get_ObjVal(i) - copy[j - 1].get_ObjVal(i))/(maxval - minval)) 
            copy[j].set_Crowding(copy[j].get_Crowding() + crowding)
    return copy

def strip_equal(population): #CHECKED - OK1
    res = []
    chrom_vec = []
    chrom_vec.append(population[0].get_chrom())
    res.append(population[0])
    for i in range(1, len(population)):
        new_chrom = population[i].get_chrom()
        if new_chrom not in chrom_vec:
            chrom_vec.append(new_chrom)
            res.append(population[i])
    return res

def strip_rejected(population): #CHECKED - OK1 - OK2
    i = 0
    while i < len(population): 
        if not population[i].get_valid():
            population.pop(i)
            # i -= 1
        else:
            i += 1
    return population

def area_under_Front(population):
    pareto_front = return_pareto_front(population)
    area = calculate_hyper_volume(pareto_front)
    return area

def sort_ObjVal(population, objval_index): #CHECKED - OK1
    new_pop = []
    copy = population.copy()
    while len(copy) > 0:
        min_obj_val = infinite
        i = 0
        hold = 0
        while i < len(copy):
            obj_val = copy[i].get_ObjVal(objval_index)
            if obj_val < min_obj_val:
                min_obj_val = obj_val
                hold = i
            i += 1
        new_pop.append(copy[hold])
        copy.pop(hold)
    return new_pop

def return_pareto_front(population):
    population = assign_fronts(population)
    pareto_front = []
    min_front = infinite
    for ind in population:
        if ind.get_Front() < min_front:
            min_front = ind.get_Front()
    for ind in population:
        if ind.get_Front() == min_front:
            pareto_front.append(ind)
    return pareto_front

def calculate_hyper_volume(front):
    front = sort_ObjVal(front, 0)
    lenght = len(front)
    if lenght == 0:
        return 0
    n_objvals = len(front[0].get_ObjVal())
    main_matrix = []
    for i in range(n_objvals):
        obj_vector = []
        for ind in front:
            obj_vector.append(abs(ind.get_ObjVal(index = i)))
        main_matrix.append(obj_vector)
    area = 0
    for i in range(lenght):
        accumulator = 1
        if i == 0:
            for j in range(n_objvals):
                accumulator *= main_matrix[j][i]
        else:
            for j in range(n_objvals - 1):
                accumulator *= (main_matrix[j][i] - main_matrix[j][i - 1])
            accumulator *= main_matrix[-1][i]
        area += accumulator
    return area




    # def crossover1Point(self, mother, father):
    #     crossover_point = random.randint(0, len(mother) - 1)
    #     son1_genes = mother[:crossover_point]
    #     son1_genes.extend(father[crossover_point:])
    #     son2_genes = father[:crossover_point]
    #     son2_genes.extend(mother[crossover_point:])
    #     return son1_genes, son2_genes

    # def crossover2Point(self, mother, father):
    #     crossover_point1 = random.randint(0, len(mother) - 1)
    #     crossover_point2 = random.randint(0, len(mother) - 1)
    #     if (crossover_point2 == crossover_point1) or (abs(crossover_point1 - crossover_point2) == len(mother) - 1):
    #         return mother, father
    #     if crossover_point2 < crossover_point1:
    #         temp = crossover_point1
    #         crossover_point1 = crossover_point2
    #         crossover_point2 = temp
    #     son1_genes = mother[:crossover_point1]
    #     son1_genes.extend(father[crossover_point1:crossover_point2])
    #     son1_genes.extend(mother[crossover_point2:])
    #     son2_genes = father[:crossover_point1]
    #     son2_genes.extend(mother[crossover_point1:crossover_point2])
    #     son2_genes.extend(father[crossover_point2:])
    #     return son1_genes, son2_genes