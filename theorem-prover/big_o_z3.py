from z3 import *

set_param(proof=True)


UsingLinkedList = Const('UsingLinkedList', BoolSort())

GetNext = Const('GetNext', IntSort())
CanGetNext = Const('CanGetNext', BoolSort())

UsingLinkedListForGetNext = Const('UsingLinkedListForGetNext', BoolSort())

axioms = [
    Implies(UsingLinkedListForGetNext, UsingLinkedList),
    Implies(UsingLinkedListForGetNext, GetNext == 1),
    CanGetNext == Or(UsingLinkedListForGetNext),
    CanGetNext
]

# opt = Optimize()
# opt.add(axioms)
# mGetNext = opt.minimize(GetNext)

# print opt.check()
# print opt.model()
# print mGetNext.value()


impls = [
    ("getNext", { "getByIndex": 0 }),
    ("getFirst", { "getByIndex": 0 }),
    ("getByIndex", { "getFirst": 0, "getNext": 1 })
]

structures = {
    "LinkedList": {
        'getFirst': 1,
        'getNext': 1
    }
}

impls2 = [
    ('getNext', 0),
    ('getFirst', 0),
    ("getByIndex", { "getFirst": 0, "getNext": 1 })
]

def multiply(power1, power2):
    return power1 + power2

def add(power1, power2):
    return If(power1 > power2, power1, power2)

def big_o_sum(items):
    if len(items) == 0:
        return 0
    elif len(items) == 1:
        return items[0]
    else:
        return add(items[0], big_o_sum(items[1:]))



def calculate_times_for_impls(impls):
    axioms = []

    method_vars = {}

    impls_for_methods = {}

    def get_method_var(name):
        if name in method_vars:
            return method_vars[name]
        else:
            res = Const(name + "-time", IntSort()), Const(name + "-doable", BoolSort())
            method_vars[name] = res
            return res

    for (idx, impl) in enumerate(impls):
        method_name, cost = impl
        name = 'Impl-' + method_name + '-' + str(idx)

        if isinstance(cost, dict):
            used_methods = list(cost)
            if len(cost) == 0:
                cost_expr = 0
            else:
                cost_expr = 0
                for method in used_methods:
                    factor = multiply(get_method_var(method)[0], cost[method])
                    cost_expr = add(cost_expr, factor)
        else:
            cost_expr = cost

        using = Const('Using-' + name, BoolSort())

        method_time, can_do_method = get_method_var(method_name)

        time_axiom = Implies(using, method_time == cost_expr)
        axioms.append(time_axiom)

        if can_do_method in impls_for_methods:
            impls_for_methods[can_do_method].append(using)
        else:
            impls_for_methods[can_do_method] = [using]


    for impl in impls_for_methods:
        axioms.append(Implies(impl, Or(*impls_for_methods[impl])))

    opt = Optimize()
    opt.add(axioms + [get_method_var('getByIndex')[1]])
    mGetNext = opt.minimize(get_method_var('getByIndex')[0])
    print opt.check()
    print opt.model()
    print mGetNext.value()

class ImplCalculator:
    @staticmethod
    def calculate(impls, goals):
        calculator = ImplCalculator()
        calculator.add_axioms_from_impls(impls)
        calculator.add_goal_impls(goals)
        print calculator.opt.check()
        print calculator.opt.model()

    def __init__(self):
        self.opt = Optimize()
        self.literals = {}

    def cost_literal(self, method_name):
        literal_name = "cost-" + method_name
        if literal_name not in self.literals:
            self.literals[literal_name] = Const(literal_name, IntSort())
        return self.literals[literal_name]

    def can_do_literal(self, method_name):
        literal_name = "canDo-" + method_name
        if literal_name not in self.literals:
            self.literals[literal_name] = Const(literal_name, BoolSort())
        return self.literals[literal_name]

    def convert_cost_to_cost_expr(self, cost):
        if isinstance(cost, dict):
            used_methods = list(cost)
            if len(cost) == 0:
                return 0
            else:
                cost_expr = 0
                for method in used_methods:
                    factor = multiply(self.cost_literal(method), cost[method])
                    cost_expr = add(cost_expr, factor)

                return cost_expr
        else:
            return cost

    def convert_cost_to_can_do_expr(self, cost):
        if isinstance(cost, dict):
            return And([self.can_do_literal(x) for x in cost])
        else:
            return True


    def add_axioms_from_impls(self, impls):
        axioms = []
        impls_for_methods = {}

        for (idx, impl) in enumerate(impls):
            method_name, cost = impl
            name = 'Impl-' + method_name + '-' + str(idx)

            using = Const('Using-' + name, BoolSort())

            cost_expr = self.convert_cost_to_cost_expr(cost)
            method_time = self.cost_literal(method_name)
            time_axiom = Implies(using, method_time == cost_expr)
            axioms.append(time_axiom)

            can_do_axiom = Implies(using, self.convert_cost_to_can_do_expr(cost))
            axioms.append(can_do_axiom)

            if method_name in impls_for_methods:
                impls_for_methods[method_name].append(using)
            else:
                impls_for_methods[method_name] = [using]

        for method_name in impls_for_methods:
            axioms.append(Implies(self.can_do_literal(method_name),
                Or(*impls_for_methods[method_name])))

        self.opt.add(axioms)

    def add_goal_impls(self, goals):
        for goal in goals:
            self.opt.add(self.can_do_literal(goal))
            self.opt.minimize(self.cost_literal(goal))


    # opt = Optimize()
    # opt.add(axioms + [get_method_var('getByIndex')[1]])
    # mGetNext = opt.minimize(get_method_var('getByIndex')[0])
    # print opt.check()
    # print opt.model()
    # print mGetNext.value()

print ImplCalculator.calculate(impls2, ["getByIndex"])
