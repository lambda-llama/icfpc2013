import itertools

def f(it):
    s = ""
    for i in it:
        s += "(Op1 %s " % i
    s += "e"
    for i in it:
        s += ")"
    return s
    

m = {}
l = ["Shr1", "Shr4", "Shr16"]
for i in range(2, 10):
    for r in itertools.combinations_with_replacement(l, i):
        m[tuple(r)] = sorted(r, key=(lambda x: int(x[3:])))

ll = []
for rule in m:
    s = "go "
    s += f(rule)
    s += " = Right "
    s += f(m[rule])
    ll.append(s)

ll = sorted(ll, key=(lambda x: x.count("(")), reverse=True)
for i in ll:
    print(i)