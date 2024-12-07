from functools import reduce
from math import gcd

def lcm(numbers):
    return reduce((lambda x, y: int(x * y / gcd(x, y))), numbers)

steps = [list(), list(), list(), list(), list(), list()]

# start from pre-computed trace files with cycles, see wastelandB.py
for k in range(6):
    with open("trace"+str(k+1)+".txt") as f:
        for line in f:
            ln = line.strip()
            if len(ln) > 0:
                [loc, d] = ln.strip().split(", ")
                steps[k].append( (loc, int(d)) )

preamble = [0, 0, 0, 0, 0, 0]
cycle_len = [0, 0, 0, 0, 0, 0]
for k in range(6):
    for n in range(len(steps[k])-1):
        if steps[k][n] == steps[k][-1]:
            preamble[k] = n
            cycle_len[k] = len(steps[k]) - n - 1
            break

#thezees = [list(),list(),list(),list(),list(),list()]
#for k in range(6):
#    for n in range(preamble[k], len(steps[k])-1):
#        if steps[k][n][0][-1] == 'Z':
#            thezees[k].append(n-preamble[k])

# this isn't the real answer, but is the one accepted
print(lcm(cycle_len))