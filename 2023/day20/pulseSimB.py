import time
from functools import reduce
from math import gcd

def lcm(numbers):
    return reduce((lambda x, y: int(x * y / gcd(x, y))), numbers)

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

start = time.time()

c = dict()
for e in entries:
    [fullname, dests] = e.split(' -> ')
    name = fullname
    ch = ''
    if name[0] == '%' or name[0] == '&':
        name = fullname[1:]
        ch = fullname[0]
    c[name] = [ch, dests.split(', '), dict(), False]

terminal_nodes = []
for key, val in c.items():
    for d in val[1]:
        if d not in c:
            terminal_nodes.append(d)
for d in terminal_nodes:
    c[d] = ['', [], dict(), False]

for key, val in c.items():
    if val[0] == '&':
        for k, v in c.items():
            for d in v[1]:
                if d == key:
                    val[2][k] = False
high_pulse = dict()
for b in range(10000):
    pulse_stack = [('button', 'broadcaster', False)]
    while len(pulse_stack) > 0:
        pulse = pulse_stack.pop(0)
        # by analysis of 'input.txt', dg is the NAND gate that sends to rx
        if pulse[1] == 'dg' and pulse[2]:
            if pulse[0] not in high_pulse:
                high_pulse[pulse[0]] = [b]
            else:
                high_pulse[pulse[0]].append(b)
        #print(pulse)
        m = c[pulse[1]]
        if m[0] == '':
            for d in m[1]:
                pulse_stack.append( (pulse[1], d, pulse[2]) )
        elif m[0] == '%' and pulse[2] == False:
            m[3] = not m[3]
            for d in m[1]:
                pulse_stack.append( (pulse[1], d, m[3]) )
        elif m[0] == '&':
            m[2][pulse[0]] = pulse[2]
            sendv = True
            for k, v in m[2].items():
                sendv = sendv and v
            for d in m[1]:
                pulse_stack.append( (pulse[1], d, not sendv) )

periods = []
for k, v in high_pulse.items():
    periods.append(v[1]-v[0])
print(lcm(periods))

print()
print("-------")
end = time.time()
print(end - start)

# 162 second run time for 1 000 000 trials which was not enough