import time

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

lowcount = 0
highcount = 0
for b in range(1000):
    pulse_stack = [('button', 'broadcaster', False)]
    while len(pulse_stack) > 0:
        pulse = pulse_stack.pop(0)
        if pulse[2]:
            highcount += 1
        else:
            lowcount += 1
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
print(lowcount)
print(highcount)
print(highcount*lowcount)

print()
print("-------")
end = time.time()
print(end - start)
