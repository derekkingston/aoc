def has_cycle(trc):
    if len(trc) < 1:
        return False
    if len(trc) == 2:
        if trc[0] == trc[1]:
            return True
        else:
            return False
    # general case length > 2
    for k in range(len(trc)-1):
        if trc[k] == trc[-1]:
            return True
    return False

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

directions = ''
rlocations = dict()
llocations = dict()

for e in entries:
    if e.find('=') >= 0:
        [lc, pairs] = e.split(" = ")
        pairs = pairs.replace(')','')
        pairs = pairs.replace('(','')
        [l, r] = pairs.split(', ')
        llocations[lc] = l
        rlocations[lc] = r
    elif len(e) > 0:
        directions = e

loc = list(filter(lambda x: x[2] == 'A', list(rlocations.keys())))
steps = list()
for k in range(len(loc)):
    steps.append([ (loc[k], 0) ])

for k in range(len(loc)):
    d = 0
    c = 0
    print("----- " + str(k+1) + " -----")
    while not has_cycle(steps[k]):
        if c % 1000 == 0 and c != 0:
            print( str(int(c/1000)) )
        c = c + 1

        step_direction = directions[d]
        d = (d + 1) % len(directions)
        if step_direction == 'L':
            steps[k].append( (llocations[steps[k][-1][0]], d) )
        else:
            steps[k].append( (rlocations[steps[k][-1][0]], d) )

# save traces to files so no need to re-compute
for k in range(len(loc)):
    print("["  + str(k+1) + "] total trace len: " + str(len(steps[k])))
    with open("trace"+str(k+1)+".txt", "w") as f:
        for n in range(len(steps[k])):
            f.write(steps[k][n][0] + ", "+ str(steps[k][n][1]) + "\n")
