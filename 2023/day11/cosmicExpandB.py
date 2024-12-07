expandRate = 1000000

with open("input.txt") as f:
    m = [line.rstrip() for line in f]

empty_row = [False for r in range(len(m))]
empty_col = [True for c in range(len(m[0]))]
for r in range(len(m)):
    if all([x == '.' for x in m[r]]):
        empty_row[r] = True
        continue
    for c in range(len(m[r])):
        if m[r][c] == '#':
            empty_col[c] = False

mr = -1
mc = -1
loc = list()
for r in range(len(m)):
    mr += 1
    if empty_row[r]:
        mr += expandRate-1
    mc = -1
    for c in range(len(m[r])):
        mc += 1
        if empty_col[c]:
            mc += expandRate-1
        if m[r][c] == '#':
            loc.append( (mr, mc) )

loc_dist = 0
for k in range(len(loc)):
    for j in range(k+1, len(loc)):
        loc_dist += abs(loc[k][0]-loc[j][0]) + abs(loc[k][1]-loc[j][1])

print(loc_dist)