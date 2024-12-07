with open("input.txt") as f:
    small_m = [line.rstrip() for line in f]

empty_row = [False for r in range(len(small_m))]
empty_col = [True for c in range(len(small_m[0]))]
for r in range(len(small_m)):
    if all([x == '.' for x in small_m[r]]):
        empty_row[r] = True
        continue
    for c in range(len(small_m[r])):
        if small_m[r][c] == '#':
            empty_col[c] = False

# expand map
m = [list() for i in range(len(small_m)+empty_row.count(True))]
for r in range(len(m)):
    m[r] = ['.' for i in range(len(small_m[0])+empty_col.count(True))]

mr = -1
mc = -1
loc = list()
for r in range(len(small_m)):
    mr += 1
    if empty_row[r]:
        mr += 1
    mc = -1
    for c in range(len(small_m[r])):
        mc += 1
        if empty_col[c]:
            mc += 1
        if small_m[r][c] == '#':
            m[mr][mc] = '#'
            loc.append( (mr, mc) )

#for r in m:
#    print(''.join(r))
#print(loc)

loc_dist = 0
for k in range(len(loc)):
    for j in range(k+1, len(loc)):
        loc_dist += abs(loc[k][0]-loc[j][0]) + abs(loc[k][1]-loc[j][1])

print(loc_dist)