def minDistVertex(q, dst):
    c_best = q[0]
    v_best = dst[q[0][0]][q[0][1]]
    for c in q:
        if v_best > dst[c[0]][c[1]]:
            c_best = c
            v_best = dst[c[0]][c[1]]
    return c_best

def findNeighbors(c, m):
    neighbors = []
    nsew = [False, False, False, False]
    v = m[c[0]][c[1]] 
    if v == 'S':
        nsew = [True, False, False, True]
    elif v == '|':
        nsew = [True, True, False, False]
    elif v == '-':
        nsew = [False, False, True, True]
    elif v == 'L':
        nsew = [True, False, True, False]
    elif v == 'J':
        nsew = [True, False, False, True]
    elif v == '7':
        nsew = [False, True, False, True]
    elif v == 'F':
        nsew = [False, True, True, False]

    if nsew[0] and c[0] > 0 and m[c[0]-1][c[1]] != '.':
        neighbors.append( (c[0]-1, c[1]) )
    if nsew[1] and c[0] < (len(m)-1) and m[c[0]+1][c[1]] != '.':
        neighbors.append( (c[0]+1, c[1]) )
    if nsew[2] and c[1] < (len(m[c[0]])-1) and m[c[0]][c[1]+1] != '.':
        neighbors.append( (c[0], c[1]+1) )
    if nsew[3] and c[1] > 0 and m[c[0]][c[1]-1] != '.':
        neighbors.append( (c[0], c[1]-1) )
    return neighbors

with open("input.txt") as f:
    m = [line.rstrip() for line in f]

# max distance value (this is inf effectively)
M = 1
for r in m:
    M = M + len(r)

dst = list()
prv = list()
for r in range(len(m)):
    dst.append(list())
    for c in range(len(m[r])):
        if m[r][c] == 'S':
            dst[-1].append(0)
        else:
            dst[-1].append(M)
        if m[r][c] != '.':
            prv.append( (r,c) )

while len(prv) > 0:
    nxt = minDistVertex(prv, dst)
    prv.remove(nxt)

    for n in findNeighbors(nxt, m):
        W = 1 # generalizes to weighted edge cost
        candidate = dst[nxt[0]][nxt[1]] + W
        if candidate < dst[n[0]][n[1]]:
            dst[n[0]][n[1]] = candidate
            prv.append(n)

# remove inf replacement value
for r in range(len(dst)):
    for c in range(len(dst[r])):
        if dst[r][c] == M:
            dst[r][c] = -1

print(max(map(lambda x: max(x), dst)))