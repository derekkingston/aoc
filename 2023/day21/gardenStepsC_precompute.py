import functools
import time

with open("input.txt") as f:
    m = [line.rstrip() for line in f]
M = len(m[0]) # map is square, equal in each dimension

@functools.cache
def getNeighbors(loc):
    neighbors = set()
    if loc[0] > 0 and m[loc[0]-1][loc[1]] != '#':
        neighbors.add( (loc[0]-1, loc[1]) )

    if loc[0] < len(m)-1 and m[loc[0]+1][loc[1]] != '#':
        neighbors.add(        (loc[0]+1, loc[1]) )

    if loc[1] > 0 and m[loc[0]][loc[1]-1] != '#':
        neighbors.add( (loc[0], loc[1]-1) )

    if loc[1] < len(m[0])-1 and m[loc[0]][loc[1]+1] != '#':
        neighbors.add(           (loc[0], loc[1]+1) )

    return neighbors

def minDistVertex(q, dst):
    c_best = q[0]
    v_best = dst[q[0][0]][q[0][1]]
    for c in q:
        if v_best > dst[c[0]][c[1]]:
            c_best = c
            v_best = dst[c[0]][c[1]]
    return c_best

def dijkstra(start_loc):
    # initialize dst to 'inf' using M^2 as inf
    dst = [ [M*M]*M for k in range(M) ]
    dst[start_loc[0]][start_loc[1]] = 0
    prv = list()
    for r in range(M):
        for c in range(M):
            if m[r][c] != '#':
                prv.append( (r,c) )

    while len(prv) > 0:
        nxt = minDistVertex(prv, dst)
        prv.remove(nxt)

        for n in getNeighbors(nxt):
            W = 1 # generalizes to weighted edge cost
            candidate = dst[nxt[0]][nxt[1]] + W
            if candidate < dst[n[0]][n[1]]:
                dst[n[0]][n[1]] = candidate
                prv.append(n)
    return dst

start = time.time()

start_loc = (0, 0)
for r in range(M):
    for c in range(M):
        if m[r][c] == 'S':
            start_loc = (r,c)

# center
print("Solving center wave")
dst = dijkstra(start_loc)
with open("center.txt", "w") as f:
    for r in dst:
        f.write(', '.join([str(x) for x in r]))
        f.write('\n')
print("Solving north wave")
dst = dijkstra( (0, (M-1)//2) )
with open("n.txt", "w") as f:
    for r in dst:
        f.write(', '.join([str(x) for x in r]))
        f.write('\n')
print("Solving east wave")
dst = dijkstra( ((M-1)//2, M-1) )
with open("e.txt", "w") as f:
    for r in dst:
        f.write(', '.join([str(x) for x in r]))
        f.write('\n')
print("Solving south wave")
dst = dijkstra( (M-1, (M-1)//2) )
with open("s.txt", "w") as f:
    for r in dst:
        f.write(', '.join([str(x) for x in r]))
        f.write('\n')
print("Solving west wave")
dst = dijkstra( ((M-1)//2, 0) )
with open("w.txt", "w") as f:
    for r in dst:
        f.write(', '.join([str(x) for x in r]))
        f.write('\n')
# north-west
print("Solving from north-west")
dst = dijkstra( (0, 0) )
with open("nw.txt", "w") as f:
    for r in dst:
        f.write(', '.join([str(x) for x in r]))
        f.write('\n')
# north-east
print("Solving from north-east")
dst = dijkstra( (0, M-1) )
with open("ne.txt", "w") as f:
    for r in dst:
        f.write(', '.join([str(x) for x in r]))
        f.write('\n')
# south-west
print("Solving from south-west")
dst = dijkstra( (M-1, 0) )
with open("sw.txt", "w") as f:
    for r in dst:
        f.write(', '.join([str(x) for x in r]))
        f.write('\n')
# south-east
print("Solving from south-east")
dst = dijkstra( (M-1, M-1) )
with open("se.txt", "w") as f:
    for r in dst:
        f.write(', '.join([str(x) for x in r]))
        f.write('\n')

print()
print("-------")
end = time.time()
print(end - start)

# 190 seconds to compute