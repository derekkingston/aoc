import time

def getNeighbors(m, loc):
    neighbors = set()
    if loc[0] > 0 and m[loc[0]-1][loc[1]] != '#':
        neighbors.add( (loc[0]-1, loc[1]) )

    if loc[0] < len(m)-1 and m[loc[0]+1][loc[1]] != '#':
        neighbors.add(        (loc[0]+1, loc[1]) )

    if loc[1] > 0 and m[loc[0]][loc[1]-1] != '#':
        neighbors.add( (loc[0], loc[1]-1) )

    if loc[1] < len(m[loc[0]])-1 and m[loc[0]][loc[1]+1] != '#':
        neighbors.add(                (loc[0], loc[1]+1) )
    return neighbors


with open("input.txt") as f:
    m = [line.rstrip() for line in f]

start = time.time()

locs = set()
for r in range(len(m)):
    for c in range(len(m[r])):
        if m[r][c] == 'S':
            locs.add((r,c))

for k in range(64):
    next_locs = set()
    for loc in locs:
        neighbors = getNeighbors(m, loc)
        for n in neighbors:
            next_locs.add(n)
    locs = next_locs

print(len(locs))

print()
print("-------")
end = time.time()
print(end - start)
