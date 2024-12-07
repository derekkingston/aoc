def findCombinations(record, worklist):
    arrangements = 0
    # iterative DFS counting valid leaves
    S = [record]
    while len(S) > 0:
        v = S.pop()
        c = v.find('?')
        cdot = list(v)
        cdot[c] = '.'
        if validLeaf(cdot, worklist):
            arrangements += 1
        elif edgeAllowed(cdot, worklist):
            S.append(''.join(cdot))
    return arrangements

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

arrangements = 0
for e in entries:
    print()
    print(e)
    [record, totalsStr] = e.split(' ')
    totals = [int(x) for x in totalsStr.split(',')]
    arrangements += findCombinations(record, totals)
print()
print("Total: " + str(arrangements))