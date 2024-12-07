def validLeaf(record, worklist):
    if '?' in record:
        return False
    r = ''.join(record)
    springs = [x for x in r.split('.') if x]
    if len(springs) != len(worklist):
        return False
    for k in range(len(worklist)):
        if len(springs[k]) != worklist[k]:
            return False
    return True

def edgeAllowed(record, worklist):
    if '?' not in record:
        return False
    return True

def findCombinations(record, worklist):
    arrangements = 0
    # short cut all ? and single value to place
    if all(x=='?' for x in record) and len(worklist) == 1: 
        return len(record)-worklist[0]+1
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
        chash = list(v)
        chash[c] = '#'
        if validLeaf(chash, worklist):
            arrangements += 1
        elif edgeAllowed(chash, worklist):
            S.append(''.join(chash))
    print(arrangements)
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