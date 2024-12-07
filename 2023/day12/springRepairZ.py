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
    return arrangements

def computeArrangements(springs, totals):
    arrangements = list()
    t = 0
    for s in range(len(springs)):
        if t < len(totals) and totals[t] < len(springs[s]):
            possibleInside = [totals[t]]
            forcedSeperator = 1
            while sum(possibleInside)+forcedSeperator+totals[t+1] <= len(springs[s]):
                t += 1
                forcedSeperator += 1
                possibleInside.append(totals[t])
                if t >= len(totals)-1:
                    break
            arrangements.append(findCombinations(springs[s], possibleInside))
        t += 1
    result = 1
    for x in arrangements:
        result = result * x
    print(result)
    return result

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

arrangements = 0
for e in entries:
    print()
    print(e)
    [record, totalsStr] = e.split(' ')
    springs = [x for x in record.split('.') if x]
    totals = [int(x) for x in totalsStr.split(',')]
    arrangements += computeArrangements(springs, totals)
print()
print("Total: " + str(arrangements))