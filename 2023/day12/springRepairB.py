def validLeaf(record, worklist):
    if '?' in record:
        return 0
    r = ''.join(record)
    springs = [x for x in r.split('.') if x]
    if len(springs) != len(worklist):
        return 0
    for k in range(len(worklist)):
        if len(springs[k]) != worklist[k]:
            return 0
    return 1

def edgeAllowed(record, worklist):
    if '?' not in record:
        return False
    r = ''.join(record)
    # forward check
    q_idx = r.find('?')
    rc = r[:q_idx]
    springs = [x for x in rc.split('.') if x]
    if len(springs) == 0:
        return True
    if len(springs) > len(worklist):
        return False
    for k in range(len(springs)-1):
        if len(springs[k]) != worklist[k]:
            return False
    # backwards check
    q_idx = r.rfind('?')
    rc = r[(q_idx+1):]
    springs = [x for x in rc.split('.') if x]
    if len(springs) == 0:
        return True
    if len(springs) > len(worklist):
        return False
    W = len(worklist)-1
    K = len(springs)-1
    for k in range(len(springs)-1):
        if len(springs[K-k]) != worklist[W-k]:
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

        # expand a-type
        c = v.find('?#')
        has_a_type = False
        if c != -1:
            has_a_type = True
            cdot = list(v)
            cdot[c] = '.'
            a = validLeaf(cdot, worklist)
            if a > 0:
                arrangements += a
            elif edgeAllowed(cdot, worklist):
                S.append(''.join(cdot))
            chash = list(v)
            chash[c] = '#'
            a = validLeaf(chash, worklist)
            if a > 0:
                arrangements += a
            elif edgeAllowed(chash, worklist):
                S.append(''.join(chash))

        # expand b-type
        c = v.find('#?')
        has_b_type = False
        if c != -1 and not has_a_type:
            has_b_type = True
            cdot = list(v)
            cdot[c+1] = '.'
            a = validLeaf(cdot, worklist)
            if a > 0:
                arrangements += a
            elif edgeAllowed(cdot, worklist):
                S.append(''.join(cdot))
            chash = list(v)
            chash[c+1] = '#'
            a = validLeaf(chash, worklist)
            if a > 0:
                arrangements += a
            elif edgeAllowed(chash, worklist):
                S.append(''.join(chash))
        
        # expand next ? if not next to a hash
        if not has_a_type and not has_b_type:
            c = v.find('?')
            cdot = list(v)
            cdot[c] = '.'
            a = validLeaf(cdot, worklist)
            if a > 0:
                arrangements += a
            elif edgeAllowed(cdot, worklist):
                S.append(''.join(cdot))
            chash = list(v)
            chash[c] = '#'
            a = validLeaf(chash, worklist)
            if a > 0:
                arrangements += a
            elif edgeAllowed(chash, worklist):
                S.append(''.join(chash))

    return arrangements

with open("example.txt") as f:
    entries = [line.rstrip() for line in f]

arrangements = 0
for e in entries:
    print()
    print(e)
    [r, totalsStr] = e.split(' ')
    totals = [int(x) for x in totalsStr.split(',')]

    t = []
    for k in range(5):
        for tl in totals:
            t.append(tl)
    a = [0 for i in range(16)]
    a[0] = findCombinations(r+'.'+r+'.'+r+'.'+r+'.'+r, t)
    print(a[0])
    a[1] = findCombinations(r+'.'+r+'.'+r+'.'+r+'#'+r, t)
    print(a[1])
    a[2] = findCombinations(r+'.'+r+'.'+r+'#'+r+'.'+r, t)
    print(a[2])
    a[3] = findCombinations(r+'.'+r+'.'+r+'#'+r+'#'+r, t)
    print(a[3])
    a[4] = findCombinations(r+'.'+r+'#'+r+'.'+r+'.'+r, t)
    print(a[4])
    a[5] = findCombinations(r+'.'+r+'#'+r+'.'+r+'#'+r, t)
    print(a[5])
    a[6] = findCombinations(r+'.'+r+'#'+r+'#'+r+'.'+r, t)
    print(a[6])
    a[7] = findCombinations(r+'.'+r+'#'+r+'#'+r+'#'+r, t)
    print(a[7])
    a[8] = findCombinations(r+'#'+r+'.'+r+'.'+r+'.'+r, t)
    print(a[8])
    a[9] = findCombinations(r+'#'+r+'.'+r+'.'+r+'#'+r, t)
    print(a[9])
    a[10] = findCombinations(r+'#'+r+'.'+r+'#'+r+'.'+r, t)
    print(a[10])
    a[11] = findCombinations(r+'#'+r+'.'+r+'#'+r+'#'+r, t)
    print(a[11])
    a[12] = findCombinations(r+'#'+r+'#'+r+'.'+r+'.'+r, t)
    print(a[12])
    a[13] = findCombinations(r+'#'+r+'#'+r+'.'+r+'#'+r, t)
    print(a[13])
    a[14] = findCombinations(r+'#'+r+'#'+r+'#'+r+'.'+r, t)
    print(a[14])
    a[15] = findCombinations(r+'#'+r+'#'+r+'#'+r+'#'+r, t)
    print(a[15])
    arrangements += sum(a)
print()
print("Total: " + str(arrangements))

#   66312687202 too low   | pow(x,5)
#  675392268341 too low   | pow(max(f,b),4)
# 3384337640277