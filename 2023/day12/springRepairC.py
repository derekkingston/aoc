import functools
import time

@functools.cache
def findCombinations(record, worklist):
    # base case, record is empty
    #   if worklist is also empty, then there is 1 combination
    #   otherwise, there are no valid combinations
    if record == "":
        return 1 if worklist == () else 0

    # base case, worklist is empty
    #   if there are no other # in the record, then only 1 combination
    #   otherwise, there are no valid combinations
    if worklist == ():
        return 1 if "#" not in record else 0
    
    # base case, only ? remain
    if all(x=='?' for x in record):
        if len(worklist) == 1 and worklist[0] <= len(record): 
            return len(record)-worklist[0]+1
        elif sum(worklist)+len(worklist)-1 > len(record):
            # can't possibly fit
            return 0
    
    arrangements = 0
    # work left to right, check first character and first expected worklist[0] value

    # case 1, treat ? as .
    # if it is already a dot, then do the same thing
    if record[0] == "." or record[0] == "?":
        arrangements += findCombinations(record[1:], worklist)

    # case 2, treat all ? as # in next worklist[0] block
    # if first character is already a #, then do the same thing
    if record[0] == "#" or record[0] == "?":
        valid = True
        if worklist[0] > len(record):
            # not enough space for the worklist[0] block
            valid = False
        elif '.' in record[:worklist[0]]:
            # no dots allowed in worklist[0] block
            valid = False
        elif worklist[0] < len(record) and record[worklist[0]] == '#':
            # theres enough space for the block, but a forced # follows it
            # which makes it too long
            valid = False
        
        if valid:
            # compute the combination having completed block worklist[0]
            arrangements += findCombinations(record[worklist[0]+1:], worklist[1:])
    return arrangements

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

start = time.time()
arrangements = 0
for e in entries:
    print()
    print(e)
    [r, totalsStr] = e.split(' ')
    totals = tuple(map(int, totalsStr.split(',')))

    record = '?'.join([r]*5)
    totals *= 5

    a = findCombinations(record, totals)
    print(a)
    arrangements += a
print()
print("Total: " + str(arrangements))
end = time.time()
print(end - start)