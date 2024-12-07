with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

directions = ''
rlocations = dict()
llocations = dict()

for e in entries:
    if e.find('=') >= 0:
        [loc, pairs] = e.split(" = ")
        pairs = pairs.replace(')','')
        pairs = pairs.replace('(','')
        [l, r] = pairs.split(', ')
        llocations[loc] = l
        rlocations[loc] = r
    elif len(e) > 0:
        directions = e

loc = 'AAA'
steps = 0
d = 0
while loc != 'ZZZ':
    if steps % 1000 == 0:
        print(steps)
    step_direction = directions[d]
    d = d + 1
    if d >= len(directions):
        d = 0
    
    #print('From ' + loc + ' go ' + step_direction)
    if step_direction == 'L':
        loc = llocations[loc]
    else:
        loc = rlocations[loc]
    steps = steps + 1

print("Result: " + str(steps))

