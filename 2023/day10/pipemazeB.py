def nextStep(q, v, e):
    p = q
    d = e
    #print("Visiting '" + v + "' at (" + str(p[0]) + ", " + str(p[1]) + ") with direction " + d)
    if v == '|' and d == 'S':
        p[0] = p[0]+1 # continue south
    elif v == '|' and d == 'N':
        p[0] = p[0]-1 # continue north
    elif v == '-' and d == 'W':
        p[1] = p[1]-1 # continue west
    elif v == '-' and d == 'E':
        p[1] = p[1]+1 # continue east
    elif v == 'L' and d == 'S':
        d = 'E'
        p[1] = p[1]+1
    elif v == 'L' and d == 'W':
        d = 'N'
        p[0] = p[0]-1
    elif v == 'J' and d == 'S':
        d = 'W'
        p[1] = p[1]-1
    elif v == 'J' and d == 'E':
        d = 'N'
        p[0] = p[0]-1
    elif v == '7' and d == 'N':
        d = 'W'
        p[1] = p[1]-1
    elif v == '7' and d == 'E':
        d = 'S'
        p[0] = p[0]+1
    elif v == 'F' and d == 'N':
        d = 'E'
        p[1] = p[1]+1
    elif v == 'F' and d == 'W':
        d = 'S'
        p[0] = p[0]+1
    return [p, d]

# input: [True, False, False, True]
# example B: [False, True, False, True]
start_nsew = [False, True, False, True]
with open("input.txt") as f:
    m = [line.rstrip() for line in f]

# find start position
st = (0, 0)
io_mark = list()
for r in range(len(m)):
    io_mark.append(list())
    for c in range(len(m[r])):
        if m[r][c] == 'S':
            st = (r, c)
            io_mark[-1].append('X')
        else:
            io_mark[-1].append('?')

# mark the pipe with X
p = [st[0], st[1]-1]
d = 'W'
while p[0] != st[0] or p[1] != st[1]:
    io_mark[p[0]][p[1]] = 'X'
    [p, d] = nextStep(p, m[p[0]][p[1]], d)

enclosed_count = 0
for r in range(len(m)):
    cmark = 'O'
    for c in range(len(m[r])):
        v = m[r][c]
        x = io_mark[r][c]
        if x != 'X':
            io_mark[r][c] = cmark
            if cmark == 'I':
                enclosed_count += 1
        if v in '|LJS' and x == 'X' and cmark == 'O':
            cmark = 'I'
        elif v in '|LJS' and x == 'X' and cmark == 'I':
            cmark = 'O'

print(enclosed_count)

