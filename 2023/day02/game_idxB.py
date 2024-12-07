with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

total_pwr = 0
for e in entries:
    e = e.lower()
    r = 0
    g = 0
    b = 0
    [gm, trials] = e.split(': ')
    tests = trials.split("; ")
    for t in tests:
        draws = t.split(', ')
        for d in draws:
            [balls, color] = d.split(' ')
            bls = int(balls)
            if color == 'red' and bls > r:
                r = bls
            if color == 'green' and bls > g:
                g = bls
            if color == 'blue' and bls > b:
                b = bls
    print(gm + ": red (" + str(r) + ") green (" + str(g) + ") blue (" + str(b) + ")")
    total_pwr += r*b*g

print(total_pwr)