bag = [('red', 12), ('green', 13), ('blue', 14)]

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

total_idx = 0
for e in entries:
    e = e.lower()
    [gm, trials] = e.split(': ')
    tests = trials.split("; ")
    test_passes = True
    for t in tests:
        draws = t.split(', ')
        for d in draws:
            [balls, color] = d.split(' ')
            bls = int(balls)
            for b in bag:
                if color == b[0] and bls > b[1]:
                    test_passes = False
    if test_passes:
        total_idx += int(gm.replace('game ', ''))

print(total_idx)