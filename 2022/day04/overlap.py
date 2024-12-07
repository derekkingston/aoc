with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

total_overlap = 0
any_overlap = 0
for e in entries:
    ix, iy = e.split(',', 1)
    x = list(map(int, ix.split('-')))
    y = list(map(int, iy.split('-')))

    # total overlap
    if x[0] >= y[0] and x[1] <= y[1]:
        total_overlap += 1
    elif y[0] >= x[0] and y[1] <= x[1]:
        total_overlap += 1

    # partial overlap
    if x[0] >= y[0] and x[0] <= y[1]:
        any_overlap += 1
    elif x[1] >= y[0] and x[1] <= y[1]:
        any_overlap += 1
    elif y[0] >= x[0] and y[0] <= x[1]:
        any_overlap += 1
    elif y[1] >= x[0] and y[1] <= x[1]:
        any_overlap += 1
print('Total overlap')
print(total_overlap)
print('Any overlap')
print(any_overlap)