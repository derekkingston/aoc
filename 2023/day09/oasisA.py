with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

extrap_sum = 0
for e in entries:
    m = [[int(x) for x in e.split(" ")]]

    # roll forward
    while any(m[-1]):
        y = []
        for i in range(1,len(m[-1])):
            y.append(m[-1][i] - m[-1][i-1])
        m.append(y)

    # roll back up
    m[-1].append(0)
    for row in reversed(range(len(m)-1)):
        m[row].append(m[row+1][-1]+m[row][-1])

    extrap_sum = extrap_sum + m[0][-1]
        
print(extrap_sum)