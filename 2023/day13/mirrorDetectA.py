import time

def processMirror(m):
    colsLeft = 0
    for c in range(1, len(m[0])):
        left = c-1
        right = c
        isMirror = True
        while left >= 0 and right < len(m[0]) and isMirror:
            for row in range(len(m)):
                if m[row][left] != m[row][right]:
                    isMirror = False
                    break
            left -= 1
            right += 1
        if isMirror:
            colsLeft = max([colsLeft, c])

    rowsAbove = 0
    for r in range(1, len(m)):
        above = r-1
        below = r
        isMirror = True
        while above >= 0 and below < len(m) and isMirror:
            if m[above] != m[below]:
                isMirror = False
                break
            above -= 1
            below += 1
        if isMirror:
            rowsAbove = max([rowsAbove, r])

    print("(" + str(colsLeft) + ", " + str(rowsAbove) + ")")
    return [colsLeft, rowsAbove]

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

start = time.time()

res = 0
mirror = []
for e in entries:
    if e == '':
        [colsLeft, rowsAbove] = processMirror(mirror)
        res += colsLeft + 100*rowsAbove
        mirror = []
    else:
        mirror.append(e)
[colsLeft, rowsAbove] = processMirror(mirror)
res += colsLeft + 100*rowsAbove
print(res)

print()
print("-------")
end = time.time()
print(end - start)

# 27326 too low