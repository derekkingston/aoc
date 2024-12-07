import time

def singleMirror(m, a):
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
        if isMirror and c != a[0]:
            return [c, 0]
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
        if isMirror and r != a[1]:
            return [0, r]
    return [0, 0]

def processMirror(m):
    [avoidLeft, avoidAbove] = singleMirror(m, [0, 0])
    for r in range(len(m)):
        for c in range(len(m[0])):
            old_char = m[r][c]
            new_char = '#' if old_char == '.' else '.'
            row = list(m[r])
            row[c] = new_char
            m[r] = ''.join(row)
            [colsLeft, rowsAbove] = singleMirror(m, [avoidLeft, avoidAbove])
            row = list(m[r])
            row[c] = old_char
            m[r] = ''.join(row)
            if colsLeft > 0:
                return [colsLeft, 0]
            if rowsAbove > 0:
                return [0, rowsAbove]
    return [0, 0]


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

# 70314 too high
# 36771
