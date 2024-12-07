with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

shr = []
e = 0
while e < len(entries):
    a = entries[e]
    b = entries[e+1]
    c = entries[e+2]
    e += 3

    for d in a:
        if d in b and d in c:
            shr.append(d)
            break

score = 0
for c in shr:
    if c.islower():
        score += int(ord(c) - ord('a')) + 1
    else:
        score += int(ord(c) - ord('A')) + 27
print(score)