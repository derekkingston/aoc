with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

shr = []
for e in entries:
    a = e[0:int(len(e)/2)]
    b = e[int(len(e)/2):]
    for c in a:
        if c in b:
            shr.append(c)
            break

score = 0
for c in shr:
    if c.islower():
        score += int(ord(c) - ord('a')) + 1
    else:
        score += int(ord(c) - ord('A')) + 27
print(score)