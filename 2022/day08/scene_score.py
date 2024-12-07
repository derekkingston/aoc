
def calcScore(r, c, trees):
    # east score
    east = 0
    col = c+1
    while col < len(trees[r]):
        east += 1
        if trees[r][col] >= trees[r][c]:
            break
        col += 1
    # west score
    west = 0
    col = c-1
    while col >= 0:
        west += 1
        if trees[r][col] >= trees[r][c]:
            break
        col -= 1
    # north score
    north = 0
    row = r-1
    while row >= 0:
        north += 1
        if trees[row][c] >= trees[r][c]:
            break
        row -= 1
    # south score
    south = 0
    row = r+1
    while row < len(trees[r]):
        south += 1
        if trees[row][c] >= trees[r][c]:
            break
        row += 1
    return east*west*north*south
    

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

scores = []
trees = []
for e in entries:
    trees.append([int(i) for i in e])
    scores.append([0*int(i) for i in e])
#print(trees)

# compute scenic score
for row in range(len(trees)):
    for col in range(len(trees[row])):
        scores[row][col] = calcScore(row, col, trees)

# find best
max_score = 0
for row in range(len(scores)):
    for col in range(len(scores[row])):
        if scores[row][col] > max_score:
            max_score = scores[row][col]
print(max_score)
