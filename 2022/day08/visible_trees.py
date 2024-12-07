
def isVisible(r, c, trees):
    east = trees[r][(c+1):]
    west = trees[r][0:c]
    north = []
    for row in range(r):
        north.append( trees[row][c])
    south = []
    row = r+1
    while row < len(trees[r]):
        south.append( trees[row][c])
        row += 1
    if trees[r][c] > max(east):
        return True
    if trees[r][c] > max(west):
        return True
    if trees[r][c] > max(north):
        return True
    if trees[r][c] > max(south):
        return True
    return False


with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

visible = []
trees = []
for e in entries:
    visible.append(list(e))
    trees.append([int(i) for i in e])
#print(trees)
for c in range(len(visible[0])):
    visible[0][c] = 'v'
for r in visible:
    r[0] = 'v'
    r[-1] = 'v'
for c in range(len(visible[-1])):
    visible[-1][c] = 'v'

# compute visibility
for row in range(len(visible)):
    for col in range(len(visible[row])):
        if visible[row][col] != 'v' and isVisible(row, col, trees):
            visible[row][col] = 'v'

# sum all visible
total_visible = 0
for row in range(len(visible)):
    for col in range(len(visible[row])):
        if visible[row][col] == 'v':
            total_visible += 1
print(total_visible)
