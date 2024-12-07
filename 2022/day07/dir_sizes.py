from anytree import AnyNode, RenderTree, PostOrderIter

def childByID(id, children):
    for c in children:
        if c.id == id:
            return c
    return None

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

# create the file tree
root = AnyNode(id="root", files=dict(), dsize=0)
p = root
listmode = False
for e in entries:
    if e.startswith('$ cd '):
        listmode = False
        dolla, cd, pth = e.split(' ')
        if pth == '/':
            p = root
        elif pth == '..':
            p = p.parent
        else:
            p = childByID(pth, p.children)
            if p is None:
                print('!! Unexpected child from ID: "' + e + '" !!')
    elif e.startswith('$ ls'):
        listmode = True
    elif listmode and e.startswith('dir'):
        d, dId = e.split(' ')
        AnyNode(id=dId, parent=p, files=dict(), dsize=0)
    elif listmode:
        sz, fId = e.split(' ')
        p.files[fId] = int(sz)
    else:
        print('!! Unexpected line: "' + e + '" !!')

# compute sizes for each directory (node)
for node in PostOrderIter(root):
    for fname, fsize in node.files.items():
        node.dsize += fsize
    for child in node.children:
        node.dsize += child.dsize

# print the file tree
print(RenderTree(root))

# print sum of small directories
r = [node.dsize for node in PostOrderIter(root, filter_=lambda n: n.dsize < 100000)]
print("Small directory sum: " + str(sum(r)))

# free up space
totalSize = root.dsize
print("Total size: " + str(totalSize))
unusedSpace = 70000000 - totalSize
print("Unused space: " + str(unusedSpace))
mustDelete = 30000000 - unusedSpace
print("To free up: " + str(mustDelete))
bestDir = root.id
bestSize = root.dsize
for node in PostOrderIter(root):
    if node.dsize >= mustDelete and node.dsize <= bestSize:
        bestSize = node.dsize
        bestDir = node.id
print("Best directory to delete: '" + bestDir + "' with size: " + str(bestSize))
