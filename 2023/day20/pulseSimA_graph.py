import time
import networkx as nx

with open("exampleA.txt") as f:
    entries = [line.rstrip() for line in f]

start = time.time()

dependencyGraph = nx.DiGraph()
c = dict()
for e in entries:
    [fullname, dests] = e.split(' -> ')
    name = fullname
    ch = ''
    if name[0] == '%' or name[0] == '&':
        name = fullname[1:]
        ch = fullname[0]
    for d in dests.split(','):
        # name sends information to d
        dependencyGraph.add_edge(name, d)
    c[name] = (ch, dests.split(','), dict(), False)

for key, val in c.items():
    print('#### ' + key + ' ####')
    for a, b in nx.dfs_edges(dependencyGraph, key):
        print(a + ' --> ' + b)

print()
print("-------")
end = time.time()
print(end - start)
