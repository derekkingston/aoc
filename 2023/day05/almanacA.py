def find_map_index(m, source_idx):
    dest_idx = source_idx
    for b in m[2]:
        sz = b[2]
        if source_idx >= b[1] and source_idx < b[1]+sz:
            dest_idx = b[0] + (source_idx - b[1])
    return dest_idx

with open("example.txt") as f:
    entries = [line.rstrip() for line in f]

seeds = []
maps = []
for e in entries:
    if not e:
        continue
    if e.find(":") >= 0:
        [label, val] = e.split(":")
        if label == "seeds":
            seeds = [int(x) for x in val.rstrip().split()]
        elif label.rfind(" map") >= 0:
            key = label[0:label.rfind(" map")]
            [src, dst] = key.split("-to-")
            print("found map from " + src + " to " + dst)
            maps.append( (src, dst, []) )
    elif len(maps) > 0:
        maps[-1][2].append([int(x) for x in e.split()])

locs = []
for seed in seeds:
    soil = find_map_index(maps[0], seed)
    fert = find_map_index(maps[1], soil)
    watr = find_map_index(maps[2], fert)
    ligt = find_map_index(maps[3], watr)
    temp = find_map_index(maps[4], ligt)
    humd = find_map_index(maps[5], temp)
    loc  = find_map_index(maps[6], humd)
    locs.append(loc)

#print(locs)
print(min(locs))