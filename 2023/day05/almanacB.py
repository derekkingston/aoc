def find_map_ranges(m, source_ranges):
    #print("Using map " + m[0] + " to " + m[1])
    #print(source_ranges)

    dest_ranges = []
    for srng in source_ranges:
        found_overlap = False
        sa = srng[0]
        sb = srng[0] + srng[1] - 1
        for b in m[2]:
            da = b[1]
            db = b[1] + b[2] - 1
            if sa < da and sb >=da:
                found_overlap = True
                if sb > db:
                    for x in find_map_ranges(m, [[sa, da-sa-1]]):
                        dest_ranges.append(x)
                    dest_ranges.append( [b[0], b[2]] )
                    for x in find_map_ranges(m, [[db+1, sb-db]]):
                        dest_ranges.append(x)
                else:
                    for x in find_map_ranges(m, [[sa, da-sa-1]]):
                        dest_ranges.append(x)
                    dest_ranges.append( [b[0], sb-da+1] )
            if sa >= da and sa <= db:
                found_overlap = True
                if sb > db:
                    dest_ranges.append( [b[0]+(sa-da), db-sa+1] )
                    for x in find_map_ranges(m, [[db+1, sb-db]]):
                        dest_ranges.append(x)
                else:
                    dest_ranges.append( [b[0]+(sa-da), srng[1]] )
        if not found_overlap:
            dest_ranges.append(srng)
    return dest_ranges

with open("input.txt") as f:
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
for s in range(len(seeds)):
    if s%2 == 1:
        continue;
    soil = find_map_ranges(maps[0], [[seeds[s], seeds[s+1]]])
    fert = find_map_ranges(maps[1], soil)
    watr = find_map_ranges(maps[2], fert)
    ligt = find_map_ranges(maps[3], watr)
    temp = find_map_ranges(maps[4], ligt)
    humd = find_map_ranges(maps[5], temp)
    loc  = find_map_ranges(maps[6], humd)
    for lc in loc:
        locs.append(lc[0])

#print(locs)
print(min(locs))