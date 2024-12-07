def compute_gear_ratio(mtx, row, col):
    parts = set()
    if row > 0:
        if col > 0 and mtx[row-1][col-1] > -1:
            parts.add(mtx[row-1][col-1])
        if mtx[row-1][col] > -1:
            parts.add(mtx[row-1][col])
        if col+1 < len(mtx[row-1]) and mtx[row-1][col+1] > -1:
            parts.add(mtx[row-1][col+1])
    if col > 0 and mtx[row][col-1] > -1:
        parts.add(mtx[row][col-1])
    if col+1 < len(mtx[row]) and mtx[row][col+1] > -1:
        parts.add(mtx[row][col+1])
    if row+1 < len(mtx):
        if col > 0 and mtx[row+1][col-1] > -1:
            parts.add(mtx[row+1][col-1])
        if mtx[row+1][col] > -1:
            parts.add(mtx[row+1][col])
        if col+1 < len(mtx[row+1]) and mtx[row+1][col+1] > -1:
            parts.add(mtx[row+1][col+1])
    print("Gear at [" + str(row) + "][" + str(col) + "] has parts")
    print(parts)
    if len(parts) > 1:
        gear_ratio = 1
        for p in parts:
            gear_ratio *= p
        return gear_ratio
    return 0

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

m = []
for row in range(len(entries)):
    m.append([])
    for col in range(len(entries[row])):
        m[len(m)-1].append(-1)

potential_gears = []
for row in range(len(entries)):
    open_num = False
    candidate_part = ""
    start_idx = -1
    for col in range(len(entries[row])):
        if open_num:
            if not entries[row][col].isdigit():
                open_num = False
                for idx in range(start_idx, col):
                    m[row][idx] = int(candidate_part)
                if entries[row][col] == '*':
                    potential_gears.append( (row, col) )
            else:
                candidate_part += entries[row][col]
                if col+1 >= len(entries[row]):
                    open_num = False
                    for idx in range(start_idx, col+1):
                        m[row][idx] = int(candidate_part)
        else:
            if entries[row][col].isdigit():
                open_num = True
                start_idx = col
                candidate_part = entries[row][col]
                if col+1 >= len(entries[row]):
                    open_num = False
                    m[row][col] = int(candidate_part)
            elif entries[row][col] == '*':
                potential_gears.append( (row, col) )

gear_ratio_sum = 0
for g in potential_gears:
    gear_ratio_sum += compute_gear_ratio(m, g[0], g[1])
print(gear_ratio_sum)