def near_symb(entries, row, col):
    symb = '!@#$%^&*()-+=_/?,<>'
    if row > 0:
        if col > 0 and entries[row-1][col-1] in symb:
            return True
        if entries[row-1][col] in symb:
            return True
        if col+1 < len(entries[row-1]) and entries[row-1][col+1] in symb:
            return True
    if col > 0 and entries[row][col-1] in symb:
        return True
    if col+1 < len(entries[row]) and entries[row][col+1] in symb:
        return True
    if row+1 < len(entries):
        if col > 0 and entries[row+1][col-1] in symb:
            return True
        if entries[row+1][col] in symb:
            return True
        if col+1 < len(entries[row+1]) and entries[row+1][col+1] in symb:
            return True
    return False

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

part_sum = 0
for row in range(len(entries)):
    open_num = False
    candidate_part = ""
    by_symb = False
    for col in range(len(entries[row])):
        if open_num:
            if not entries[row][col].isdigit():
                open_num = False
                if by_symb:
                    part_sum += int(candidate_part)
            else:
                candidate_part += entries[row][col]
                by_symb = by_symb or near_symb(entries, row, col)
                if col+1 >= len(entries[row]):
                    open_num = False
                    if by_symb:
                        part_sum += int(candidate_part)
        else:
            if entries[row][col].isdigit():
                open_num = True
                candidate_part = entries[row][col]
                by_symb = near_symb(entries, row, col)
                if col+1 >= len(entries[row]):
                    open_num = False
                    if by_symb:
                        part_sum += int(candidate_part)
    
print(part_sum)