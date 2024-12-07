with open("input.txt") as f:
    data = f.read()
entries = data.split('\n')

elf_cal = []
cal = 0
for e in entries:
    if not e:
        elf_cal.append(cal)
        cal = 0
    else:
        cal += int(e)
elf_cal.append(cal)

print('Top elf cal:')
print(max(elf_cal))

elf_cal.sort(reverse=True)
print('Top 3 elf cal:')
print(elf_cal[0] + elf_cal[1] + elf_cal[2])