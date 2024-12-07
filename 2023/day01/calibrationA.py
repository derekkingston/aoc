with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

total_cal = 0
for e in entries:
    val = ""
    for i in e:
        if i.isdigit():
            #print("First: " + i)
            val = i
            break
    for i in reversed(e):
        if i.isdigit():
            #print("Second: " + i)
            val = val + i
            break
    #print("Full: " + val)
    total_cal += int(val)
    
print('Total calibration')
print(total_cal)