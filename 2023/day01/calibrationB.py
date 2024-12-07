digi = [ ('one', '1'), 
         ('two', '2'),
         ('three', '3'),
         ('four', '4'),
         ('five', '5'),
         ('six', '6'), 
         ('seven', '7'),
         ('eight', '8'), 
         ('nine', '9') ]

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

total_cal = 0
for e in entries:
    e = e.lower()

    # find index of first numerical digit
    val = ""
    first_digit = ("", len(e))
    for i in e:
        if i.isdigit():
            val = i
            break
    if val:
        first_digit = (val, e.index(val))

    for d in digi:
        idx = e.find(d[0])
        if idx != -1 and idx < first_digit[1]:
            first_digit = (d[1], idx)

    # find index of last numerical digit
    val = ""
    second_digit = ("", -1)
    for i in reversed(e):
        if i.isdigit():
            val = i
            break
    if val:
        second_digit = (val, e.rindex(val))

    for d in digi:
        idx = e.rfind(d[0])
        if idx != -1 and idx > second_digit[1]:
            second_digit = (d[1], idx)

    #print(first_digit[0] + second_digit[0])
    total_cal += int(first_digit[0] + second_digit[0])
    
print('Total calibration')
print(total_cal)