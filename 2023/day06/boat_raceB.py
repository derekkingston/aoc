example = (71530, 940200)
actual = (48989083, 390110311121360)
r = actual

combos = 0
for v in range(r[0]):
    if v*(r[0]-v) > r[1]:
        combos += 1
print("Race (" + str(r[0]) + ", " + str(r[1]) + ") has combos: " + str(combos))