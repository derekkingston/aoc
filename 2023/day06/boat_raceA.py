example = [(7, 9), (15, 40), (30, 200)]
actual = [(48, 390), (98, 1103), (90, 1112), (83, 1360)]

races = actual

tally = 1
for r in races:
    combos = 0
    for v in range(r[0]):
        if v*(r[0]-v) > r[1]:
            combos += 1
    print("Race (" + str(r[0]) + ", " + str(r[1]) + ") has combos: " + str(combos))
    tally = tally * combos;

print("Tally: " + str(tally))