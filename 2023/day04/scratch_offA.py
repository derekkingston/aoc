with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

total_winnings = 0
for e in entries:
    [card, otherstuff] = e.split(': ')
    [winners, purchased] = otherstuff.split(" | ")
    win = [int(w) for w in winners.split()]
    pur = [int(p) for p in purchased.split()]
    times_won = 0
    for p in pur:
        if p in win:
            times_won += 1
    if times_won > 0:
        total_winnings += 2**(times_won-1)
print(total_winnings)