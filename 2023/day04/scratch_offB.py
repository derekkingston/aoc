with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

num_cards = []
for e in entries:
    num_cards.append(1)

for c in range(len(entries)):
    [card, otherstuff] = entries[c].split(': ')
    [winners, purchased] = otherstuff.split(" | ")
    win = [int(w) for w in winners.split()]
    pur = [int(p) for p in purchased.split()]
    times_won = 0
    for p in pur:
        if p in win:
            times_won += 1
            num_cards[c+times_won] += num_cards[c]
print(sum(num_cards))