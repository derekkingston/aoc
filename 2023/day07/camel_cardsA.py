strength = {
    'A': 12,
    'K': 11,
    'Q': 10,
    'J': 9,
    'T': 8,
    '9': 7,
    '8': 6,
    '7': 5,
    '6': 4,
    '5': 3,
    '4': 2,
    '3': 1,
    '2': 0
}

handtype = {
    'Five': 7,
    'Four': 6,
    'Full': 5,
    'Three': 4,
    'TwoPair': 3,
    'OnePair': 2,
    'High': 1
}

def compute_type(hand):
    if hand.count(hand[0]) == 5:
        return handtype['Five']
    for key in strength:
        if hand.count(key) == 4:
            return handtype['Four']
        if hand.count(key) == 3:
            for key2 in strength:
                if key2 == key:
                    continue
                if hand.count(key2) == 2:
                    return handtype['Full']
            return handtype['Three']
        if hand.count(key) == 2:
            for key2 in strength:
                if key2 == key:
                    continue
                if hand.count(key2) == 3:
                    return handtype['Full']
                if hand.count(key2) == 2:
                    return handtype['TwoPair']
            return handtype['OnePair']
    return handtype['High']

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

games = []
for e in entries:
    [hand, bid] = e.split(" ")
    games.append( (compute_type(hand),
                   strength[hand[0]],
                   strength[hand[1]],
                   strength[hand[2]],
                   strength[hand[3]],
                   strength[hand[4]], 
                   hand, int(bid)))
    #print("(" + hand + ", " + bid + ") has type: " + str(games[-1][0]))

winnings = 0
rank = len(games)
for g in sorted(games, reverse=True):
    winnings = winnings + rank*g[7]
    rank = rank - 1

print(winnings)