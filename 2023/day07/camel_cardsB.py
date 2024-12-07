strength = {
    'A': 12,
    'K': 11,
    'Q': 10,
    'T': 9,
    '9': 8,
    '8': 7,
    '7': 6,
    '6': 5,
    '5': 4,
    '4': 3,
    '3': 2,
    '2': 1,
    'J': 0
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

def compute_normal_type(hand):
    if hand.count(hand[0]) == 5:
        return (handtype['Five'], hand[0], hand[0])
    for key in strength:
        if hand.count(key) == 4:
            return (handtype['Four'], key, hand.replace(key, ''))
        if hand.count(key) == 3:
            for key2 in strength:
                if key2 == key:
                    continue
                if hand.count(key2) == 2:
                    return (handtype['Full'], key, key2)
            return (handtype['Three'], key, hand.replace(key, ''))
        if hand.count(key) == 2:
            for key2 in strength:
                if key2 == key:
                    continue
                if hand.count(key2) == 3:
                    return (handtype['Full'], key2, key)
                if hand.count(key2) == 2:
                    return (handtype['TwoPair'], key, key2)
            return (handtype['OnePair'], key, hand.replace(key, ''))
    return (handtype['High'], hand, hand)

def compute_type(hand):
    (nt, p1, p2) = compute_normal_type(hand)
    if nt == handtype['Five']:
        return nt
    if nt == handtype['Four']:
        if p1 == 'J' or p2 == 'J':
            return handtype['Five']
        return nt
    if nt == handtype['Full']:
        if p1 == 'J' or p2 == 'J':
            return handtype['Five']
        return nt
    if nt == handtype['Three']:
        if p1 == 'J' or p2.find('J') >= 0:
            return handtype['Four']
        return nt
    if nt == handtype['TwoPair']:
        if p1 == 'J' or p2 == 'J':
            return handtype['Four']
        if hand.find('J') >= 0:
            return handtype['Full']
        return nt
    if nt == handtype['OnePair']:
        if hand.find('J') >= 0:
            return handtype['Three']
        return nt
    if nt == handtype['High']:
        if hand.find('J') >= 0:
            return handtype['OnePair']
        return nt
    return nt

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
    print("(" + hand + ", " + bid + ") has type: " + str(games[-1][0]))

winnings = 0
rank = len(games)
for g in sorted(games, reverse=True):
    winnings = winnings + rank*g[7]
    rank = rank - 1

print(winnings)