examples = [
    'mjqjpqmgbljsphdztnvjfqwrcgsmlb',
    'bvwbjplbgvbhsrlpgdmjqwftvncz',
    'nppdvjthqldpwncqszvftbrmjlhg',
    'nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg',
    'zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw'
]

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

for e in entries:
    examples.append(e)

for e in examples:
    c = 13
    while len(e) - c > 0:
        s = set()
        for i in range(14):
            s.add(e[c-i])
        if len(s) == 14:
            print(str(c+1) + " for " + e)
            break
        c += 1