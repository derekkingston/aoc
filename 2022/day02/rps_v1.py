def score_round(opp_play, my_play):
    rscore = 0
    if my_play == 'X': # ROCK
        rscore = 1
        if opp_play == 'A': #tie
            rscore += 3
        if opp_play == 'B': #loss
            rscore += 0
        if opp_play == 'C': #win
            rscore += 6
    if my_play == 'Y': # PAPER
        rscore = 2
        if opp_play == 'A': #win
            rscore += 6
        if opp_play == 'B': #tie
            rscore += 3
        if opp_play == 'C': #loss
            rscore += 0
    if my_play == 'Z': # SCISSORS
        rscore = 3
        if opp_play == 'A': #loss
            rscore += 0
        if opp_play == 'B': #win
            rscore += 6
        if opp_play == 'C': #tie
            rscore += 3
    return rscore

with open("input.txt") as f:
    lines = [line.rstrip() for line in f]

score = 0
for round in lines:
    [opp, me] = round.split(' ', 1)
    score += score_round(opp, me)

print(score)