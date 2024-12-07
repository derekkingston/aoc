def score_round(opp_play, my_play):
    rscore = 0
    if my_play == 'A': # ROCK
        rscore = 1
        if opp_play == 'A': #tie
            rscore += 3
        if opp_play == 'B': #loss
            rscore += 0
        if opp_play == 'C': #win
            rscore += 6
    if my_play == 'B': # PAPER
        rscore = 2
        if opp_play == 'A': #win
            rscore += 6
        if opp_play == 'B': #tie
            rscore += 3
        if opp_play == 'C': #loss
            rscore += 0
    if my_play == 'C': # SCISSORS
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
    [opp, outcome] = round.split(' ', 1)
    me = opp
    if outcome == 'X': # need to lose
        if opp == 'A':
            me = 'C'
        if opp == 'B':
            me = 'A'
        if opp == 'C':
            me = 'B'
    if outcome == 'Z': # need to win
        if opp == 'A':
            me = 'B'
        if opp == 'B':
            me = 'C'
        if opp == 'C':
            me = 'A'
    score += score_round(opp, me)

print(score)