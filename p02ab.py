# Advent of Code 2022 - Day 2 task A & B
# Solutions in Python3
# (Ter leering ende vermaeck...)
#
# The total score for strategy 1:  9759
# The total score for strategy 2: 12429
#
# (cl) by Arno Jacobs, 2022-12-03


# Open file and convert to a line by line array
guide = open('data/inputDay02_2022.txt')
moves = guide.readlines()

# init scores for both strategies
score1 = 0
score2 = 0

# pattern match the 9 possible moves for both strategies
for move in moves:
    s = move[0:3]
    if s == 'A X': score1 += 4; score2 += 3
    if s == 'A Y': score1 += 8; score2 += 4
    if s == 'A Z': score1 += 3; score2 += 8
    if s == 'B X': score1 += 1; score2 += 1
    if s == 'B Y': score1 += 5; score2 += 5
    if s == 'B Z': score1 += 9; score2 += 9
    if s == 'C X': score1 += 7; score2 += 2
    if s == 'C Y': score1 += 2; score2 += 6
    if s == 'C Z': score1 += 6; score2 += 7

# Present the result . . .
print("Advent of Code 2022 - day 2  (Python)")
print("The total score for strategy 1: %5d" % score1)
print("The total score for strategy 3: %5d" % score2)
print("0K.\n")
