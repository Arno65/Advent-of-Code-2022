# Advent of Code 2022 - Day 1 task A & B
# Solutions in Python3
# (Ter leering ende vermaeck...)
#
# The Elf carrying the most Calories,         has  a total of:  70116
# The three Elves carrying the most Calories, have a total of: 206582
#
# (cl) by Arno Jacobs, 2022-12-06

# Open file and convert to a line by line string array
guide = open('data/inputDay01_2022.txt')
calories = guide.readlines()

# Sub-sum the calories
cl  = []
sub = 0
for cs in calories:
    if cs == "\n":
        cl.append(sub)
        sub = 0
    else:
        sub += int(cs)  # need to convert String to Int

# sort and reverse the list 
cl.sort()
cl.reverse()
# Calories are now ordered from maximum to minimum

# init scores for both strategies
maxCalories1 = cl[0]
maxCalories2 = sum(cl[0:3])

# Present the result . . .
print("Advent of Code 2022 - day 1  (Python)")
print("The Elf carrying the most Calories,         has  a total of: %6d" % maxCalories1)
print("The three Elves carrying the most Calories, have a total of: %6d" % maxCalories2)
print("0K.\n")
