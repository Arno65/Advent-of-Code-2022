# Advent of Code 2022 - Day 6 task A & B
# Solutions in Python3
# (Ter leering ende vermaeck...)
#
# Start of message marker for part 1 is at: 1766
# Start of message marker for part 2 is at: 2383
#
# (cl) by Arno Jacobs, 2022-12-06

# check if all characters are unique
# absolutely slow 
# but more that quick enough for here and now
def unique(lst):
    lst.sort()
    # after sort() ~check for consecutive characters
    for ix in range(len(lst)-1):
        if lst[ix] == lst[ix+1]:    
            return False
    return True

# "walk" the list and check for unique sub-list
def messageMarker(wl,msg):
    for ix in range(len(msg)-wl+1):
        ps = msg[ix:(ix+wl)]        # sub-list
        if unique(ps):
            return (ix+wl)
    return (-1)

# # #   From here ~ starting the main
#
# Open file and store in one list of characters - not a string
file = open('data/inputDay06_2022.txt')
message = []    # no string - but list of chars - that will sort()
message[:0] = file.read()

# get the two parts
part1 = messageMarker( 4,message)
part2 = messageMarker(14,message)

# Present the result . . .
print("Advent of Code 2022 - day 6  (Python)")
print("Start of message marker for part 1 is at: %4d" % part1)
print("Start of message marker for part 2 is at: %4d" % part2)
print("0K.\n")
