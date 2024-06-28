import sys

network = sys.argv[2]

## File to be adjusted: umist2krome.py
file = 'umist2krome.py'

## Read the file
with open(file, 'r') as f:
    lines = f.readlines()

## Split the lines
lines_split = list()
for i in range(len(lines)):
    line = lines[i].split(' ')
    lines_split.append(line)

## Change the file to the correct UMIST network
for i in range(len(lines_split)):
    if 'fname_umist' in lines_split[i]:
        lines_split[i] = ['fname_umist', '=', "'"+network+"'", "\n"]

## Write the file
with open(file, 'w') as f:
    for i in range(len(lines_split)):
        f.write(' '.join(lines_split[i]))




    