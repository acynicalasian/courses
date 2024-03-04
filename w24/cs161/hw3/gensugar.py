#!/usr/bin/python3

print("; graph coloring")
num_nodes = 4
num_colors = 3
for n in range(1, num_nodes + 1):
    for c in range(1, num_colors + 1):
        print("(bool b_", n, "_", c, ")")
for n in range(1, num_nodes + 1):
    printtarget = "(alldifferent "
    for c in range(1, num_colors + 1):
        printtarget += "b_" + n + "_" + c
        if c < num_colors:
            printtarget += " "
    printtarget += ")\n"
print(printtarget)
for edge in [(1,2), (2,3), (3,4), (1,4)]:
    printtarget = "(xor "
    a,b = edge
    for c in range(1, num_colors + 1):
        return
    
