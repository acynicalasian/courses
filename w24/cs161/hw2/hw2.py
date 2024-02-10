##############
# Homework 2 #
##############


###################
# Read This First #
###################


# All functions that you need to modify are marked with 'EXERCISE' in their header comments.
# Do not modify astar.py
# This file also contains many helper functions. You may call any of them in your functions.


# Due to the memory limitation, the A* algorithm may crash on some hard sokoban problems if too many
# nodes are generated. Improving the quality of the heuristic will mitigate
# this problem, as it will allow A* to solve hard problems with fewer node expansions.


# Remember that most functions are not graded on efficiency (only correctness).
# Efficiency can only influence your heuristic performance in the competition (which will affect your score).


# Load the astar.py and do not modify it.
import astar
# Load the numpy package and the state is represented as a numpy array during this homework.
import numpy as np


# a_star perform the A* algorithm with the start_state (numpy array), goal_test (function), successors (function) and
# heuristic (function). a_star prints the solution from start_state to goal_state (path), calculates the number of
# generated nodes (node_generated) and expanded nodes (node_expanded), and the solution depth (len(path)-1). a_star
# also provides the following functions for printing states and moves: prettyMoves(path): Translate the solution to a
# list of moves printlists(path): Visualize the solution and Print a list of states
def a_star(start_state, goal_test, successors, heuristic):
    goal_node, node_generated, node_expanded = astar.a_star_search(start_state, goal_test, successors, heuristic)
    if goal_node:
        node = goal_node
        path = [node.state1]
        while node.parent:
            node = node.parent
            path.append(node.state1)
        path.reverse()

        # print('My path:{}'.format(path))
        # print(prettyMoves(path))
        # printlists(path)
        print('Nodes Generated by A*: {}'.format(node_generated))
        print('Nodes Expanded by A*: {}'.format(node_expanded))
        print('Solution Depth: {}'.format(len(path) - 1))
    else:
        print('no solution found')


# A shortcut function
# Transform the input state to numpy array. For other functions, the state s is presented as a numpy array.
# Goal-test and next-states stay the same throughout the assignment
# You can just call sokoban(init-state, heuristic function) to test the result
def sokoban(s, h):
    return a_star(np.array(s), goal_test, next_states, h)


# Define some global variables
blank = 0
wall = 1
box = 2
keeper = 3
star = 4
boxstar = 5
keeperstar = 6


# Some helper functions for checking the content of a square
def isBlank(v):
    return (v == blank)


def isWall(v):
    return (v == wall)


def isBox(v):
    return (v == box)


def isKeeper(v):
    return (v == keeper)


def isStar(v):
    return (v == star)


def isBoxstar(v):
    return (v == boxstar)


def isKeeperstar(v):
    return (v == keeperstar)


# Help function for get KeeperPosition
# Given state s (numpy array), return the position of the keeper by row, col
# The top row is the zeroth row
# The first (right) column is the zeroth column
def getKeeperPosition(s):
    row = s.shape[0]
    col = s.shape[1]
    for i in range(row):
        for j in range(col):
            if (isKeeper(s[i, j]) or isKeeperstar(s[i, j])):
                return i, j


# For input list s_list, remove all None element
# For example, if s_list = [1, 2, None, 3], returns [1, 2, 3]
def cleanUpList(s_list):
    clean = []
    for state in s_list:
        if state is not None:
            clean.append(state)
    return clean


# EXERCISE: Modify this function to return Ture
# if and only if s (numpy array) is a goal state of a Sokoban game.
# (no box is on a non-goal square)
# Remember, the number of goal can be larger than the number of box.
# Currently, it always returns False. If A* is called with
# this function as the goal testing function, A* will never
# terminate until the whole search space is exhausted.
def goal_test(s):
    # If we have a box, we either haven't solved/failed yet, or the problem isn't
    # solvable because we have more boxes than goals
    row = s.shape[0]
    col = s.shape[1]
    for i in range(row):
        for j in range(col):
            if isBox(s[i,j]):
                return False
    return True



# EXERCISE: Modify this function to return the list of
# successor states of s (numpy array).
#
# This is the top-level next-states (successor) function.
# Some skeleton code is provided below.
# You may delete them totally, depending on your approach.
# 
# If you want to use it, you will need to set 'result' to be 
# the set of states after moving the keeper in each of the 4 directions.
#
# You can define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
# Any None result in the list can be removed by cleanUpList.
#
# When generated the successors states, you may need to copy the current state s (numpy array).
# A shallow copy (e.g, direcly set s1 = s) constructs a new compound object and then inserts references 
# into it to the objects found in the original. In this case, any change in the numpy array s1 will also affect
# the original array s. Thus, you may need a deep copy (e.g, s1 = np.copy(s)) to construct an indepedent array.
def next_states(s):
    row, col = getKeeperPosition(s)
    s_list = []

    ### Edited: commented this out for now to only create array copies if the conditions are right
    #s1 = np.copy(s)

    # NOT IMPLEMENTED YET! YOU NEED TO FINISH THIS FUNCTION.

    ### Beginning of my implementation:
    height, width = np.shape(s)

    # check UP DOWN LEFT RIGHT
    adjacent_coords = [(row-1,col), (row+1,col), (row,col-1), (row,col+1)]
    push_coords = [(row-2,col), (row+2,col), (row,col-2), (row,col+2)]
    for i in range(4):
        (adj_y, adj_x) = adjacent_coords[i]
        # Skip if adj_y or adj_x coordinates are invalid
        if adj_y < 0 or adj_y >= height or adj_x < 0 or adj_x >= width:
            continue
        # Walkable? So direction we walk is either blank or goal
        if isBlank(s[adj_y, adj_x]) or isStar(s[adj_y, adj_x]):
            s_copy = np.copy(s)
            # Are we stepping off a goal or a blank?
            if isKeeper(s_copy[row, col]):
                s_copy[row, col] = blank
            elif isKeeperstar(s_copy[row, col]):
                s_copy[row, col] = star
            else: # this shouldn't happen, we have a logic error
                assert False, "logic error"
            # Are we stepping onto a blank or a goal?
            if isBlank(s_copy[adj_y, adj_x]):
                s_copy[adj_y, adj_x] = keeper
            elif isStar(s_copy[adj_y, adj_x]):
                s_copy[adj_y, adj_x] = keeperstar
            else:
                assert False, "logic error"
            s_list.append(s_copy)
        # We walk into a wall
        elif isWall(s[adj_y, adj_x]):
            continue
        # We walk into a box
        elif isBox(s[adj_y, adj_x]) or isBoxstar(s[adj_y, adj_x]):
            # Are we pushing onto a blank, goal, or box/wall?
            (push_y, push_x) = push_coords[i]
            # Skip if push coordinates are invalid
            if push_y < 0 or push_y >= height or push_x < 0 or push_x >= width:
                continue
            if isBlank(s[push_y, push_x]) or isStar(s[push_y, push_x]):
                # Did we push a box into a blank space where a corner is? If so, we have no way to solve
                # so don't generate new states
                if isBlank(s[push_y, push_x]) and push_y > 0 and isWall(s[push_y - 1, push_x]): # check up
                    if push_x > 0 and isWall(s[push_y, push_x - 1]): # check left
                        continue
                    if push_x < width - 1 and isWall(s[push_y, push_x + 1]): # check right
                        continue
                if isBlank(s[push_y, push_x]) and push_y < height - 1 and isWall(s[push_y + 1, push_x]): # check down
                    if push_x > 0 and isWall(s[push_y, push_x - 1]): # check left
                        continue
                    if push_x < width - 1 and isWall(s[push_y, push_x + 1]): # check right
                        continue
                # Only bother to copy if we're pushing into a pushable area
                s_copy = np.copy(s)
                # Are we stepping off a blank or a goal?
                s_copy[row, col] = blank if isKeeper(s[row, col]) else star
                # Are we pushing a box off and stepping onto a blank or a goal?
                s_copy[adj_y, adj_x] = keeper if isBox(s[adj_y, adj_x]) else keeperstar
                # Are we pushing a box onto a blank or a goal?
                s_copy[push_y, push_x] = box if isBlank(s[push_y, push_x]) else boxstar
                s_list.append(s_copy)
            elif isWall(s[push_y, push_x]) or isBox(s[push_y, push_x]) or isBoxstar(s[push_y, push_x]):
                continue
            else:
                assert False, "logic error"
        else:
            assert False, "we walked into another keeper? tf"
    
    return s_list
    ### End of my implementation

    ### Edited: commented this out for now since I *think* my implementation doesn't add Nones
    # return cleanUpList(s_list)


# EXERCISE: Modify this function to compute the trivial
# admissible heuristic.
def h0(s):
    return 0


# EXERCISE: Modify this function to compute the
# number of misplaced boxes in state s (numpy array).
#
# My answer to HW2 problem:
# -----------------------------
#
# This heuristic is admissible because the lowest possible number of steps to a game with n misplaced boxes is n steps
def h1(s):
    count = 0
    (row, col) = np.shape(s)
    for i in range(row):
        for j in range(col):
            if isBox(s[i,j]):
                count += 1
    return count
    
    


# EXERCISE: Change the name of this function to h<UID> where
# <UID> is your actual student ID number. Then, modify this 
# function to compute an admissible heuristic value of s. 
# For example, if my UID is 123456789, then I should change the function name to 'h123456789'
# This function will be tested in various hard examples.
# Objective: make A* solve problems as fast as possible.

def h004919548(s):
    #return h_greedy(s)
    return h_manhattan(s)

def h_greedy(s):
    """
    We use a greedy approach for this heuristic. Choose a box and corresponding goal that gives the lowest cost 
    for moving just that box. For example, if there's one box that's 1m away, and the closest goal to that box is 
    4m away, and there's a second box that's 2m away but the closest goal to that box is 2m away from it, moving 
    the second box to the goal that's 2m away from it would be the greedy choice.

    This heuristic function is admissible for a number of reasons. For n = 1 box, this approach solves the 
    problem trivially, but for greater than that, note the following:

    If the greedy choice is optimal, then the total cost of moving every box is always higher than the cost of 
    moving the first box greedily, especially since you cannot move more than one box at a time. If the greedy 
    choice would not be optimal, that implies that the moving the first box wherever has a higher cost than 
    moving the box we chose with the greedy method.

    Finally, this heuristic function relies on straight-line distances, and due to the geometry/constraints of 
    the problem, straight-line distances are always at least the distance/steps needed in Sokoban.

    Every time we calculate this heuristic, we search the entire field for misplaced boxes in the current state.
    Assume that's size s^2. Then, for n misplaced boxes and >= n goals in a solvable puzzle, we calculate the 
    Euclidean distance to the nearest box, and then we search for the nearest goal. This gives n^2. So the
    overall time complexity is roughly O(s^2).
    """
    (keeper_y, keeper_x) = getKeeperPosition(s)
    (height, width) = np.shape(s)
    box_coords = []
    goal_coords = []
    for i in range(height):
        for j in range(width):
            if isBox(s[i,j]):
                box_coords.append((i,j))
            if isStar(s[i,j]):
                goal_coords.append((i,j))
    minval = None
    # No more boxes? Return 0
    # Not sure why we need this second condition but was failing otherwise
    # But I did have a bug in state generator which may be why I needed it
    if not box_coords or not goal_coords:
        return 0
    for (box_y, box_x) in box_coords:
        # Find the nearest box
        a2_box = (box_y - keeper_y) * (box_y - keeper_y)
        b2_box = (box_x - keeper_x) * (box_x - keeper_x)
        c_box = np.sqrt(a2_box + b2_box)
        for (goal_y, goal_x) in goal_coords:
            # Find the nearest goal to the nearest box
            a2_goal = (goal_y - box_y) * (goal_y - box_y)
            b2_goal = (goal_x - box_x) * (goal_x - box_x)
            c_goal = np.sqrt(a2_goal + b2_goal)
            hestimate = c_box + c_goal
            if minval is None:
                minval = hestimate
            elif hestimate < minval:
                minval = hestimate
    assert minval is not None, "Logic error in heuristic fn"
    return int(minval)

def h_manhattan(s):
    """
    Same principles as before. Let's try measuring both a box distance and the goal distance with the 
    manhattan distance. O(s^2) complexity.
    """
    (keeper_y, keeper_x) = getKeeperPosition(s)
    (height, width) = np.shape(s)
    box_coords = []
    goal_coords = []
    for i in range(height):
        for j in range(width):
            if isBox(s[i,j]):
                box_coords.append((i,j))
            if isStar(s[i,j]):
                goal_coords.append((i,j))
    minval = None
    if not box_coords or not goal_coords:
        return 0
    for (box_y, box_x) in box_coords:
        c_box = abs(box_y - keeper_y) + abs(box_x - keeper_x)
        for (goal_y, goal_x) in goal_coords:
            c_goal = abs(goal_y - box_y) + abs(goal_x - box_x)
            hestimate = c_box + c_goal
            if minval is None:
                minval = hestimate
            elif hestimate < minval:
                minval = hestimate
    assert minval is not None, "Logic error in manhattan fn"
    return int(minval)

def solve_from_cli(puzzle):
    currwidth = 0
    maxwidth = 0
    parsed_puzzle = []
    lineaccum = []
    for i in range(len(puzzle)):
        if not puzzle[i] == '\n':
            currwidth += 1
        if puzzle[i] == '#':
            lineaccum.append(wall)
        elif puzzle[i] == '@':
            lineaccum.append(keeper)
        elif puzzle[i] == '+':
            lineaccum.append(keeperstar)
        elif puzzle[i] == '$':
            lineaccum.append(box)
        elif puzzle[i] == '*':
            lineaccum.append(boxstar)
        elif puzzle[i] == '.':
            lineaccum.append(star)
        elif puzzle[i] == ' ':
            lineaccum.append(blank)
        elif puzzle[i] == '\n':
            if currwidth > maxwidth:
                maxwidth = currwidth
            currwidth = 0
            parsed_puzzle.append(lineaccum)
            lineaccum = []
        else:
            assert False, "unexpected input!"
    if lineaccum:
        parsed_puzzle.append(lineaccum)
    # Pad lines so they line up
    padded_puzzle = []
    for line in parsed_puzzle:
        if len(line) < maxwidth:
            new_line = line + [blank for i in range(maxwidth - len(line))]
            padded_puzzle.append(new_line)
        else:
            padded_puzzle.append(line)
    # Sanity check
    for line in padded_puzzle:
        assert len(line) == maxwidth, "logic error!"
        assert isinstance(line, list), "logic error!"
        assert isinstance(line[0], int)
        
    import time
    start_time = time.time()
    sokoban(padded_puzzle, h0)
    print("h0: ", str(time.time() - start_time), '\n')
    start_time = time.time()
    sokoban(padded_puzzle, h1)
    print("h1: ", str(time.time() - start_time), '\n')
    start_time = time.time()
    sokoban(padded_puzzle, h_manhattan)
    print("h_manhattan: ", str(time.time() - start_time), '\n')
    
            
        
            

# Some predefined problems with initial state s (array). Sokoban function will automatically transform it to numpy
# array. For other function, the state s is presented as a numpy array. You can just call sokoban(init-state,
# heuristic function) to test the result Each problem can be visualized by calling prettyMoves(path) and printlists(
# path) in a_star function
#
# Problems are roughly ordered by their difficulties.
# For most problems, we also provide 2 additional number per problem:
#    1) # of nodes expanded by A* using our next-states and h0 heuristic.
#    2) the depth of the optimal solution.
# These numbers are located at the comments of the problems. For example, the first problem below 
# was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
# 
# Your implementation may not result in the same number of nodes expanded, but it should probably
# give something in the same ballpark. As for the solution depth, any admissible heuristic must 
# make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
# for checking whether your heuristic is admissible.
#
# Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!


# [80,7]
s1 = [[1, 1, 1, 1, 1, 1],
      [1, 0, 3, 0, 0, 1],
      [1, 0, 2, 0, 0, 1],
      [1, 1, 0, 1, 1, 1],
      [1, 0, 0, 0, 0, 1],
      [1, 0, 0, 0, 4, 1],
      [1, 1, 1, 1, 1, 1]]

# [110,10],
s2 = [[1, 1, 1, 1, 1, 1, 1],
      [1, 0, 0, 0, 0, 0, 1],
      [1, 0, 0, 0, 0, 0, 1],
      [1, 0, 0, 2, 1, 4, 1],
      [1, 3, 0, 0, 1, 0, 1],
      [1, 1, 1, 1, 1, 1, 1]]

# [211,12],
s3 = [[1, 1, 1, 1, 1, 1, 1, 1, 1],
      [1, 0, 0, 0, 1, 0, 0, 0, 1],
      [1, 0, 0, 0, 2, 0, 3, 4, 1],
      [1, 0, 0, 0, 1, 0, 0, 0, 1],
      [1, 0, 0, 0, 1, 0, 0, 0, 1],
      [1, 1, 1, 1, 1, 1, 1, 1, 1]]

# [300,13],
s4 = [[1, 1, 1, 1, 1, 1, 1],
      [0, 0, 0, 0, 0, 1, 4],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 1, 1, 1, 0, 0],
      [0, 0, 1, 0, 0, 0, 0],
      [0, 2, 1, 0, 0, 0, 0],
      [0, 3, 1, 0, 0, 0, 0]]

# [551,10],
s5 = [[1, 1, 1, 1, 1, 1],
      [1, 1, 0, 0, 1, 1],
      [1, 0, 0, 0, 0, 1],
      [1, 4, 2, 2, 4, 1],
      [1, 0, 0, 0, 0, 1],
      [1, 1, 3, 1, 1, 1],
      [1, 1, 1, 1, 1, 1]]

# [722,12],
s6 = [[1, 1, 1, 1, 1, 1, 1, 1],
      [1, 0, 0, 0, 0, 0, 4, 1],
      [1, 0, 0, 0, 2, 2, 3, 1],
      [1, 0, 0, 1, 0, 0, 4, 1],
      [1, 1, 1, 1, 1, 1, 1, 1]]

# [1738,50],
s7 = [[1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
      [0, 0, 1, 1, 1, 1, 0, 0, 0, 3],
      [0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 1, 0, 0, 1, 0],
      [0, 0, 1, 0, 0, 1, 0, 0, 1, 0],
      [0, 2, 1, 0, 0, 0, 0, 0, 1, 0],
      [0, 0, 1, 0, 0, 0, 0, 0, 1, 4]]

# [1763,22],
s8 = [[1, 1, 1, 1, 1, 1],
      [1, 4, 0, 0, 4, 1],
      [1, 0, 2, 2, 0, 1],
      [1, 2, 0, 1, 0, 1],
      [1, 3, 0, 0, 4, 1],
      [1, 1, 1, 1, 1, 1]]

# [1806,41],
s9 = [[1, 1, 1, 1, 1, 1, 1, 1, 1],
      [1, 1, 1, 0, 0, 1, 1, 1, 1],
      [1, 0, 0, 0, 0, 0, 2, 0, 1],
      [1, 0, 1, 0, 0, 1, 2, 0, 1],
      [1, 0, 4, 0, 4, 1, 3, 0, 1],
      [1, 1, 1, 1, 1, 1, 1, 1, 1]]

# [10082,51],
s10 = [[1, 1, 1, 1, 1, 0, 0],
       [1, 0, 0, 0, 1, 1, 0],
       [1, 3, 2, 0, 0, 1, 1],
       [1, 1, 0, 2, 0, 0, 1],
       [0, 1, 1, 0, 2, 0, 1],
       [0, 0, 1, 1, 0, 0, 1],
       [0, 0, 0, 1, 1, 4, 1],
       [0, 0, 0, 0, 1, 4, 1],
       [0, 0, 0, 0, 1, 4, 1],
       [0, 0, 0, 0, 1, 1, 1]]

# [16517,48],
s11 = [[1, 1, 1, 1, 1, 1, 1],
       [1, 4, 0, 0, 0, 4, 1],
       [1, 0, 2, 2, 1, 0, 1],
       [1, 0, 2, 0, 1, 3, 1],
       [1, 1, 2, 0, 1, 0, 1],
       [1, 4, 0, 0, 4, 0, 1],
       [1, 1, 1, 1, 1, 1, 1]]

# [22035,38],
s12 = [[0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0],
       [1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1],
       [1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 1],
       [1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
       [1, 0, 0, 0, 2, 1, 1, 1, 0, 0, 0, 1],
       [1, 0, 0, 0, 0, 1, 0, 1, 4, 0, 4, 1],
       [1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1]]

# [26905,28],
s13 = [[1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
       [1, 4, 0, 0, 0, 0, 0, 2, 0, 1],
       [1, 0, 2, 0, 0, 0, 0, 0, 4, 1],
       [1, 0, 3, 0, 0, 0, 0, 0, 2, 1],
       [1, 0, 0, 0, 0, 0, 0, 0, 0, 1],
       [1, 0, 0, 0, 0, 0, 0, 0, 4, 1],
       [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]]

# [41715,53],
s14 = [[0, 0, 1, 0, 0, 0, 0],
       [0, 2, 1, 4, 0, 0, 0],
       [0, 2, 0, 4, 0, 0, 0],
       [3, 2, 1, 1, 1, 0, 0],
       [0, 0, 1, 4, 0, 0, 0]]

# [48695,44],
s15 = [[1, 1, 1, 1, 1, 1, 1],
       [1, 0, 0, 0, 0, 0, 1],
       [1, 0, 0, 2, 2, 0, 1],
       [1, 0, 2, 0, 2, 3, 1],
       [1, 4, 4, 1, 1, 1, 1],
       [1, 4, 4, 1, 0, 0, 0],
       [1, 1, 1, 1, 0, 0, 0]]

# [91344,111],
s16 = [[1, 1, 1, 1, 1, 0, 0, 0],
       [1, 0, 0, 0, 1, 0, 0, 0],
       [1, 2, 1, 0, 1, 1, 1, 1],
       [1, 4, 0, 0, 0, 0, 0, 1],
       [1, 0, 0, 5, 0, 5, 0, 1],
       [1, 0, 5, 0, 1, 0, 1, 1],
       [1, 1, 1, 0, 3, 0, 1, 0],
       [0, 0, 1, 1, 1, 1, 1, 0]]

# [3301278,76],
# Warning: This problem is very hard and could be impossible to solve without a good heuristic!
s17 = [[1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
       [1, 3, 0, 0, 1, 0, 0, 0, 4, 1],
       [1, 0, 2, 0, 2, 0, 0, 4, 4, 1],
       [1, 0, 2, 2, 2, 1, 1, 4, 4, 1],
       [1, 0, 0, 0, 0, 1, 1, 4, 4, 1],
       [1, 1, 1, 1, 1, 1, 0, 0, 0, 0]]

# [??,25],
s18 = [[0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
       [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
       [1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1],
       [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
       [0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0],
       [0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0],
       [0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0],
       [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
       [1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1],
       [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
       [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
       [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 4, 1, 0, 0, 0, 0],
       [0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 1, 0, 0, 0, 0],
       [0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 4, 1, 0, 0, 0, 0]]

# [??,21],
s19 = [[0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0],
       [0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0],
       [0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0],
       [1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1],
       [0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0],
       [0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 2, 0],
       [0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 4],
       [1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1],
       [0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0],
       [0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0],
       [0, 0, 0, 1, 0, 2, 0, 4, 1, 0, 0, 0]]


# Utility functions for printing states and moves.
# You do not need to understand any of the functions below this point.


# Helper function of prettyMoves
# Detect the move from state s --> s1
def detectDiff(s, s1):
    row, col = getKeeperPosition(s)
    row1, col1 = getKeeperPosition(s1)
    if (row1 == row + 1):
        return 'Down'
    if (row1 == row - 1):
        return 'Up'
    if (col1 == col + 1):
        return 'Right'
    if (col1 == col - 1):
        return 'Left'
    return 'fail'


# Translates a list of states into a list of moves
def prettyMoves(lists):
    initial = 0
    action = []
    for states in (lists):
        if (initial != 0):
            action.append(detectDiff(previous, states))
        initial = 1
        previous = states
    return action


# Print the content of the square to stdout.
def printsquare(v):
    if (v == blank):
        print(' ', end='')
    if (v == wall):
        print('#', end='')
    if (v == box):
        print('$', end='')
    if (v == keeper):
        print('@', end='')
    if (v == star):
        print('.', end='')
    if (v == boxstar):
        print('*', end='')
    if (v == keeperstar):
        print('+', end='')


# Print a state
def printstate(s):
    row = s.shape[0]
    col = s.shape[1]
    for i in range(row):
        for j in range(col):
            printsquare(s[i, j])
        print('\n')


# Print a list of states with delay.
def printlists(lists):
    for states in (lists):
        printstate(states)
        print('\n')


if __name__ == "__main__":
    sokoban(s1, h0)

    sokoban(s2, h0)

    sokoban(s3, h0)

    sokoban(s4, h0)

    
    # import time
    # start_time = time.time()
    # sokoban(s19, h0)
    # print("h0: ", str(time.time() - start_time))
    # start_time = time.time()
    # sokoban(s19, h1)
    # print("h1: ", str(time.time() - start_time))
    # start_time = time.time()
    # sokoban(s19, h_greedy)
    # print("h_greedy: ", str(time.time() - start_time))
    # start_time = time.time()
    # sokoban(s19, h_boxonly_greedy)
    # print("h_boxonly_greedy: ", str(time.time() - start_time))
    # start_time = time.time()
    # sokoban(s19, h_manhattan)
    # print("h_manhattan: ", str(time.time() - start_time))
    # start_time = time.time()
    # sokoban(s19, h_boxonly_manhattan)
    # print("h_boxonly_manhattan: ", str(time.time() - start_time))
    
    
