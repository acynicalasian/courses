##############
# Homework 1 #
##############

##############
# Question 1 #
##############

"""Each tree has terminal and non-terminal nodes. For each depth level in the tree, append terminal nodes to an
output list. Then, add non-terminal nodes to a new queue. Repeat this process for each depth level until we have no
more non-terminal nodes to iterate through.

Arguments:

    (tuple) FRINGE
        - Tuple representing an input tree.
"""
def BFS(FRINGE):
    nt_queue = []
    outlist = []

    for node in FRINGE:
        if isinstance(node, tuple):
            nt_queue.append(node)
        else:
            outlist.append(node)
    while nt_queue:
        new_nt_queue = []
        for node in nt_queue:
            for child in node:
                if isinstance(child, tuple):
                    new_nt_queue.append(child)
                else:
                    outlist.append(child)
        nt_queue = new_nt_queue

    return tuple(outlist)

def q1test():
    assert BFS(("ROOT",)) == ("ROOT",)
    assert BFS((((('L', 'E'), 'F'), 'T'))) == ('T', 'F', 'L', 'E')
    assert BFS((("R", ("I", ("G", ("H", "T")))))) == ('R', 'I', 'G', 'H', 'T')
    assert BFS(((("A", ("B",)), "C", ("D",)))) == ('C', 'A', 'D', 'B')
    assert BFS((("T", ("H", "R", "E"), "E"))) == ('T', 'E', 'H', 'R', 'E')
    assert BFS((("A", (("C", (("E",), "D")), "B")))) == ('A', 'B', 'C', 'D', 'E')
    assert BFS(('P', ('G', ('E', (('S',)), 'R'), 'G'), 'O')) == ('P', 'O', 'G', 'G', 'E', 'R', 'S')
                
##############
# Question 2 #
##############

# These functions implement a depth-first solver for the homer-baby-dog-poison
# problem. In this implementation, a state is represented by a single tuple
# (homer, baby, dog, poison), where each variable is True if the respective entity is
# on the west side of the river, and False if it is on the east side.
# Thus, the initial state for this problem is (False False False False) (everybody
# is on the east side) and the goal state is (True True True True).

# The main entry point for this solver is the function DFS, which is called
# with (a) the state to search from and (b) the path to this state. It returns
# the complete path from the initial state to the goal state: this path is a
# list of intermediate problem states. The first element of the path is the
# initial state and the last element is the goal state. Each intermediate state
# is the state that results from applying the appropriate operator to the
# preceding state. If there is no solution, DFS returns [].
# To call DFS to solve the original problem, one would call
# DFS((False, False, False, False), [])
# However, it should be possible to call DFS with a different initial
# state or with an initial path.

# First, we define the helper functions of DFS.

# FINAL-STATE takes a single argument S, the current state, and returns True if it
# is the goal state (True, True, True, True) and False otherwise.
def FINAL_STATE(S):
    return S == (True, True, True, True)

# NEXT-STATE returns the state that results from applying an operator to the
# current state. It takes three arguments: the current state (S), and which entity
###
### Potential error in hw-skeleton: two arguments, not three??
###
# to move (A, equal to "h" for homer only, "b" for homer with baby, "d" for homer
# with dog, and "p" for homer with poison).
# It returns a list containing the state that results from that move.
# If applying this operator results in an invalid state (because the dog and baby,
# or poisoin and baby are left unsupervised on one side of the river), or when the
# action is impossible (homer is not on the same side as the entity) it returns None.
# NOTE that next-state returns a list containing the successor state (which is
# itself a tuple)# the return should look something like [(False, False, True, True)].
def NEXT_STATE(S, A):
    HOMER = 0
    BABY = 1
    DOG = 2
    POISON = 3

    S = list(S)

    # Move first, and then check if anyone dies to avoid typing the check a bunch
    # Move Homer only
    if A == 'h':
        S[HOMER] = not S[HOMER]
    # Move Homer with baby
    elif A == 'b':
        if S[HOMER] == S[BABY]:
            S[HOMER] = not S[HOMER]
            S[BABY] = not S[BABY]
        else:
            return None
    # Move Homer with dog
    elif A == 'd':
        if S[HOMER] == S[DOG]:
            S[HOMER] = not S[HOMER]
            S[DOG] = not S[DOG]
        else:
            return None
    # Move Homer with poison
    else:
        if S[HOMER] == S[POISON]:
            S[HOMER] = not S[HOMER]
            S[POISON] = not S[POISON]
        else:
            return None
    # Check kill conditions
    if (
        # Baby alone with dog
        (S[BABY] == S[DOG] and not S[BABY] == S[HOMER]) or
        # Baby alone with poison
        (S[BABY] == S[POISON] and not S[BABY] == S[HOMER])):
        return None
    else:
        return [tuple(S)]

def q2nexttest():
    init_state = (False, False, False, False)
    assert NEXT_STATE(init_state, 'h') == None
    assert NEXT_STATE(init_state, 'p') == None
    assert NEXT_STATE((True, True, False, False), 'b') == [init_state]
    assert NEXT_STATE((False, False, False, True), 'd') == [(True, False, True, True)]
    test_output = NEXT_STATE((False, True, True, False), 'p')
    assert FINAL_STATE(test_output[0])
    assert len(test_output) == 1
    assert isinstance(test_output, list)
    assert isinstance(test_output[0], tuple)
        


# SUCC-FN returns all of the possible legal successor states to the current
# state. It takes a single argument (s), which encodes the current state, and
# returns a list of each state that can be reached by applying legal operators
# to the current state.
def SUCC_FN(S):
    outlist = []
    apply_h = NEXT_STATE(S, 'h')
    if apply_h:
        outlist.append(apply_h[0])
    apply_b = NEXT_STATE(S, 'b')
    if apply_b:
        outlist.append(apply_b[0])
    apply_d = NEXT_STATE(S, 'd')
    if apply_d:
        outlist.append(apply_d[0])
    apply_p = NEXT_STATE(S, 'p')
    if apply_p:
        outlist.append(apply_p[0])
    return outlist


# ON-PATH checks whether the current state is on the stack of states visited by
# this depth-first search. It takes two arguments: the current state (S) and the
# stack of states visited by DFS (STATES). It returns True if s is a member of
# states and False otherwise.
def ON_PATH(S, STATES):
    return S in STATES


# MULT-DFS is a helper function for DFS. It takes two arguments: a list of
# states from the initial state to the current state (PATH), and the legal
# successor states to the last, current state in the PATH (STATES). PATH is a
# first-in first-out list of states# that is, the first element is the initial
# state for the current search and the last element is the most recent state
# explored. MULT-DFS does a depth-first search on each element of STATES in
# turn. If any of those searches reaches the final state, MULT-DFS returns the
# complete path from the initial state to the goal state. Otherwise, it returns
# [].
def MULT_DFS(STATES, PATH):
    for succ_state in STATES:
        new_path = PATH
        new_path.append(succ_state)
        out = DFS(succ_state, new_path)
        if out:
            return out
    return []            

# DFS does a depth first search from a given state to the goal state. It
# takes two arguments: a state (S) and the path from the initial state to S
# (PATH). If S is the initial state in our search, PATH is set to False. DFS
# performs a depth-first search starting at the given state. It returns the path
# from the initial state to the goal state, if any, or False otherwise. DFS is
# responsible for checking if S is already the goal state, as well as for
# ensuring that the depth-first search does not revisit a node already on the
# search path.
def DFS(S, PATH):
    if PATH == False:
        PATH = [S]
    if FINAL_STATE(S):
        return PATH
    next_poss_states = SUCC_FN(S)
    next_states = []
    if next_poss_states:
        for s in next_poss_states:
            if not ON_PATH(s, PATH):
                next_states.append(s)
        if next_states:
            out = MULT_DFS(next_states, PATH)
            return out if out else False
        else:
            return False
    else:
        return False
