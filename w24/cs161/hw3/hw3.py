##############
# Homework 3 #
##############

# Exercise: Fill this function.
# Returns the index of the variable that corresponds to the fact that
# "Node n gets Color c" when there are k possible colors

# Personal note:
# I think this seems to set up a structure where given n nodes and k colors, if you look in some var
# table, it'll be like:
# vtable[node2var] = (node# * total#colors + color#)
# So given 2 nodes (so far) and 3 possible colors; node 1 can be color 1 or 2 but not 3, node 2 can
# be 2 but not 1 or 3
# vtable = [1, 2, -3, -4, 5, -6]
# So basically, value at index i tells you if node n can be color c
# i = node2var
# (node#, color, yesorno) = (i//k + 1, i%k + 1, vtable[i] > 0)
def node2var(n, c, k):
    return (n-1)*k + c

# Exercise: Fill this function
# Returns *a clause* for the constraint:
# "Node n gets at least one color from the set {1, 2, ..., k}"
def at_least_one_color(n, k):
    # ORs go in 1D arrays
    # At least 1 color is (node_n_color_1 or node_n_color_2 or ...)
    clause = []
    for c in range(1, k + 1):
        clause.append(node2var(n,c,k))
    return clause

# Exercise: Fill this function
# Returns *a list of clauses* for the constraint:
# "Node n gets at most one color from the set {1, 2, ..., k}"
def at_most_one_color(n, k):
    # Given the string of format [ node_n_color1, node_n_color2, ...], the only strings that are
    # true are 10000, 01000, 00100, etc.
    # We can systematically make a CNF formula by the conjunction of clauses based on values that
    # are false; add clauses to the formula that always return false for a false value
    trues = [0]
    for exp in range(k):
        trues.append(2**exp)
    output = []
    for i in range(2**k):
        if i in trues:
            continue
        bitstr = (bin(i+2**k))[3:]
        assert len(bitstr) == k
        clause = []
        for c in range(1, k + 1):
            literal = node2var(n,c,k)
            if bitstr[c-1] == '0':
                clause.append(literal)
            else:
                clause.append(-literal)
        output.append(clause)
    return output

# Exercise: Fill this function
# Returns *a list of clauses* for the constraint:
# "Node n gets exactly one color from the set {1, 2, ..., k}"
def generate_node_clauses(n, k):
    # One color? This means >=1 and <= 1
    return at_most_one_color(n,k) + [at_least_one_color(n,k)]

# Exercise: Fill this function
# Returns *a list of clauses* for the constraint:
# "Nodes connected by an edge e (represented by a list)
# cannot have the same color"
def generate_edge_clauses(e, k):
    # Given the bitstring [node_x_color1, node_x_color2, ..., node_y_color1, node_y_color2, ...],
    # we can check whether bitstr[i] AND bitstr[i+k]. If this is true, the nodes have the same color
    # so it is a false value; add the disjunctive clause that always returns false for that value
    x,y = e
    output = []
    for i in range(2**(2*k)):
        # Generate a bitstring and check whether it would cause nodes x and y to have matching color
        # values. If so, add the disjunctive clause that always returns false for that value.
        bitstr = (bin(i+2**(2*k)))[3:]
        assert len(bitstr) == 2*k
        clause = []
        found_false_value = False
        for c in range(1, k + 1):
            if bitstr[c - 1] == bitstr[c - 1 + k] and bitstr[c - 1] == '1':
                found_false_value = True
        if not found_false_value:
            continue    # No need to calculate the always-false clause for a true value
        found_false_value = False
        for c in range(1,k+1):
            xliteral = node2var(x,c,k)
            yliteral = node2var(y,c,k)
            if bitstr[c-1] == '0':
                clause.append(xliteral)
            else:
                clause.append(-xliteral)
            if bitstr[c -1 + k] == '0':
                clause.append(yliteral)
            else:
                clause.append(-yliteral)
        output.append(clause)
    return output                
            
# The function below converts a graph coloring problem to SAT
# DO NOT MODIFY
def graph_coloring_to_sat(graph_fl, sat_fl, k):
    clauses = []
    with open(graph_fl) as graph_fp:
        node_count, edge_count = tuple(map(int, graph_fp.readline().split()))
        for n in range(1, node_count + 1):
            clauses += generate_node_clauses(n, k)
        for _ in range(edge_count):
            e = tuple(map(int, graph_fp.readline().split()))
            clauses += generate_edge_clauses(e, k)
    var_count = node_count * k
    clause_count = len(clauses)
    with open(sat_fl, 'w') as sat_fp:
        sat_fp.write("p cnf %d %d\n" % (var_count, clause_count))
        for clause in clauses:
            sat_fp.write(" ".join(map(str, clause)) + " 0\n")


# Example function call
if __name__ == "__main__":
   graph_coloring_to_sat("graph1.txt", "graph1_3colors.txt", 3)
   # Custom ones
   graph_coloring_to_sat("graph1.txt", "graph1_4clr", 4)
   graph_coloring_to_sat("test_graph", "test_graph_3clr", 3)
