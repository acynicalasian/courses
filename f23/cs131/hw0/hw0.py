# Problem 2a

class Point:
    def __init__(self, x, y):
        self.x_ = x
        self.y_ = y

    def display(self):
        print("(", self.x_, ", ", self.y_, ")")

def modify(p):
    p.x_ = 50
    p = Point(20, 30)
