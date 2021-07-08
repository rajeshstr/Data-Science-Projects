from turtle import Turtle

MOVE_DIST = 10
STARTING_POS = [(0, 0), (-10, 0), (-20, 0), (-30, 0), (-40, 0)]


class Snake:
    def __init__(self):
        self.turtle_segments = []
        self.create_snake()
        self.head = self.turtle_segments[0]

    def create_snake(self):
        for i in STARTING_POS:
            self.add_segment(i)

    def add_segment(self, pos):
        new_segment = Turtle()
        new_segment.shapesize(stretch_len=0.5, stretch_wid=0.5)
        new_segment.shape('square')
        new_segment.color('white')
        new_segment.penup()
        new_segment.goto(pos)
        self.turtle_segments.append(new_segment)

    def extend(self):
        self.add_segment(self.turtle_segments[-1].position())
        self.add_segment(self.turtle_segments[-1].position())

    def move(self):
        for seg_num in range(len(self.turtle_segments) - 1, 0, -1):
            new_x = self.turtle_segments[seg_num - 1].xcor()
            new_y = self.turtle_segments[seg_num - 1].ycor()
            self.turtle_segments[seg_num].setheading(self.head.heading())
            self.turtle_segments[seg_num].goto(new_x, new_y)
        self.head.forward(MOVE_DIST)

    def up(self):
        if self.head.heading() != 270:
            self.head.setheading(90)

    def down(self):
        if self.head.heading() != 90:
            self.head.setheading(270)

    def left(self):
        if self.head.heading() != 0:
            self.head.setheading(180)

    def right(self):
        if self.head.heading() != 180:
            self.head.setheading(0)
