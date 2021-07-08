from turtle import Turtle, Screen


class Ball(Turtle):

    def __init__(self):
        super().__init__()
        self.shape('circle')
        self.color('white')
        self.penup()
        self.xmove = 1
        self.ymove = 1
        self.move_speed = 0.005

    def move(self):
        new_x = self.xcor() + self.xmove
        new_y = self.ycor() + self.ymove
        self.goto(new_x, new_y)

    def bounce(self):
        self.ymove *= -1

    def reflect(self):
        self.xmove *= -1
        self.move_speed /= 1.25

    def reset(self):
        self.goto(0, 0)
        self.reflect()
        self.move_speed = 0.005
