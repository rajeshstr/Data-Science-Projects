import random
from turtle import Turtle
COLOR_PALETTE = ['orange', 'red', 'blue', 'white', 'purple', 'green', 'yellow']


class Food(Turtle):
    def __init__(self):
        super().__init__()
        self.shape('circle')
        self.color(random.choice(COLOR_PALETTE))
        self.resizemode('user')
        self.shapesize(0.5, 0.5, 1)
        self.penup()
        self.refresh()

    def refresh(self):
        self.color(random.choice(COLOR_PALETTE))
        x_cor = random.randint(-270, 270)
        y_cor = random.randint(-270, 270)
        self.goto(x_cor, y_cor)
