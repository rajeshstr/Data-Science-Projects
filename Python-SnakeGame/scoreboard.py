from turtle import Turtle

ALIGN = 'center'
FONT = ('Cambria', 15, 'normal')


class Scoreboard(Turtle):
    def __init__(self):
        super().__init__()
        self.hideturtle()
        self.score = 0
        self.penup()
        self.goto(0, 280)
        self.color('white')
        self.update_score()

    def update_score(self):
        self.write(f"Score: {self.score}", move=False, align=ALIGN, font=FONT)

    def inc_score(self):
        self.score += 1
        self.clear()
        self.update_score()

    def game_over(self):
        self.goto(0, 0)
        self.write("Game Over :( ", move=False, align=ALIGN, font=FONT)
