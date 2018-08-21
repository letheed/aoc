from aoc.common import answer, Answers, Date, Day, Puzzle
from . import YEAR


DATE: Date = Date(Day.D01, YEAR)
PUZZLE: Puzzle = Puzzle(DATE, lambda input: solve(input))


def solve(input: str) -> Answers:
    floor, moves = 0, 0
    visited_basement = False
    for c in input:
        if   c == '(': floor += 1
        elif c == ')': floor -= 1
        else: raise ValueError(str)
        if not visited_basement:
            moves += 1
            if floor == -1: visited_basement = True
    return answer(floor, moves)
