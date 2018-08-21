from aoc.common import answer, Answers, Date, Day, Puzzle
from . import YEAR


DATE: Date = Date(Day.D01, YEAR)
PUZZLE: Puzzle = Puzzle(DATE, lambda input: solve(input))


def solve(input: str) -> Answers:
    return answer(None, None)
