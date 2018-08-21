from __future__ import annotations
from typing import Any, Callable, Optional, Tuple
from dataclasses import dataclass
from enum import Enum, unique
import subprocess
import time


ROOT_DIR: str = subprocess.run(  # type: ignore # FIXME mypy#1279
    ['git', 'rev-parse', '--show-toplevel'],
    capture_output=True, check=True, text=True
).stdout.rstrip()

PUZZLE_DIR: str = ROOT_DIR + '/puzzles'


Answer = str
Answers = Tuple[Answer, Answer]
Solver = Callable[[str], Answers]


@unique
class Day(Enum):
    D01: int = 1
    D02: int = 2
    D03: int = 3
    D04: int = 4
    D05: int = 5
    D06: int = 6
    D07: int = 7
    D08: int = 8
    D09: int = 9
    D10: int = 10
    D11: int = 11
    D12: int = 12
    D13: int = 13
    D14: int = 14
    D15: int = 15
    D16: int = 16
    D17: int = 17
    D18: int = 18
    D19: int = 19
    D20: int = 20
    D21: int = 21
    D22: int = 22
    D23: int = 23
    D24: int = 24
    D25: int = 25

    def __str__(self) -> str:
        return f'{self.value:02}'


@unique
class Year(Enum):
    Y2015: int = 2015
    Y2016: int = 2016
    Y2017: int = 2017

    def __str__(self) -> str:
        return str(self.value)


@dataclass(frozen=True)
class Date:
    day: Day
    year: Year

    def __str__(self) -> str:
        return f'{self.year}-{self.day}'


@dataclass(frozen=True)
class Puzzle:
    date: Date
    _solver: Solver
    _input_filename: str = 'input'
    _solution_filename: Optional[str] = 'solution'

    @staticmethod
    def example(date: Date, solver: Solver) -> Puzzle:
        return Puzzle(date, solver, 'example', 'example_solution')

    @staticmethod
    def no_solution(date: Date, solver: Solver) -> Puzzle:
        return Puzzle(date, solver, 'input', None)

    def _read_to_string(self, filename: str):
        path = f'{PUZZLE_DIR}/{self.date.year}/{self.date.day}/{filename}'
        with open(path) as file:
            return file.read()

    def solve(self) -> Tuple[Answers, float]:
        input = self._read_to_string(self._input_filename)
        t0 = time.process_time()
        answers = getattr(self, '_solver')(input)
        t1 = time.process_time()
        return answers, t1 - t0

    def read_solution(self) -> Optional[Answers]:
        if self._solution_filename is None: return None
        path = (f'{PUZZLE_DIR}'
                f'/{self.date.year}/{self.date.day}'
                f'/{self._solution_filename}')
        with open(path) as file:
            sol1, sol2 = file.read().splitlines()
            return sol1, sol2


def answer(ans1: Any, ans2: Any) -> Answers:
    return str(ans1), str(ans2)
