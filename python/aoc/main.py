from aoc.common import Puzzle
from aoc.y2015 import puzzles
from colors import red, green


def main():
    present_puzzles()


def present_puzzles():
    for puzzle in puzzles.values():
        present_puzzle(puzzle)


def present_puzzle(puzzle: Puzzle):
    date_str = f'{puzzle.date}'
    answers, duration = puzzle.solve()
    solutions = puzzle.read_solution()
    duration = round(duration * 1e6)
    if duration < 1000:
        duration_str = f'{duration:7} μs'
    elif duration < 1_000_000:
        dur_int, dur_frac = divmod(duration, 1000)
        duration_str = f'{dur_int:3}.{dur_frac:03} ms'
    else:
        dur_int, dur_frac = divmod(duration, 1_000_000)
        duration_str = red(f'{dur_int:3}.{dur_frac:03} s ')
    ans1, ans2 = answers
    solutions_str = ''
    if solutions is None:
        answer_str = f'({ans1}, {ans2})'
    else:
        sol1, sol2 = solutions
        ans1_str = green(f'{ans1}') if ans1 == sol1 else red(f'{ans1}')
        ans2_str = green(f'{ans2}') if ans2 == sol2 else red(f'{ans2}')
        answer_str = f'({ans1_str}, {ans2_str})'
        if answers != solutions:
            solutions_str = f' solution = ({sol1}, {sol2})'
    print(f'{date_str} {duration_str} {answer_str}{solutions_str}')


if __name__ == '__main__':
    main()
