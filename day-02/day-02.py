import sys
import dataclasses
from enum import Enum
from typing import Callable, List

Direction = Enum("Direction", "UP FORWARD DOWN")


@dataclasses.dataclass
class Instruction:
    direction: Direction
    amount: int


@dataclasses.dataclass
class Position:
    x: int = 0
    y: int = 0
    aim: int = 0


def parse_line(line: str) -> Instruction:
    [dir, amount] = line.split(" ")

    if dir == "forward":
        direction = Direction.FORWARD
    elif dir == "up":
        direction = Direction.UP
    elif dir == "down":
        direction = Direction.DOWN
    else:
        raise Exception(f"Can't parse: {line}")

    return Instruction(direction, int(amount))


def read_file(filename: str) -> List[str]:
    with open(filename) as f:
        lines = f.read().splitlines()
        return lines


def eval_instruction_p1(pos: Position, instruction: Instruction) -> Position:
    dir, amount = dataclasses.astuple(instruction)
    if dir == Direction.DOWN:
        return Position(pos.x, pos.y + amount)
    elif dir == Direction.UP:
        return Position(pos.x, pos.y - amount)
    elif dir == Direction.FORWARD:
        return Position(pos.x + amount, pos.y)


def eval_instruction_p2(pos: Position, instruction: Instruction) -> Position:
    dir, amount = dataclasses.astuple(instruction)
    if dir == Direction.DOWN:
        return Position(pos.x, pos.y, pos.aim + amount)
    elif dir == Direction.UP:
        return Position(pos.x, pos.y, pos.aim - amount)
    elif dir == Direction.FORWARD:
        return Position(pos.x + amount, pos.y + pos.aim * amount, pos.aim)


def eval(
    fn: Callable[[Position, Instruction], Position], instructions: List[Instruction]
) -> Position:
    position = Position()
    for instruction in instructions:
        position = fn(position, instruction)
    return position


def answer(pos: Position) -> int:
    return pos.x * pos.y


def main() -> int:
    lines = read_file("input.txt")
    instructions = list(map(parse_line, lines))

    p1 = answer(eval(eval_instruction_p1, instructions))
    print(f"Part One: {p1}")

    p2 = answer(eval(eval_instruction_p2, instructions))
    print(f"Part Two: {p2}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
