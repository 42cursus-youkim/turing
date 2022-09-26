from pathlib import Path
from pprint import pprint
from typing import Literal

from lark.lark import Lark
from lark.visitors import Transformer

first = lambda self, f: f  # type: ignore
first_str = lambda self, s: str(s[0])  # type: ignore
first_str_maybe = lambda self, s: str(s[0])[1:-1] if s[0] else None  # type: ignore


class Convert(Transformer):  # type: ignore
    state, matches, match = [first for _ in range(3)]  # type: ignore
    name, direction = [first_str for _ in range(2)]  # type: ignore
    write = first_str_maybe  # type: ignore

    def blank(self, b: list[str]):
        return str(b[0][1:-1])

    def read(self, s: list[str]):
        return list(s[0][1:-1])

    def transition(self, t: list[str]):
        return str(t[0]) if t[0] else None


parser = Lark.open("util/syntax.lark", parser="lalr")  # type: ignore

if __name__ == "__main__":
    tree = parser.parse(Path("script/unary_sub.tr").read_text())
    print(tree.pretty())
    res = Convert().transform(tree)  # type: ignore
    pprint(res)  # type: ignore
