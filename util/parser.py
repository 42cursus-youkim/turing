from pathlib import Path

from lark.lark import Lark

lark = Lark.open("util/syntax.lark")  # type: ignore
text = Path("docs/examples/unary_sub.tr").read_text()

print(lark.parse(text).pretty())
