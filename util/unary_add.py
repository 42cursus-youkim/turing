from pathlib import Path


def gen(left: int, right: int) -> str:
    return f"{'1' * left}+{'1' * right}="


Path("test/inputs/unary_add.txt").write_text("\n".join(gen(i, 1) for i in range(1, 20)))
