#!/bin/env python3


from pydantic import BaseModel, ConstrainedStr, Field, conlist


class Char(ConstrainedStr):
    min_length = 1
    max_length = 1


class NonEmptyStr(ConstrainedStr):
    min_length = 1


Chars = conlist(Char, min_items=1)


class Action(BaseModel):
    read: Char
    to_state: NonEmptyStr
    write: Char
    action: NonEmptyStr


class Program(BaseModel):
    name: NonEmptyStr = Field(..., description="Name of program")

    alphabet: Chars = Field(..., description="Program symbols of the program")
    blank: Char = Field(..., description="Symbol to render as blank tape during execution")

    states: frozenset[NonEmptyStr]
    initial: NonEmptyStr
    finals: frozenset[NonEmptyStr]

    transitions: dict[NonEmptyStr, list[Action]]


if __name__ == "__main__":
    import json
    from pathlib import Path

    def gen_settings(schema: Path):
        return {
            "json.schemas": [
                {
                    "fileMatch": ["*.json", "!/.vscode/*.json"],
                    "url": schema.as_uri(),
                }
            ],
            "python.formatting.provider": "black",
            "python.formatting.blackArgs": [
                "--target-version",
                "py310",
                "--line-length",
                "100",
            ],
        }

    def save_schema(settings: Path, schema: Path, text: str):
        settings_text = gen_settings(schema)
        schema.write_text(text)
        settings.write_text(json.dumps(settings_text, indent=2))
        settings.parent.mkdir(parents=True, exist_ok=True)

    def print_diff(org: str, new: str):
        from difflib import unified_diff

        from pygments import highlight  # type: ignore
        from pygments.formatters import TerminalFormatter
        from pygments.lexers import DiffLexer

        diff = unified_diff(
            org.splitlines(keepends=True),
            new.splitlines(keepends=True),
            fromfile=schema.name,
            tofile=schema.name,
        )
        print(highlight("".join(diff), DiffLexer(), TerminalFormatter()))  # type: ignore

    settings = Path(".vscode/settings.json")
    schema = Path(".vscode/turing.schema.json").absolute()

    text = Program.schema_json(indent=2)

    if not schema.exists():
        print(text)
        exit(0)

    if (org_text := schema.read_text()) == text:
        exit(0)

    print_diff(org_text, text)
    save_schema(settings, schema, text)
