#!/bin/env python3


from typing import Literal
from pydantic import BaseModel, ConstrainedStr, Field, conlist

from utils import print_diff, save_schema


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
    action: Literal["LEFT", "RIGHT"]


class Program(BaseModel):
    name: NonEmptyStr = Field(..., description="Name of program")

    alphabet: Chars = Field(..., description="Program symbols of the program")
    blank: Char = Field(..., description="Symbol to render as blank tape during execution")

    states: frozenset[NonEmptyStr]
    initial: NonEmptyStr
    finals: frozenset[NonEmptyStr]

    transitions: dict[NonEmptyStr, list[Action]]


if __name__ == "__main__":
    from pathlib import Path

    settings = Path(".vscode/settings.json")
    schema = Path(".vscode/turing.schema.json").absolute()

    text = Program.schema_json(indent=2)

    if not schema.exists():
        print(text)
        exit(0)

    if (org_text := schema.read_text()) != text:
        print_diff(org_text, text)

    save_schema(settings, schema, text)
