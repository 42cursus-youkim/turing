#!/bin/env python3

import json
from dataclasses import dataclass, field
from functools import cached_property
from parser import Convert, parser
from pathlib import Path
from typing import Any, Iterable, Literal, TypeVar

T = TypeVar("T")


def flatten(nested: Iterable[Iterable[T]]) -> Iterable[T]:
    return (item for sublist in nested for item in sublist)


@dataclass
class Match:
    read: list[str]
    write: str | None = None
    direction: Literal["<-", "->"] | None = None
    transition: str | None = None

    @cached_property
    def action(self):
        match self.direction:
            case "<-":
                return "LEFT"
            case "->":
                return "RIGHT"
            case None:
                raise ValueError("Direction must be specified")

    def to_dict(self, stay: str):
        return [
            {
                "read": r,
                "to_state": self.transition or stay,
                "write": self.write or r,
                "action": self.action,
            }
            for r in self.read
        ]


@dataclass
class State:
    name: str
    matches: list[Match] = field(default_factory=list)

    @cached_property
    def alphabet(self):
        return set(flatten(m.read for m in self.matches))

    @cached_property
    def transitions(self):
        return set(m.transition or self.name for m in self.matches)

    @classmethod
    def from_token(cls, token: list[list[Any]]):
        name: str
        name, matches = token  # type: ignore
        return cls(name, [Match(*m) for m in matches])

    def to_dict(self):
        nested = [m.to_dict(self.name) for m in self.matches]
        return self.name, [item for sublist in nested for item in sublist]


@dataclass
class Program:
    transitions: list[State]
    blank: str

    alphabets: set[str] = field(init=False)
    states: set[str] = field(init=False)
    finals: set[str] = field(init=False)

    def __post_init__(self):
        names = set(s.name for s in self.transitions)
        assert len(names) == len(self.transitions), "duplicate state names"

        self.alphabets = set(flatten(s.alphabet for s in self.transitions))
        assert self.blank in self.alphabets, f"{self.blank} must be in {self.alphabets}"

        to = set().union(*(s.transitions for s in self.transitions))  # type: ignore
        self.states = names | to
        self.finals = to - names

    def compile(self, name: str, *, indent: int = 2):
        def to_dict():
            all_transitions = [s.to_dict() for s in self.transitions]
            return {
                "name": name,
                "alphabet": list(self.alphabets),
                "blank": self.blank,
                "states": list(self.states),
                "initial": self.transitions[0].name,
                "finals": list(self.finals),
                "transitions": {k: v for k, v in all_transitions},
            }

        Path(name).with_suffix(".json").write_text(json.dumps(to_dict(), indent=indent))

    @classmethod
    def from_file(cls, path: Path | str):
        text = Path(path).read_text()
        tree = parser.parse(text)  # type: ignore

        blank: str
        blank, *tokens = Convert().transform(tree).children  # type: ignore

        states = [State.from_token(token) for token in tokens]  # type: ignore

        return cls(states, blank)


program = Program.from_file("docs/examples/unary_sub.tr")

program.compile("unary_sub", indent=2)
