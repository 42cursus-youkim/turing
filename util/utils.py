import json
from pathlib import Path


def gen_settings(schema: Path):
    return {
        "json.schemas": [
            {
                "fileMatch": [f"./{p}/*.json" for p in Path("doc").glob("**/")],
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
        fromfile="original schema",
        tofile="new schema",
    )
    print(highlight("".join(diff), DiffLexer(), TerminalFormatter()))  # type: ignore
