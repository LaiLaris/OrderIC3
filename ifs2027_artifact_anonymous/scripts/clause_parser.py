#!/usr/bin/env python3
"""Compare IC3/IC3QE log clause blocks.

Finds the earliest same-clause literal order divergence and the earliest
same-clause content divergence between two Kind2 IC3-style trace logs.
"""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path


CLAUSE_RE = re.compile(r"^(#(\d+) \{)(.*)$")


@dataclass
class ClauseBlock:
    cid: int
    line: int
    literals: list[str]


def normalize(text: str) -> str:
    return " ".join(text.strip().split())


def parse_clause_blocks(path: Path) -> list[ClauseBlock]:
    lines = path.read_text(errors="replace").splitlines()
    clauses: list[ClauseBlock] = []
    i = 0

    while i < len(lines):
        match = CLAUSE_RE.match(lines[i].strip())
        if not match:
            i += 1
            continue

        cid = int(match.group(2))
        start_line = i + 1
        literals: list[str] = []
        current: list[str] = []

        def finish_current() -> None:
            nonlocal current
            if current:
                literal = normalize(" ".join(current))
                if literal:
                    literals.append(literal)
                current = []

        def feed(part: str) -> bool:
            nonlocal current
            part = part.strip()
            if not part:
                return False
            has_end = "}" in part
            if has_end:
                part = part.rsplit("}", 1)[0].rstrip()
            pieces = part.split(";")
            for index, piece in enumerate(pieces):
                piece = piece.strip()
                if piece:
                    current.append(piece)
                if index < len(pieces) - 1:
                    finish_current()
            if has_end:
                finish_current()
            return has_end

        if feed(match.group(3)):
            clauses.append(ClauseBlock(cid=cid, line=start_line, literals=literals))
            i += 1
            continue

        i += 1

        while i < len(lines):
            if feed(lines[i].strip()):
                break
            i += 1

        clauses.append(ClauseBlock(cid=cid, line=start_line, literals=literals))
        i += 1

    return clauses

