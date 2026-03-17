#!/usr/bin/env python3
"""
Extract per-case status, verification time, and top-level inductive
invariant clause counts from FMCAD08 logs and save them as a CSV.

Expected log filename format:
  <case_name>_original.log
  <case_name>_l1l2.log
  <case_name>_l2l1.log

Example:
  python3 scripts/analyze_invariant_clause_correlation.py \
    --log-root /home/lyh/kind2-exp/OrderIC3/log_pre_exp/FMCAD08/Int \
    --output-csv /tmp/fmcad08_invariant_clause_time.csv
"""

from __future__ import annotations

import argparse
import csv
import os
import re
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple


LABELS = ("original", "l1l2", "l2l1")
FILE_RE = re.compile(r"(.+?)_(original|l1l2|l2l1)\.log$")
SUCCESS_RE = re.compile(r"<Success> Property .* after ([0-9.]+)s\.")
FAILURE_RE = re.compile(r"<Failure> Property .* after ([0-9.]+)s\.")
DONE_RE = re.compile(r"<Done> All properties proved or disproved in\s+([0-9.]+)s\.")


@dataclass
class RunRecord:
    case_id: str
    label: str
    status: str
    clause_count: str
    verify_time: str


def find_logs(log_root: str) -> Dict[str, Dict[str, str]]:
    grouped: Dict[str, Dict[str, str]] = defaultdict(dict)
    for dp, _, files in os.walk(log_root):
        for fn in files:
            match = FILE_RE.match(fn)
            if not match:
                continue
            base, label = match.groups()
            case_id = os.path.relpath(os.path.join(dp, base), log_root)
            grouped[case_id][label] = os.path.join(dp, fn)
    return grouped


def extract_verify_time(text: str) -> Optional[float]:
    success = SUCCESS_RE.search(text)
    if success:
        return float(success.group(1))
    failure = FAILURE_RE.search(text)
    if failure:
        return float(failure.group(1))
    done = DONE_RE.search(text)
    if done:
        return float(done.group(1))
    return None


def extract_status(text: str) -> str:
    if SUCCESS_RE.search(text):
        return "safe"
    if FAILURE_RE.search(text):
        return "unsaf"
    return "unknown"


def extract_invariant_block(text: str) -> Optional[str]:
    marker = "Inductive invariant:"
    idx = text.find(marker)
    if idx < 0:
        return None

    start = text.find("{", idx)
    if start < 0:
        return None

    depth = 0
    for pos in range(start, len(text)):
        ch = text[pos]
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0:
                return text[start : pos + 1]
    return None


def count_top_level_clauses(block: str) -> int:
    if len(block) < 2 or block[0] != "{" or block[-1] != "}":
        return 0
    inner = block[1:-1]
    if not inner.strip():
        return 0
    return inner.count(";") + 1


def parse_log(path: str, case_id: str, label: str) -> Optional[RunRecord]:
    text = Path(path).read_text(encoding="utf-8", errors="ignore")
    verify_time = extract_verify_time(text)
    block = extract_invariant_block(text)
    return RunRecord(
        case_id=case_id,
        label=label,
        status=extract_status(text),
        clause_count=str(count_top_level_clauses(block)) if block is not None else "noInv",
        verify_time=f"{verify_time:.6f}" if verify_time is not None else "unknown",
    )


def write_case_csv(path: str, rows: List[Dict[str, object]]) -> None:
    fieldnames = [
        "case_id",
        "status",
        "original_time",
        "original_inv_num",
        "l1l2_time",
        "l1l2_num",
        "l2l1_time",
        "l2l1_num",
    ]
    with open(path, "w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--log-root", required=True, help="Root directory of logs")
    parser.add_argument(
        "--output-csv",
        required=True,
        help="CSV path for per-case invariant clause count and verification time",
    )
    args = parser.parse_args()

    grouped = find_logs(args.log_root)
    by_case: Dict[str, Dict[str, RunRecord]] = defaultdict(dict)

    for case_id, mapping in sorted(grouped.items()):
        for label, path in sorted(mapping.items()):
            parsed = parse_log(path, case_id, label)
            by_case[case_id][label] = parsed

    rows: List[Dict[str, object]] = []
    mismatch_rows: List[Dict[str, object]] = []
    for case_id in sorted(grouped):
        case_records = by_case.get(case_id, {})
        original = case_records.get("original")
        l1l2 = case_records.get("l1l2")
        l2l1 = case_records.get("l2l1")
        original_status = original.status if original else "unknown"
        l1l2_status = l1l2.status if l1l2 else "unknown"
        l2l1_status = l2l1.status if l2l1 else "unknown"
        merged_status = (
            original_status
            if original_status == l1l2_status == l2l1_status
            else "mismatch"
        )
        rows.append(
            {
                "case_id": case_id,
                "status": merged_status,
                "original_time": original.verify_time if original else "unknown",
                "original_inv_num": original.clause_count if original else "noInv",
                "l1l2_time": l1l2.verify_time if l1l2 else "unknown",
                "l1l2_num": l1l2.clause_count if l1l2 else "noInv",
                "l2l1_time": l2l1.verify_time if l2l1 else "unknown",
                "l2l1_num": l2l1.clause_count if l2l1 else "noInv",
            }
        )
        if merged_status == "mismatch":
            mismatch_rows.append(
                {
                    "case_id": case_id,
                    "status": merged_status,
                    "original_time": original.verify_time if original else "unknown",
                    "original_inv_num": original.clause_count if original else "noInv",
                    "l1l2_time": l1l2.verify_time if l1l2 else "unknown",
                    "l1l2_num": l1l2.clause_count if l1l2 else "noInv",
                    "l2l1_time": l2l1.verify_time if l2l1 else "unknown",
                    "l2l1_num": l2l1.clause_count if l2l1 else "noInv",
                }
            )

    normal_rows = [row for row in rows if row["status"] != "mismatch"]
    write_case_csv(args.output_csv, normal_rows + mismatch_rows)

    print(f"log_root: {args.log_root}")
    print(f"cases_found: {len(grouped)}")
    print(f"rows_written: {len(rows)}")
    print(f"parsed_logs: {sum(len(v) for v in by_case.values())}")
    print(f"output_csv: {args.output_csv}")
    print(f"mismatch_cases: {len(mismatch_rows)}")
    if mismatch_rows:
        print("mismatch_examples:")
        for row in mismatch_rows:
            print(f"  {row['case_id']}")


if __name__ == "__main__":
    main()
