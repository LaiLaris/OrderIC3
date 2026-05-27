#!/usr/bin/env python3
import csv
import re
import sys
from pathlib import Path

RUN = Path("/home/lyh/kind2-exp/OrderIC3_Auto/runs/20260524_170939_freq_v2")
PAIRWISE = RUN / "compare" / "pairwise_vs_base.csv"
LOG_ROOT = RUN / "logs" / "ic3qe_freq_sort"

VAR_RE = re.compile(r"top\.res\.2\.call_[0-9]+_[0-9]+@0")
TAG_RE = re.compile(r"\[exp=(\d+) cluster=(\d+) boundary_delay=(\d+) freq=([0-9.]+)\]")

def rows():
    with PAIRWISE.open(newline="") as f:
        for row in csv.DictReader(f):
            if row["variant"] != "ic3qe_freq_sort":
                continue
            name = row["filename"]
            if "DRAGON" in name or "/car_" in name:
                yield row

def log_for(filename):
    stem = Path(filename).with_suffix(".log")
    return LOG_ROOT / stem

def literal_kind(text):
    vars_ = VAR_RE.findall(text)
    if len(set(vars_)) != 1:
        return ("multi", None, None)
    var = vars_[0]
    if "(=" in text:
        return ("eq", var, None)
    if "(>" not in text and "(<" not in text and "(<=" not in text and "(>=" not in text:
        return ("other", var, None)
    # Very small normalization for the printed Kind2 forms used in these logs.
    # Direction is the semantic lower/upper side after top-level not for common > 0 shapes.
    neg = text.strip().startswith("(not")
    coeff = None
    if f"(* (- 1) {var})" in text:
        coeff = -1
    elif f"(* 1 {var})" in text:
        coeff = 1
    elif var in text:
        coeff = 1
    direction = None
    if "(>" in text and coeff is not None:
        lower = coeff > 0
        if neg:
            lower = not lower
        direction = "lower" if lower else "upper"
    return ("bound", var, direction)

def parse_log(path):
    if not path.exists():
        return None
    lines = path.read_text(errors="replace").splitlines()
    blocks = []
    i = 0
    while i < len(lines):
        if "ind-gen literal frequency priority for clause" not in lines[i]:
            i += 1
            continue
        start = i + 1
        lits = []
        in_priority = False
        i += 1
        while i < len(lines):
            line = lines[i]
            if "pair-structure-gated priority:" in line:
                in_priority = True
                i += 1
                continue
            if in_priority and (
                "Deactivating activation literals" in line
                or "New clause from inductive generalization" in line
                or line.startswith("block:")
            ):
                break
            if in_priority:
                m = TAG_RE.search(line)
                if m:
                    text = line[m.end():].strip()
                    i += 1
                    while i < len(lines):
                        nxt = lines[i]
                        if TAG_RE.search(nxt) or "Deactivating activation literals" in nxt or "New clause from inductive generalization" in nxt:
                            break
                        text += " " + nxt.strip()
                        i += 1
                    kind, var, direction = literal_kind(text)
                    lits.append({
                        "cluster": int(m.group(2)),
                        "kind": kind,
                        "var": var,
                        "direction": direction,
                        "text": text,
                    })
                    continue
            i += 1
        # Grab the next learned clause roughly.
        learned = ""
        for j in range(i, min(i + 20, len(lines))):
            if lines[j].startswith("#") or re.match(r"^#\d+ ", lines[j].strip()):
                learned = " ".join(x.strip() for x in lines[j:j+8])
                break
        blocks.append((start, lits, learned))
    return blocks

def summarize(path):
    blocks = parse_log(path)
    if blocks is None:
        return None
    same_dir_blocks = []
    mixed_dir_blocks = []
    eq_bound_blocks = []
    coupled_learned = 0
    eq_guard_learned = 0
    for line, lits, learned in blocks:
        learned_vars = VAR_RE.findall(learned)
        learned_var_set = set(learned_vars)
        has_eq = "(=" in learned
        # learned single bound + learned multi-var arithmetic over an overlapping var
        single_bound_vars = set()
        multi_lits = []
        for piece in re.split(r"\);|\}\s*", learned):
            vs = set(VAR_RE.findall(piece))
            if not vs:
                continue
            if len(vs) == 1 and "(>" in piece:
                single_bound_vars |= vs
            elif len(vs) > 1 and "(>" in piece:
                multi_lits.append(vs)
        if any(single_bound_vars & vs for vs in multi_lits):
            coupled_learned += 1
        if single_bound_vars and has_eq:
            eq_guard_learned += 1
        by_var = {}
        for lit in lits:
            if lit["cluster"] == 2 and lit["var"]:
                by_var.setdefault(lit["var"], []).append(lit)
        for var, xs in by_var.items():
            if len(xs) != 2:
                continue
            kinds = {x["kind"] for x in xs}
            dirs = [x["direction"] for x in xs if x["direction"]]
            retained = sum(1 for x in xs if compact(x["text"]) in compact(learned))
            item = (line, var, xs, learned, retained)
            if kinds == {"bound"} and len(set(dirs)) == 1:
                same_dir_blocks.append(item)
            elif kinds == {"bound"}:
                mixed_dir_blocks.append(item)
            elif "eq" in kinds and "bound" in kinds:
                eq_bound_blocks.append(item)
    return {
        "blocks": len(blocks),
        "same_dir": same_dir_blocks,
        "mixed_dir": mixed_dir_blocks,
        "eq_bound": eq_bound_blocks,
        "coupled_learned": coupled_learned,
        "eq_guard_learned": eq_guard_learned,
    }

def compact(s):
    return re.sub(r"\s+", "", s)

def main():
    detail = "--detail" in sys.argv
    if detail:
        wanted = set(sys.argv[sys.argv.index("--detail") + 1:])
    else:
        wanted = set()
    print("filename,comparison,ratio,base_time,var_time,blocks,same_dir,same_dir_retained1,same_dir_retained2,mixed_dir,eq_bound,coupled_learned,eq_guard_learned,example_same_dir_line")
    for row in rows():
        path = log_for(row["filename"])
        s = summarize(path)
        if s is None:
            continue
        ratio = row["time_ratio_variant_over_base"]
        example = s["same_dir"][0][0] if s["same_dir"] else ""
        r1 = sum(1 for item in s["same_dir"] if item[4] == 1)
        r2 = sum(1 for item in s["same_dir"] if item[4] == 2)
        print(",".join([
            row["filename"],
            row["comparison"],
            ratio,
            row["base_total_time_s"],
            row["variant_total_time_s"],
            str(s["blocks"]),
            str(len(s["same_dir"])),
            str(r1),
            str(r2),
            str(len(s["mixed_dir"])),
            str(len(s["eq_bound"])),
            str(s["coupled_learned"]),
            str(s["eq_guard_learned"]),
            str(example),
        ]))
        if detail and any(w in row["filename"] for w in wanted):
            for line, var, xs, learned, retained in s["same_dir"][:8]:
                print(f"  line={line} var={var} retained={retained}")
                for x in xs:
                    print(f"    {x['direction']} {x['text'][:180]}")
                print(f"    learned {learned[:240]}")

if __name__ == "__main__":
    main()
