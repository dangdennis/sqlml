#!/usr/bin/env python3
"""Compare the version-pinned pGenie analysis model with sqlml corpus contracts.

pGenie runs only in an isolated project and temporary database. Unknown failures,
timeouts, missing cases, and unexplained differences are fatal.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile

VERSION = "0.15.0"


def unsupported(case):
    nodes = [node for field in case["columns"] + case["params"]
             for node in field["type"]["types"]]
    if any(kind[0] == "domain" for _, kind in nodes):
        return "pGenie 0.15.0: Domain types are not supported yet"
    if any(identity == ["compiler_a", "dropped"] for identity, _ in nodes):
        return "pGenie 0.15.0: dropped composite attribute is reported as type OID 0"
    if any(kind[0] == "range" and identity[0] != "pg_catalog" for identity, kind in nodes):
        return "pGenie 0.15.0: custom Range types are not supported yet"
    return None


def expected(graph):
    registry = {tuple(i): k for i, k in graph["types"]}
    def norm(identity):
        k = registry[tuple(identity)]
        if k[0] == "array":
            return ["array", norm(k[1])]
        return identity
    return norm(graph["root"])


def actual(value):
    scalar = value["scalar"]
    if "primitive" in scalar:
        typ = ["pg_catalog", scalar["primitive"].replace("-", "")]
    else:
        typ = [scalar["custom"]["pg-schema"], scalar["custom"]["pg-name"]]
    return ["array", typ] if value["dimensionality"] else typ


def compare(case, query, custom_types):
    cols = query["result"]["rows"]["columns"]
    for kind, left, right in [("parameters", case["params"], query["params"]),
                              ("columns", case["columns"], cols)]:
        if len(left) != len(right):
            raise AssertionError(f"{kind}: count differs")
        for a, b in zip(left, right):
            if a["name"] != b["pg-name"] or expected(a["type"]) != actual(b["value"]):
                raise AssertionError(f"{kind}: {a['name']} {expected(a['type'])} != {b['pg-name']} {actual(b['value'])}")
            registry = {tuple(i): k for i, k in a["type"]["types"]}
            for identity, definition in registry.items():
                if definition[0] not in ("enum", "composite"):
                    continue
                c = custom_types.get(identity)
                if c is None:
                    raise AssertionError(f"missing custom type definition: {identity}")
                if definition[0] == "enum":
                    if [x["pg-name"] for x in c["definition"]["enum"]] != definition[1]:
                        raise AssertionError(f"enum labels/order differ: {identity}")
                else:
                    fields = c["definition"]["composite"]
                    want = definition[1]
                    if len(fields) != len(want):
                        raise AssertionError(f"composite arity differs: {identity}")
                    for (name, typ, _, _), field in zip(want, fields):
                        graph = {"root": typ, "types": a["type"]["types"]}
                        if field["pg-name"] != name or actual(field["value"]) != expected(graph):
                            raise AssertionError(f"composite field differs: {identity}.{name}")


def run_batch(pgn, database_url, folder, schema, cases, report, reduce=True):
    folder.mkdir()
    (folder / "queries").mkdir()
    (folder / "migrations").mkdir()
    (folder / "project1.pgn.yaml").write_text(
        "space: sqlml\nname: differential\nversion: 0.1.0\npostgres: 18\nartifacts: {}\n")
    (folder / "migrations" / "1.sql").write_text(schema)
    by_name = {}
    for case in cases:
        name = "q_" + case["name"][1:]
        by_name[name] = case
        # Corpus placeholders are generated, never free-form user SQL.
        (folder / "queries" / (name + ".sql")).write_text(re.sub(r"(?<!:):p\b", "$p", case["sql"]))
    with (folder / "model.json").open("w") as output, (folder / "analysis.log").open("w") as log:
        subprocess.run([pgn, "--database-url", database_url, "analyse", "--output", "json"],
                       cwd=folder, stdout=output, stderr=log, check=True, timeout=600)
    model = json.loads((folder / "model.json").read_text())
    custom = {(t["pg-schema"], t["pg-name"]): t for t in model["custom-types"]}
    found = set()
    for query in model["queries"]:
        name = query["name"]["in-snake-case"]
        if name in found or name not in by_name:
            raise AssertionError(f"duplicate/unexpected pGenie query {name}")
        found.add(name)
        case = by_name[name]
        try:
            compare(case, query, custom)
        except Exception as error:
            # Reduce the failing corpus to its single query, preserving both contracts.
            (folder / "failure.json").write_text(json.dumps({"case": case, "pgenie": query,
                                                              "error": str(error)}, indent=2))
            if reduce and "minimal_sql" in case and case["minimal_sql"] != case["sql"]:
                candidate = dict(case, sql=case["minimal_sql"],
                                 columns=[c for c in case["columns"] if c["name"] not in ("ordinal", "total")])
                try:
                    run_batch(pgn, database_url, folder / "reduced", schema, [candidate],
                              {"compared": 0, "policy_differences": []}, reduce=False)
                except AssertionError:
                    (folder / "minimized-case.json").write_text(json.dumps(candidate, indent=2))
            raise
        report["compared"] += 1
        for field in query["result"]["rows"]["columns"]:
            if not field["is-nullable"]:
                report["policy_differences"].append({"case": case["name"], "column": field["pg-name"],
                    "reason": "sqlml defaults to nullable; pGenie claims non-null"})
    if found != set(by_name):
        raise AssertionError(f"missing pGenie cases: {set(by_name) - found}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("artifacts", type=Path)
    parser.add_argument("--pgn", default="pgn")
    args = parser.parse_args()
    pgn = shutil.which(args.pgn)
    if not pgn:
        parser.error("pgn 0.15.0 must be installed explicitly")
    # The official 0.15.0 binaries report "0" for --version. Verify their bytes.
    digest = hashlib.sha256(Path(pgn).read_bytes()).hexdigest()
    if digest not in {
        "8968010e71d0e6033f94043ca15f50d46a77e70686a98272a430a620a7ff5e2a",  # macOS arm64
        "994f146bf2b74e6dc66383363dcb21b2cf40fd52df56a7bf06fec89447dfdcf0",  # Linux x64
    }:
        raise RuntimeError(f"binary is not a verified pGenie {VERSION} release: {digest}")
    cases = [json.loads(line) for line in (args.artifacts / "contracts.jsonl").read_text().splitlines()]
    if not cases:
        raise RuntimeError("empty corpus")
    report = {"version": VERSION, "total": len(cases), "compared": 0, "unsupported": [],
              "policy_differences": [], "batch_policy": "Separate schemas: pGenie rejects normalized same-name custom types in one project"}
    batches = {"a": [], "b": []}
    for case in cases:
        reason = unsupported(case)
        if reason:
            report["unsupported"].append({"case": case["name"], "reason": reason})
        else:
            batches["b" if "compiler_b." in case["sql"] else "a"].append(case)
    folder = Path(tempfile.mkdtemp(prefix="pgenie-", dir=args.artifacts.resolve()))
    schema = (args.artifacts / "schema.sql").read_text()
    try:
        for name, batch in batches.items():
            if batch:
                # Bound pGenie's connection fan-out independently of corpus size.
                for start in range(0, len(batch), 50):
                    run_batch(pgn, os.environ["DATABASE_URL"], folder / f"{name}-{start}",
                              schema, batch[start:start + 50], report)
        if report["compared"] + len(report["unsupported"]) != len(cases):
            raise AssertionError("unaccounted cases")
        report["status"] = "passed"
    except Exception as error:
        report["status"] = "failed"
        report["error"] = str(error)
        raise
    finally:
        (args.artifacts / "pgenie-report.json").write_text(json.dumps(report, indent=2) + "\n")
    print(f"pGenie: {report['compared']} compared; {len(report['unsupported'])} explicitly unsupported")


if __name__ == "__main__":
    main()
