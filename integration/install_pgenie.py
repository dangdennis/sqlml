#!/usr/bin/env python3
"""Install the verified pGenie 0.15.0 test executable into a supplied directory."""
import argparse
import hashlib
import platform
from pathlib import Path
import subprocess
import tarfile
import tempfile

p = argparse.ArgumentParser(description=__doc__)
p.add_argument("directory", type=Path)
a = p.parse_args()
releases = {
    ("Darwin", "arm64"): ("macos-arm64", "f3078e979f9979aa9f3f9ed9b329b64cf888a2c0bb043b8c409ae17005eb13f6"),
    ("Linux", "x86_64"): ("linux-x64", "d4817a74b2d9e3a3cefc1451dc63cf232c4cb2f42ca3dd631ec04eded177b83b"),
}
try:
    target, digest = releases[(platform.system(), platform.machine())]
except KeyError:
    p.error("verified binary currently available for macOS arm64 and Linux x64")
a.directory.mkdir(parents=True, exist_ok=True)
with tempfile.TemporaryDirectory(prefix="sqlml-pgn-") as tmp:
    archive = Path(tmp) / "pgn.tar.gz"
    subprocess.run(["curl", "--fail", "--location", "--silent", "--show-error", "--max-time", "120",
                    f"https://github.com/pgenie-io/pgenie/releases/download/v0.15.0/pgn-{target}.tar.gz",
                    "--output", str(archive)], check=True)
    if hashlib.sha256(archive.read_bytes()).hexdigest() != digest:
        raise RuntimeError("pGenie release archive checksum mismatch")
    with tarfile.open(archive) as tar:
        tar.extractall(a.directory, filter="data")
print(a.directory.resolve() / "pgn")
