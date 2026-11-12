#!/usr/bin/env python3
"""Copy the modules that bell-fp.tex quotes into src-ascii/, with
their comments removed and every non-ASCII character replaced by an ASCII
spelling.

The listings package under XeTeX cannot typeset characters such as the
lambda that the sources use in comments, and it leaks the non-ASCII
characters of lines outside a linerange into the output. The copies have
exactly the same number of lines as the originals, so the line ranges in
the paper refer to the real sources. A line that held only a comment is
empty in the copy; the paper quotes no such line, and this script stops
if a range of the paper takes in an empty line.

  src-ascii/CHSH/   src/CHSH/*.hs, the package
  src-ascii/gen/    paper/gen/Examples.hs, the paper's examples

It also writes src-ascii/language.tex, the listings language Hask with the
names of these modules: their types and constructors, and every name they
declare with a type signature, so that the paper highlights each name the
same way wherever it appears.

Run from anywhere:  python3 paper/asciify.py
"""
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent

ASCII = {
    "λ": "lambda", "π": "pi", "θ": "theta", "Φ": "Phi",
    "’": "'", "‘": "'", "“": '"', "”": '"',
    "—": "--", "–": "-", "…": "...",
    "∧": "and", "∨": "or", "⊕": "xor", "√": "sqrt",
    "≈": "~", "≠": "/=", "∈": "in", "∉": "not in",
    "≤": "<=", "≥": ">=", "±": "+/-", "×": "x",
    "·": ".", "→": "->", "←": "<-", "⇒": "=>",
    "✓": "OK", "✗": "FAILED",
}

SYMBOL = set("!#$%&*+./<=>?@\\^|-~:")

def strip_comment(line: str) -> str:
    """The line without its Haskell line comment and trailing blanks. A
    comment starts at two or more dashes that are not part of an operator,
    outside string and character literals."""
    i, n = 0, len(line)
    in_string = False
    while i < n:
        c = line[i]
        if in_string:
            if c == "\\":
                i += 2
                continue
            if c == '"':
                in_string = False
        elif c == '"':
            in_string = True
        elif c == "'" and (i == 0 or not (line[i - 1].isalnum() or line[i - 1] in "_'")):
            # a character literal, not the prime of a name
            end = line.find("'", i + (3 if line[i + 1:i + 2] == "\\" else 2))
            if end != -1:
                i = end + 1
                continue
        elif line.startswith("--", i):
            j = i
            while j < n and line[j] == "-":
                j += 1
            if (i == 0 or line[i - 1] not in SYMBOL) and (j == n or line[j] not in SYMBOL):
                return line[:i].rstrip()
            i = j
            continue
        i += 1
    return line.rstrip()

def asciify(text: str, name: str) -> str:
    out = []
    for lineno, line in enumerate(text.split("\n"), 1):
        for ch in set(line):
            if ord(ch) > 127:
                if ch not in ASCII:
                    sys.exit(f"{name}:{lineno}: no ASCII spelling for {ch!r} (U+{ord(ch):04X})")
                line = line.replace(ch, ASCII[ch])
        out.append(line)
    return "\n".join(out)

def copy(src: Path, dst: Path) -> None:
    text = src.read_text(encoding="utf-8")
    plain = "\n".join(strip_comment(line) for line in text.split("\n"))
    ascii_text = asciify(plain, src.name)
    assert ascii_text.count("\n") == text.count("\n"), src
    dst.parent.mkdir(parents=True, exist_ok=True)
    if not dst.exists() or dst.read_text(encoding="utf-8") != ascii_text:
        dst.write_text(ascii_text, encoding="utf-8")

KEYWORDS = ("case class data deriving do else forall if import in infix infixl infixr instance "
            "let module newtype of qualified then type where").split()

# library functions that the quoted code calls
LIBRARY = ("pure liftA2 liftIO mapM ask asks get gets modify modify' randomRIO "
           "runReaderT runReader evalState evalStateT runIdentity fromIntegral sqrt abs "
           "length filter sum const not cos pi even null setStdGen mkStdGen "
           "concat unlines show minBound maxBound").split()

def names(sources):
    """The capitalised names used in code (types, classes, constructors)
    and the lower-case names declared with a type signature."""
    caps, sigs = set(), set()
    for src in sources:
        lines = src.read_text(encoding="utf-8").split("\n")
        codes = [re.sub(r'"([^"\\]|\\.)*"', '""', line).split("--")[0] for line in lines]
        for k, code in enumerate(codes):
            if code.startswith("import") or code.startswith("module"):
                continue
            caps |= set(re.findall(r"\b[A-Z][A-Za-z0-9_']*", code))
            m = re.match(r"^\s*([a-z_][A-Za-z0-9_']*(?:\s*,\s*[a-z_][A-Za-z0-9_']*)*)\s*::", code)
            if m and not re.match(r"^\s*[{,]", code):
                sigs |= {name.strip() for name in m.group(1).split(",")}
            # a signature over several lines: the name alone, then "::"
            m = re.match(r"^([a-z_][A-Za-z0-9_']*)\s*$", code)
            if m and k + 1 < len(codes) and re.match(r"^\s+::", codes[k + 1]):
                sigs.add(m.group(1))
    return sorted(caps), sorted(sigs | set(LIBRARY))

def language(sources) -> str:
    caps, funs = names(sources)
    def wrap(words):
        lines, line = [], "    "
        for w in words:
            if len(line) + len(w) > 76:
                lines.append(line.rstrip())
                line = "    "
            line += w + ","
        lines.append(line.rstrip(","))
        return "\n".join(lines)
    return ("% written by asciify.py from the quoted modules; do not edit\n"
            "\\lstdefinelanguage{Hask}{\n"
            "  morekeywords=[1]{\n" + wrap(KEYWORDS) + "},\n"
            "  morekeywords=[2]{\n" + wrap(caps) + "},\n"
            "  morekeywords=[3]{\n" + wrap(funs) + "},\n"
            "  sensitive=true,\n"
            "  morecomment=[l]{--},\n"
            "  morestring=[b]\",\n"
            "}\n")

def check_ranges(out: Path) -> None:
    """Every line that a listing of the paper quotes is code: not empty in
    the copy, so neither blank nor a comment in the source."""
    tex = "\n".join(re.sub(r"(?<!\\)%.*", "", line) for line in
                    (HERE / "bell-fp.tex").read_text(encoding="utf-8").split("\n"))
    bad = []
    for ranges, name in re.findall(r"\\code(?:\[[^\]]*\])?\{([^}]*)\}\{([^}]*)\}", tex):
        lines = (out / name).read_text(encoding="utf-8").split("\n")
        for r in ranges.split(","):
            a, b = (int(x) for x in r.split("-"))
            for k in range(a, b + 1):
                if k > len(lines) or not lines[k - 1].strip():
                    bad.append(f"{name}:{k} (in {ranges})")
    if bad:
        sys.exit("listings of the paper take in empty lines: " + ", ".join(bad))

def main() -> None:
    out = HERE / "src-ascii"
    count = 0
    for folder, sources in [
        ("CHSH", sorted((ROOT / "src" / "CHSH").glob("*.hs"))),
        ("gen", [HERE / "gen" / "Examples.hs"]),
    ]:
        for src in sources:
            copy(src, out / folder / src.name)
            count += 1
    sources = sorted((ROOT / "src" / "CHSH").glob("*.hs")) + [HERE / "gen" / "Examples.hs"]
    lang = language(sources)
    target = out / "language.tex"
    if not target.exists() or target.read_text(encoding="utf-8") != lang:
        target.write_text(lang, encoding="utf-8")
    check_ranges(out)
    print(f"wrote {count} ASCII copies and the listings language into {out}")

if __name__ == "__main__":
    main()
