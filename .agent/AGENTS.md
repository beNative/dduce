# AGENTS.md — Delphi Formatting Agent Contract

---

## 0) Agent Contract

**Goal**

- Reformat source to conform to V4 style **without changing behavior**.

**Scope**

- File types: `.pas` (Delphi units), `.dfm` (text; no visual edits).
- No refactors or renames unless explicitly requested with a flag.

**I/O**

- **Input:** Entire unit text (and optional flags; see §3).
- **Output (default **``**):** Full, formatted file content.
- **Optional modes:**
  - `MODE=diff` → unified diff (`--- a/…`, `+++ b/…`, `@@` hunks).
  - `MODE=patch` → `git apply`‑ready patch.

**Ask vs. act**

- If rules conflict or there is ambiguity, **prefer the most conservative change** (whitespace/format only) and **emit a short note** at end of output (or in diff header) describing the ambiguity.

---

## 1) Rule Precedence (conflict resolution)

1. **Do not change semantics** (logic, APIs, resources).
2. **Line length ≤ 80** (hard limit).
3. **Alignment** (colon in declarations; `:=` in short assignment runs).
4. **Naming prefixes** (`A`/`L`/`F`/`S`; `T`/`I`/`E`/`P`).
5. **Aesthetic preferences** (blank lines, order within logical groups).

**Notes**

- If alignment would exceed 80 chars, **wrap first**, then align the wrapped block locally.
- If dotted unit name and file name disagree, **do not rename** unless the request includes a rename flag (see §3 `FIX=rename`).

---

## 2) Core Style Summary (executable rules)

### 2.1 Uses ordering & wrapping

- Group order (top → bottom): `System.*`, `Winapi.*`, `Net.*`, `Data.*`, `Vcl.*`, `FMX.*`, then other libraries/project units.
- `interface uses` may span multiple lines; **multiple units per line** allowed.
- **Wrap after commas**; wrapped lines end with a comma; **final line** ends with `;`. Stay ≤ 80 chars per line.

### 2.2 Regions

- Use **per‑type wrapper regions** (e.g., `{$REGION 'TMyType'}` …).
- Sub‑region order inside a type: `construction and destruction` → `event dispatching methods` → `property access methods` → `event handlers` → `action handlers` → `private methods` → `protected methods` → `public methods`.
- Do **not** create empty regions. If the unit has **no** free routines, start directly with per‑type wrappers.

### 2.3 Naming prefixes

- **Parameters:** `A*` (event handlers specifically `ASender`).
- **Locals:** `L*` (allowed shorts: `I`, `S`, `SL`, `SB`).
- **Fields:** `F*`. **Constants:** `UPPER_SNAKE_CASE`. **Resourcestrings:** `S*`.
- **Types:** `T*` (classes/records/helpers/enums/aliases), `P*` (pointers to T), `E*` (exceptions), `I*` (interfaces).

### 2.4 Signatures & blocks

- **Interface** methods with ≥2 params: one param per line, align colons, `)` on its own line, return type on the same line as `)`.
- **Allman style:** `begin`/`end` on their own lines; `begin` after `then/else/do/of`; `else` on its own line.

### 2.5 Alignment

- Align `:` in consecutive declarations; align `:=` in **short sequences** of assignments; do not over‑align across unrelated blocks.

### 2.6 Long literals (HTML/CSS/JS)

- Split to ≤80 with `+` and `sLineBreak`.
- Prefer ``\*\* `` constants\*\* and call `Format(string(CONST), […])` when templating.

### 2.7 Misc

- Indent = **2 spaces**, no tabs. Strip trailing spaces. Final newline present.
- Dotted unit & filename must match (report mismatch; don’t auto‑rename).

---

## 3) Operating Flags (input hints)

```
MODE=full|diff|patch            (default full)
FIX=all|whitespace|uses|regions|strings|rename
STRICT=on|off                   (default on; if off, allow ≤100 chars in §2.6)
```

- `FIX=whitespace` → only indent/space/newline cleanup.
- `FIX=uses` → only regroup/wrap the uses clause.
- `FIX=regions` → only enforce region structure and headers.
- `FIX=strings` → only reflow long literals per §2.6.
- `FIX=rename` → allow dotted unit/file rename to match (include a note).

---

## 4) Formatting Protocol (do this in order)

1. **Normalize whitespace**: convert tabs→spaces (2), trim trailing spaces, ensure final newline.
2. **Unit name**: check dotted unit identifier vs. filename; if mismatch, *do not rename* unless `FIX=rename`; otherwise, report in note.
3. **Uses**: reorder into groups; wrap after commas; keep ≤80.
4. **Blocks**: enforce Allman; move single‑line bodies to `begin..end` if inconsistent within the unit.
5. **Declarations**: align `:` within contiguous blocks.
6. **Assignments**: align `:=` in short sequences.
7. **Naming**: enforce `A/L/F/S` prefixes; event params `ASender`; type prefixes `T/I/E/P`.
8. **Strings**: reflow long HTML/CSS/JS per §2.6; prefer template constants.
9. **Regions**: add per‑type wrapper and sub‑region headers; remove empty ones; obey order; if no free routines, start with per‑type wrappers.
10. **80‑char pass**: re‑scan and wrap where needed; repeat 5–8 locally if the wrapping created new alignment opportunities.
11. **Output**: full file or diff/patch per `MODE`.

---

## 5) Lint Rules (machine checks)

> These are guidance patterns, not strict regex grammar. Use them to verify output before returning it.

- **Params prefix**: in method headers, each param name starts with `A`, except event handlers where `Sender` must be `ASender`.
- **Field prefix**: in `private`/`strict private`, identifiers ending with `:` start with `F`.
- **Type names**: `type\s+([A-Z]\w+)\s*=\s*class` → name starts with `T`. `= interface` → starts with `I`. `= class\(Exception` → starts with `E`.
- **Line length**: no line > 80 chars (allow up to 100 only if `STRICT=off` and the line is inside a long literal per §2.6).
- **Uses grouping**: first non‑comment tokens after `uses` belong to `System.*` group and proceed in the specified order.

---

## 6) Templates (golden scaffolds)

### 6.1 Minimal unit template

```pascal
unit Module.Area.Role;

interface

uses
  System.SysUtils, System.Classes,
  Winapi.Windows,
  Vcl.Controls;

type
  TMyType = class(TObject)
  private
    FName : string;
  public
    constructor Create; override;
    property Name: string read FName write FName;
  end;

implementation

{$REGION 'TMyType'}
{$REGION 'construction and destruction'}
constructor TMyType.Create;
begin
  inherited Create;
  FName := '';
end;
{$ENDREGION}

{$REGION 'public methods'}
// ...
{$ENDREGION}
{$ENDREGION}

end.
```

### 6.2 Interface with GUID

```pascal
type
  IMyInterface = interface
  ['{190F445F-9A1E-4B08-A9E2-ACEBCF37AEEC}']
    procedure DoIt;
  end;
```

### 6.3 Multi‑line interface signature

```pascal
function SumAligned(
  const ALeft  : Integer;
  const ARight : Integer
): Integer;
```

---

## 7) Do / Don’t

**Long literals**

- ✅ Split with `+` and `sLineBreak`; prefer `PChar` constants + `Format`.
- ❌ One giant line or arbitrary mid‑word breaks.

**Regions**

- ✅ Per‑type wrapper when no free routines.
- ❌ Empty region blocks.

**Assignments**

- ✅ Align short consecutive runs on `:=`.
- ❌ Force alignment across unrelated areas or into >80 chars.

**Naming**

- ✅ `ASender` for events; `AFileName`/`AText` for others; `LVar` locals; `F*` fields; `T/I/E/P` for types.
- ❌ Leaving legacy prefixes (`U_`, `F_`) in filenames; renaming public APIs without instruction.

---

## 8) Output Examples

### 8.1 Unified diff (MODE=diff)

```diff
--- a/Module.Area.Role.pas
+++ b/Module.Area.Role.pas
@@
-uses System.SysUtils, Vcl.Controls;
+uses
+  System.SysUtils,
+  Vcl.Controls;
```

### 8.2 Patch (MODE=patch)

Same as diff, but include full file paths relative to repo root.

---

## 9) Ambiguity & Reporting

- If the agent encounters ambiguous cases (e.g., dotted unit name does not match file name; unknown third‑party groups in `uses`), **do not guess**. Apply conservative formatting and append a brief **NOTE** section at the end (or diff header) describing what needs human review.

---

## 10) Compliance with V4

This AGENTS.md summarizes **how** to apply *Delphi Formatting Rules — Draft v4*. If this file and V4 ever disagree, **V4 is the source of truth** for style. This file governs **agent behavior** (I/O, precedence, protocol, and flags).

