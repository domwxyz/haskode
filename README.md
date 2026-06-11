# Haskode

A small, hackable, educational, Haskell-native LLM coding harness.

**License:** 0BSD — do whatever you want with it.

## Vision

Haskode is a terminal coding agent where **Haskell owns the main loop**. Inspired by OpenCode, Pi, and suckless design philosophy, it aims to be:

- **Small** — readable in an afternoon, fits in your head
- **Hackable** — every component is a plain Haskell module you can swap out
- **Educational** — heavily commented, no clever tricks, learn-by-reading
- **Native** — no Python, no Node, no wrapper scripts; just `cabal build` and go


The core loop: load config → build context → call LLM → manage typed tool
calls → read/edit files → run shell commands → show diffs → record session
events → repeat.

## Architecture

```
haskode/
|-- haskode.cabal          Build configuration
|-- app/Main.hs            CLI entry point (optparse-applicative)
|-- src/Haskode/
|   |-- Agent.hs           Agent loop and system-prompt construction
|   |-- Commands.hs        Slash command registry, status, and doctor output
|   |-- Config.hs          Config loading, defaults, and env expansion
|   |-- Core.hs            Core data types (Role, Message, ToolCall, etc.)
|   |-- Display.hs         CLI display formatting and display-event seam
|   |-- Extension.hs       Compiled extension record and tool/policy/command finalization
|   |-- Extensions.hs      Local compiled extension registration point
|   |-- Patch.hs           Patch model, diff rendering, path safety, batch helpers
|   |-- Policy.hs          Permission gate (allow/deny/ask)
|   |-- Provider.hs        Provider interface plus stub/scripted providers
|   |-- Provider/
|   |   |-- Anthropic.hs    Anthropic Messages API provider
|   |   `-- OpenAI.hs      OpenAI-compatible chat-completions provider
|   |-- Session.hs         Session event log, summaries, rotation, resume context
|   |-- Tools.hs           Public tool registry and read/search/shell tools
|   |-- Tools/
|   |   `-- FileEdit.hs    Patch, batch patch, and write-file tool implementations
|   `-- Tui.hs             Experimental isolated Brick TUI wrapper
`-- test/
    |-- Spec.hs            No-framework test runner
    `-- Haskode/Test/      Split focused test modules
```

### Module responsibilities

| Module | Purpose |
|---|---|
| `Haskode.Core` | Shared vocabulary: `Role`, `Message`, `ToolCall`, `ToolResult`, `Conversation` |
| `Haskode.Config` | Load `haskode.json` / `haskode.jsonc` / `~/.config/haskode/config.json`; parse plain JSON config, optional context/session-log/disabled-tool fields, and environment variables |
| `Haskode.Commands` | Pure slash-command registry, extension command contribution shape, and formatters for `/help`, `/new`, `/compact`, `/status`, `/doctor`, `/exit`, and `/quit` |
| `Haskode.Display` | Terminal display formatting plus the small `DisplayEvent` seam consumed by CLI and TUI paths |
| `Haskode.Extension` | Tiny compiled extension record plus finalization helpers for extension tools, policy rules, and pure text slash commands |
| `Haskode.Extensions` | Empty-by-default local registration point for statically compiled extensions in a fork |
| `Haskode.Provider` | Record-of-functions interface for LLM backends. Ships `stubProvider` (echo) and `scriptedProvider` (test replay) |
| `Haskode.Provider.OpenAI` | Real OpenAI-compatible HTTP provider (`/v1/chat/completions`). Works with OpenAI, Ollama, vLLM, LiteLLM, OpenRouter |
| `Haskode.Provider.Anthropic` | Anthropic Messages API provider with native `tool_use` / `tool_result` handling and streaming assembly |
| `Haskode.Tools` | `Tool` type, `ToolRegistry`, built-in registry, disabled-tool filtering, and read/search/shell tools |
| `Haskode.Tools.FileEdit` | Internal implementations for `preview_patch`, `apply_patch`, `preview_patch_batch`, `apply_patch_batch`, and `write_file` |
| `Haskode.Policy` | Composable rules: `Allow`, `Deny`, or `AskUser`. Default policy blocks `rm -rf /` |
| `Haskode.Session` | In-memory event log flushed to `session.jsonl`; summaries, rotation, and conservative text-only resume context |
| `Haskode.Patch` | Represent file changes, show unified diffs, apply/rollback |
| `Haskode.Agent` | The conversation state machine. Calls provider, processes tool calls, loops |
| `Haskode.Tui` | Experimental Brick wrapper over the same agent state, command registry, display events, provider path, and session log |
| `test/Haskode/Test.*` | Split no-framework test modules for config, providers, tools, commands, display, TUI helpers, session/resume, policy, patches, and core data |

### Design principles

1. **Record-of-functions over typeclasses** — providers and tools are plain records, easy to swap at runtime and mock in tests
2. **Text, not String** — strict `Text` everywhere for performance
3. **JSON on the wire** — aeson instances follow the OpenAI chat-completion format
4. **Conservative policy** — unknown tool calls require user confirmation by default
5. **No framework in the core** — the reference CLI path stays plain Cabal and plain Haskell. Brick is isolated to the explicit experimental TUI layer; no Template Haskell, no lens, no effect systems

## Build & run

### Prerequisites

- GHC >= 9.6 (tested with 9.6.7)
- Cabal >= 3.0

### Build

```sh
cabal build all
```

### Run tests

```sh
cabal test all
```

### Run

```sh
# Interactive mode with stub provider (default config; no API key or model server needed)
cabal run haskode

# Experimental minimal Brick TUI mode
cabal run haskode -- --tui

# Single-shot mode
cabal run haskode -- --prompt "What files are in the current directory?"

# With a real OpenAI-compatible provider
export OPENAI_API_KEY="sk-..."
cabal run haskode -- --provider openai --base-url https://api.openai.com --prompt "Say hello in one sentence."

# With Anthropic (Messages API, including streaming)
export ANTHROPIC_API_KEY="sk-ant-..."
cabal run haskode -- --provider anthropic --model claude-3-5-sonnet-latest --prompt "Say hello in one sentence."

# Override model and base URL from the command line
cabal run haskode -- --provider openai --model gpt-4o --base-url https://api.openai.com --prompt "Hello"

# Load one explicit config file
cabal run haskode -- --config ./haskode.json --prompt "Hello"

# Verbose mode (prints provider, model, base URL)
cabal run haskode -- --provider openai --base-url https://api.openai.com -v --prompt "Hello"

# Inspect session log (read-only, no replay)
cabal run haskode -- --show-session

# Resume conversation from session.jsonl
cabal run haskode -- --resume

# Resume with a real provider
cabal run haskode -- --provider openai --base-url https://api.openai.com --resume

# Show help
cabal run haskode -- --help
```

### Interactive commands

In interactive mode, the following slash commands are available:

| Command | Description |
|---------|-------------|
| `/help` | show this help |
| `/new` | start a fresh conversation |
| `/compact` | summarize and replace conversation context |
| `/status` | show current runtime/config status |
| `/doctor` | local diagnostic checks |
| `/exit` | save session log and exit |
| `/quit` | same as `/exit` |

Unknown slash commands print a short hint.  Anything without a leading `/` is sent to the agent as a normal prompt.
Forks may compile in additional pure text slash commands through
`compiledExtensions`; those commands appear in `/help` only when registered.

`/status` includes provider, model, working directory, streaming, resume
state, context usage, session-log limit, disabled tools, and available tools.
`/doctor` is local and read-only: it checks provider/config, API-key
presence, `SYSTEM.md`, `AGENTS.md`, disabled/available tools, and session-log
settings without contacting providers, running shell commands, or writing
files.

`/compact` is manual, provider-assisted context management.  It asks the
current provider for a compact working-memory summary with tools disabled,
shows the proposed memory, and asks for confirmation before replacing the
live conversation context.  It does not replay tools, does not advertise
tools, and rejects the draft if the provider somehow returns tool calls.
It is not automatic context management or full memory.

### Experimental TUI mode

`--tui` launches a minimal Brick-based interface. This is the first TUI
slice, not a replacement for the CLI. The screen has a transcript panel, a
single input line, and a small status/tool activity area. It uses the same
agent state, command registry, provider path, session log, and display-event
seam as the CLI.

Supported in TUI mode:

- Submit normal prompts, including the default local `stub` provider path.
- `/help`, `/status`, `/doctor`, `/new`, `/compact`, `/exit`, and `/quit`.
- Any compiled pure text extension commands registered in the final command
  registry.
- Basic display of assistant replies, tool notices, policy notices, and
  streaming chunks after the synchronous agent turn completes.
- Transcript scrolling with PageUp/PageDown.
- Input editing: Backspace to delete last character, Ctrl-W to delete the
  previous word, Ctrl-U to clear the entire input line.
- Long transcript entries and confirmation args are automatically truncated
  with clear length markers.
- Confirmation-required tool and policy actions show a focused Brick
  confirmation panel. The panel includes the tool name, reason, JSON
  arguments, and plain patch/write/batch preview text when available.
  Press `y` to approve; press `n`, `Esc`, `Enter`, or `Ctrl-C` to reject.
- Outside a confirmation panel, `Esc` or `Ctrl-C` exits the TUI cleanly.

Current TUI limitations:

- Provider calls run synchronously, so the screen does not redraw while a turn
  is in progress.
- Confirmation prompts are synchronous. The TUI opens a small focused panel
  during the agent turn rather than doing live background redraw.
- The input line has no visual cursor; Ctrl-A/E (home/end) are not supported.
- The CLI confirmation prompt and colored diff previews are unchanged.
- The TUI does not include file trees, tabs, workspace browsing, extension
  management UI, session browsing, a theme engine, or an async job dashboard.

### Configuration

Create a `haskode.json` in your project root:

```json
{
  "cfgProvider": {
    "pcProvider": "openai",
    "pcModel": "gpt-4o-mini",
    "pcBaseUrl": "https://api.openai.com",
    "pcApiKey": "$OPENAI_API_KEY"
  },
  "cfgMaxTokens": 4096,
  "cfgVerbose": false,
  "cfgWorkingDir": ".",
  "cfgMaxContextChars": 120000,
  "cfgMaxSessionLogBytes": 5242880,
  "cfgDisabledTools": []
}
```

Search order: `./haskode.json` -> `./haskode.jsonc` -> `~/.config/haskode/config.json` -> defaults.
The `.jsonc` filename is accepted for discovery, but config contents are
parsed as ordinary JSON; comments are not stripped.
Without a config file, the default provider is `stub` with model `stub`,
empty base URL, and no API key. This path is local and deterministic.

Pass `--config PATH` to load exactly one config file. When this flag is
present, a missing or malformed file is an error; Haskode does not fall back
to the normal search path or defaults.

Anthropic config example:

```json
{
  "cfgProvider": {
    "pcProvider": "anthropic",
    "pcModel": "claude-3-5-sonnet-latest",
    "pcBaseUrl": "",
    "pcApiKey": "$ANTHROPIC_API_KEY"
  },
  "cfgMaxTokens": 4096,
  "cfgVerbose": false,
  "cfgWorkingDir": ".",
  "cfgMaxContextChars": 120000,
  "cfgMaxSessionLogBytes": 5242880,
  "cfgDisabledTools": []
}
```

**Additional config fields** (config file only, no CLI override):

| Field | Type | Default | Description |
|---|---|---|---|
| `cfgMaxContextChars` | int | `120000` | Conservative context-window limit in characters (~30K tokens). When the estimated conversation size exceeds this, the provider call is blocked with a clear error. |
| `cfgMaxSessionLogBytes` | int | `5242880` | Maximum session.jsonl file size in bytes (5 MB). When the existing log exceeds this, it is rotated to `session.jsonl.1` before new events are appended. Set to `0` to disable rotation. |
| `cfgDisabledTools` | array of strings | `[]` | Compiled tool names to remove from the runtime tool registry. Unknown names are a startup error. |

**Disabling compiled tools.**  `cfgDisabledTools` is a plain list of tool
names compiled into Haskode, including built-ins and any locally registered
extension tools. The default is `[]`, which preserves the normal tool set.
This is only an enable/disable switch for tools already compiled into
Haskode; it is not a plugin system, executable-tool loader, or permission
framework.

Example: disable shell access and write-capable file tools:

```json
{
  "cfgProvider": {
    "pcProvider": "stub",
    "pcModel": "stub",
    "pcBaseUrl": "",
    "pcApiKey": ""
  },
  "cfgMaxTokens": 4096,
  "cfgVerbose": false,
  "cfgWorkingDir": ".",
  "cfgMaxContextChars": 120000,
  "cfgMaxSessionLogBytes": 5242880,
  "cfgDisabledTools": ["shell", "write_file", "apply_patch", "apply_patch_batch"]
}
```

Disabled tools are removed from the registry before provider setup. They are
not advertised in provider-native tool schemas or in the model-facing system
prompt. If a provider somehow returns a call for a disabled tool anyway,
Haskode cannot execute it because the tool is no longer in the registry; the
execution path reports it as an unknown or disabled tool. Misspelled or
unknown names in `cfgDisabledTools` fail startup clearly instead of being
ignored.

### Compiled extensions

Extensions are source-level Haskell additions compiled into Haskode.  They
can contribute tools to the same registry used by built-ins, policy rules
for those tools through the same policy gate, and pure text slash commands
through the shared command registry.  There is no runtime extension loader,
no config-based code loading, no external executable tool mechanism, and no
external command mechanism.  This is not a runtime plugin system.

To add a compiled extension in a fork:

1. Create a module (e.g. `MyExtension.hs`) that exports an `Extension`
   value with a unique name, optional `Tool`s, optional `Rule`s for those
   tools, and optional pure text `ExtensionCommand`s.
2. Import it in `src/Haskode/Extensions.hs` and add it to the
   `compiledExtensions` list.
3. Rebuild with `cabal build all`.

Duplicate extension names and duplicate tool names (across built-ins and
extensions) are rejected at startup with a clear error. Duplicate command
names and aliases (across built-ins, extension commands, and extension
aliases) are also rejected at startup.

Extension tools use the same provider advertisement, policy confirmation,
and session logging path as built-ins.  Extension policy rules are appended
after the built-in/default policy and are scoped to enabled tools contributed
by that extension.  This keeps built-in hard denials ahead of extension
rules, prevents disabled extension tools from carrying allow rules, and
leaves unknown/no-match calls on the existing `AskUser` fallback.
`cfgDisabledTools` applies to the final set of compiled tool names
(built-ins + extensions).

Extension commands are deliberately limited for now. They can return pure
text and may define aliases, but they cannot run IO, mutate `AgentState`,
inspect tools, call providers, exit or reset the session, add display hooks,
or load command code from config. CLI and TUI resolve them through the same
final command registry, and `/help` lists them only when they are compiled
into `compiledExtensions`.

**Provider names:** `openai`, `ollama`, `vllm`, `litellm`, `openrouter` (all
OpenAI-compatible HTTP endpoints), `anthropic` (Anthropic Messages API), or
`stub` (local echo for development). Any other name produces a clear error.

**Base URL convention:** OpenAI-compatible providers require `pcBaseUrl`
or `--base-url`. Haskode appends `/v1/chat/completions`, so set the value
to the API root without `/v1`:

| Provider   | Example `pcBaseUrl`                    |
|------------|----------------------------------------|
| OpenAI     | `https://api.openai.com`               |
| Ollama     | `http://localhost:11434`               |
| vLLM       | `http://localhost:8000`                |
| LiteLLM    | `http://localhost:4000`                |
| OpenRouter | `https://openrouter.ai/api`            |

OpenAI-compatible providers do not infer these roots when `pcBaseUrl` is
empty. Anthropic defaults to `https://api.anthropic.com` when `pcBaseUrl` is empty.
If you set `pcBaseUrl` for Anthropic, Haskode appends `/v1/messages`.

**API key resolution** (for OpenAI-compatible providers):

1. `--api-key` CLI flag (highest priority)
2. `OPENAI_API_KEY` environment variable
3. `pcApiKey` field in config file

`ollama` and `vllm` do not require an API key by default. If a key is
provided for either, Haskode sends it as a bearer token. `openai`,
`litellm`, and `openrouter` require a key.

**API key resolution** (for Anthropic):

1. `--api-key` CLI flag (highest priority)
2. `pcApiKey` field in config file
3. `ANTHROPIC_API_KEY` environment variable

**Environment variable expansion:** String fields in the config file
(`pcApiKey`, `pcBaseUrl`, `pcModel`, `pcProvider`, `cfgWorkingDir`) support
environment-variable references.  Syntax: `$VAR` or `${VAR}`.
Undefined variables expand to the empty string.

```json
{
  "cfgProvider": {
    "pcApiKey": "$OPENAI_API_KEY",
    "pcBaseUrl": "${MY_API_URL}"
  },
  "cfgWorkingDir": "$PROJECT_ROOT"
}
```

For hosted OpenAI-compatible providers, you may leave `pcApiKey` empty and
rely on `OPENAI_API_KEY` directly. For local `ollama` and `vllm`, you may
leave `pcApiKey` empty when the server does not require auth.
For Anthropic, leave `pcApiKey` empty and set `ANTHROPIC_API_KEY`.

**CLI flags override config values:**

| Flag             | Overrides      |
|------------------|----------------|
| `--provider`, `-P` | `pcProvider` |
| `--model`, `-m`    | `pcModel`    |
| `--api-key`        | `pcApiKey`   |
| `--base-url`       | `pcBaseUrl`  |
| `--verbose`, `-v`  | `cfgVerbose` |
| `--config`, `-c`    | config search path |
| `--show-session`   | *(n/a)*      |
| `--resume`         | *(n/a)*      |
| `--tui`            | launch experimental TUI mode |

## Smoke test with a real model

Verify that real providers work end-to-end:

```sh
# 1. Set your API key
export OPENAI_API_KEY="sk-..."

# 2. Single-shot prompt (should print a real response)
cabal run haskode -- --provider openai --base-url https://api.openai.com --prompt "Say hello in one sentence."

# 3. With Ollama (no API key needed)
cabal run haskode -- --provider ollama --model llama3.1 --base-url http://localhost:11434 --prompt "Say hello."

# 4. With Anthropic (Messages API, including streaming)
export ANTHROPIC_API_KEY="sk-ant-..."
cabal run haskode -- --provider anthropic --model claude-3-5-sonnet-latest --prompt "Say hello."

# 5. Interactive session with tools
cabal run haskode -- --provider openai --model gpt-4o-mini --base-url https://api.openai.com
# Then type: "List the files in the current directory"
# The agent should call list_files and report the results.

# 6. Interactive shell approval
# When the model calls the shell tool, the terminal prompts:
#   [confirm] Tool:     shell
#   [confirm] Args:     {"command":"ls -la"}
#   [confirm] Reason:   no policy rule matched; confirmation required
#   [confirm] Approve? (y/N)
# Type "y" to approve, anything else (or Enter) to reject.
```

If a required API key is missing, Haskode prints a clear error explaining the
three ways to provide one (CLI flag, env var, config file).

### What works today

Haskode performs a real multi-step tool loop with OpenAI-compatible
providers and Anthropic.  A typical session looks like:

1. User asks: *"List the files in this repo, read the Cabal file, and
   run the tests."*
2. The model calls `list_files` and `read_file` in one response.
3. Haskode executes the tools, appends results to the conversation.
4. The model sees the results, then calls `shell` with `cabal test all`.
5. Haskode's policy flags `shell` as `AskUser`; the terminal prompts
   for confirmation.
6. After approval, the shell runs and Haskode returns structured output
   (exit code, stdout, stderr) to the model.
7. The model summarizes the results.

Additional read-only tools: `glob` finds files by pattern (supports `*`
and `**` wildcards), and `search` does substring search across text
files, returning path:line:snippet results.  `search` defaults to the
whole directory tree and accepts an optional `directory` to scope the
search.  Matching is case-sensitive by default; pass `ignore_case: true`
for case-insensitive search.  Both tools skip `.git`, `dist-newstyle`,
and other build/cache directories automatically.  `search` also skips
files larger than 1 MB and reports how many were skipped.

Both `glob` and `search` also respect a `.agentignore` file in the
working directory root.  The syntax is deliberately small: blank lines
and `#` comments are ignored, and every other line is a pattern.  A
pattern without `/` (e.g. `build`, `*.log`) matches the entry name at
any depth; a pattern with `/` (e.g. `vendor/*`) is matched against the
full relative path using Haskode's `*` and `**` matcher.  Skipped entries
are reported in the traversal stats (e.g. `[skipped 2 by .agentignore]`).

### AGENTS.md — repo-level agent instructions

If a file named `AGENTS.md` exists in the working directory root, its
contents are included in the system prompt sent to the LLM on every
turn.  This is the standard convention for repository-level
instructions to coding agents (analogous to `.agentignore` for
traversal exclusions).  Use it to encode project-specific rules,
coding style, or workflow preferences that the agent should follow.

The file is read once per turn with the same symlink-safe
canonicalization used by `read_file` and `search` — it cannot escape
the working directory through symlinks or path tricks.  Files larger
than 32 KB are silently skipped.  If the file is missing or
unreadable, behavior is unchanged.

### SYSTEM.md — project-local system instructions

If a file named `SYSTEM.md` exists in the working directory root, its
contents are included in the system prompt sent to the LLM on every
turn.  Use it for project-local system instructions that shape how the
agent behaves in this specific project.

**How it differs from `AGENTS.md`:** `AGENTS.md` encodes repository
instructions (coding style, workflow preferences, repo conventions).
`SYSTEM.md` encodes project-local system instructions (behavioral
guidance, tone, scope boundaries).  When both files are present, they
appear in the system prompt in this order:

1. Built-in Haskode system prompt
2. Project `SYSTEM.md`
3. Project `AGENTS.md`

Both files are optional.  Either or both can be omitted without
affecting normal behavior.

The file uses the same symlink-safe canonicalization and 32 KB size
limit as `AGENTS.md`.  If the file is missing, unreadable, resolves
outside the working directory through symlinks, or exceeds the size
limit, it is silently skipped.

### preview_patch — diff preview without modification

The `preview_patch` tool lets the agent propose a file change and see
the unified diff **without modifying the filesystem**.  It accepts a
`path` (the existing file) and `replacement` (the proposed new
content), reads the current file using the same symlink-safe
containment pattern as `read_file`, and returns a diff via the
built-in `Haskode.Patch` machinery.

Key properties:

- **Read-only** — never writes to disk, no approval required.
- **Safe** — same `safeCanonicalize` + `isUnderRoot` guards as all
  other file tools.  Broken symlinks, missing files, and outside-root
  paths produce clear errors.
- **Conservative** — diffs larger than 8 KB are refused with a
  message suggesting the agent read the file and make the change
  manually, preventing context flooding.

### apply_patch — single-file confirmed patch application

The `apply_patch` tool writes replacement text to an existing file,
but only after the user confirms via the policy gate (`AskUser`).
It accepts the same `path` and `replacement` arguments as
`preview_patch`, enforces the same root-containment safety checks,
and returns the unified diff in the result so the user can see
exactly what changed.

When confirmation is requested, the agent displays the target file
path and a concise unified diff preview before the y/N prompt, so
the user can make an informed decision without running a separate
`preview_patch` call:

```
  [policy] Confirmation needed: apply_patch
  [confirm] File:     src/Lib.hs
  [confirm] Diff:
    --- src/Lib.hs
  +++ src/Lib.hs
  -old line
  +new line
  [confirm] Approve? (y/N)
```

Key properties:

- **Confirmed** — default policy requires user approval before
  writing.  The agent cannot silently modify files.  The confirmation
  prompt shows the target path and a unified diff preview.
- **Single-file only** — one file per call, no multi-file patches,
  no new-file creation, no deletion.
- **Safe** — same containment pattern as all other file tools.

Haskode supports single-file confirmed patch application, multi-file
batch patching via `preview_patch_batch` and `apply_patch_batch`,
a character-count context guard, and conservative session resume via
`--resume`.

### write_file — new-file creation with confirmation

The `write_file` tool creates a new file under the working directory
with the given content, but only after the user confirms via the policy
gate (`AskUser`).  It accepts `path` (the new file to create) and
`content` (the file content), enforces the same root-containment safety
checks as all other file tools, and returns a concise diff-like preview
in the result so the user can see exactly what was created.

Key properties:

- **Confirmed** — default policy requires user approval before
  writing.  The agent cannot silently create files.  The confirmation
  prompt shows the target path and a preview of the new content.
- **Create-only** — refuses to overwrite existing files.  If the
  target already exists (file or directory), the tool returns an error.
- **Parent must exist** — the parent directory must already exist;
  the tool does not create intermediate directories.
- **Safe** — same `safeCanonicalize` + `isUnderRoot` containment
  pattern as all other file tools.  Broken symlinks, outside-root
  paths, and directory targets produce clear errors.

When confirmation is requested, the agent displays the target file
path and a concise preview before the y/N prompt:

```
  [policy] Confirmation needed: write_file
  [confirm] File:     src/NewModule.hs
  [confirm] Preview:
    --- (new file)
  +++ src/NewModule.hs
  +module NewModule where
  +import Data.List
  +myFunc = sort
  [confirm] Approve? (y/N)
```

### preview_patch_batch — read-only batch diff preview

The `preview_patch_batch` tool lets the agent preview multiple file
changes (replacements and new-file creations) in one read-only call.
It accepts an `operations` array where each entry has an `op` tag
(`"replace"` or `"create"`), a `path`, and the appropriate content
field (`replacement` for replace, `content` for create).

Key properties:

- **Read-only** — never writes to disk, no approval required.
- **All-or-nothing validation** — every operation is validated before
  any preview is computed.  If any operation fails, the entire batch
  is rejected with a clear error.
- **Safe** — same `safeCanonicalize` + `isUnderRoot` containment
  pattern as all other file tools.  Replace targets must exist and
  be regular files; create targets must not exist and their parent
  directory must already exist.
- **Bounded output** — individual diffs are limited to 8 KB each;
  combined output is limited to 32 KB.  Oversized batches are
  rejected with a suggestion to split.

### apply_patch_batch — confirmed multi-file batch write

The `apply_patch_batch` tool applies multiple file changes (replacements
and new-file creations) in one confirmed call.  It accepts the same
`operations` array schema as `preview_patch_batch`.

Key properties:

- **Confirmed** — default policy requires user approval before
  writing.  The confirmation prompt shows a concise per-file summary
  and bounded diff previews for all operations.
- **All-or-nothing validation** — every operation is validated before
  any file is written.  If any operation fails validation, the entire
  batch is rejected and no files are modified.
- **Ordered execution** — after approval, operations are applied in
  array order.  If a write fails mid-batch, remaining operations are
  skipped and partial success is reported clearly.
- **No rollback** — if a write fails after partial application, already
  written files are not rolled back.  The result reports which
  operations succeeded and which failed.
- **Safe** — same `safeCanonicalize` + `isUnderRoot` containment
  pattern as all other file tools.  Replace targets must exist and
  be regular files; create targets must not exist and their parent
  directory must already exist.
- **Bounded output** — same per-file (8 KB) and combined (32 KB)
  limits as `preview_patch_batch`.

When confirmation is requested, the agent displays a concise summary
and diff previews before the y/N prompt:

```
  [policy] Confirmation needed: apply_patch_batch
  Batch: 3 operations
    1. replace src/Foo.hs
    2. replace src/Bar.hs
    3. create src/Baz.hs
    --- Operation 1 (replace): src/Foo.hs
    --- src/Foo.hs
    +++ src/Foo.hs
    -old
    +new
    ...
  [confirm] Approve? (y/N)
```

OpenAI-compatible providers use OpenAI's native `tool_calls` / `tool`
message format. Anthropic uses native `tool_use` / `tool_result` content
blocks. Neither path relies on JSON-in-text hacks.

### Session log / audit trail

Every agent run records a sequence of session events in memory.  The
log can be flushed to `session.jsonl` (one JSON object per line) for
debugging or inspection.

**What gets recorded:**

| Event type | When | Data payload |
|---|---|---|
| `session_start` | Interactive or single-shot run begins | `"session started"` |
| `session_end` | Run exits normally (before log flush) | `"session ended"` |
| `conversation_reset` | `/new` resets the in-memory conversation | `"conversation reset by /new"` |
| `conversation_compacted` | Accepted `/compact` replaces live context with compact memory | Compact memory text |
| `run_limit_reached` | A configured run-control limit stops a turn | Local limit message |
| `user_message` | User sends input | Raw user text |
| `assistant_reply` | Model replies | Reply text; when tool calls are present, a summary of call IDs and tool names is appended (e.g. `\| tool_calls: call_1=read_file, call_2=list_files`) |
| `tool_call` | Tool begins execution | Call ID, tool name, JSON arguments |
| `tool_result` | Tool finishes | Call ID, output text (or denial/error message) |
| `policy_decision` | Policy gate fires | Tool name and decision (`Allow`, `Deny`, `AskUser`); approval/rejection outcomes |

**What is NOT recorded:**

- API keys: they stay in the HTTP transport layer, never in event data
- System prompts: the system message is rebuilt each turn but not logged
- Raw provider request/response bodies: only the parsed summary is logged
- Standalone filesystem snapshots. Tool results are logged as returned, so a
  tool result can include file text that was read or generated during the run

**Example event (pretty-printed):**

```json
{
  "time": "2025-01-15T10:30:00Z",
  "type": "tool_result",
  "data": "tc-1 [exit] ExitSuccess\n[stdout]\nhello\n[stderr]\n"
}
```

The session log is purely in-memory during a run.  The log is flushed
to `session.jsonl` on normal exit (single-shot completion or typing
`/exit` in interactive mode) and when the agent encounters a handled
error (e.g. a provider failure).  The file is appended to — multiple
runs in the same directory accumulate events.  The JSONL file is a
write-only audit trail and cannot restore executable session state.  Lifecycle
events (`session_start`, `session_end`) mark run boundaries.  Resume uses
`conversation_reset` and `conversation_compacted` as text-context boundaries;
lifecycle events are never used for executable replay.

**Empty and command-only sessions.**  A session is flushed only when
it contains at least one *content* event (`user_message`,
`assistant_reply`, `tool_call`, `tool_result`, `policy_decision`, or
`conversation_compacted`, or `run_limit_reached`).
Sessions that contain only lifecycle events — such as an immediate
`/exit`, `/help` then `/exit`, `/status` then `/exit`, or `/new` then
`/exit` — are silently discarded and do not create `session.jsonl`.
This keeps the log free of noisy lifecycle-only entries.

**Log rotation.**  When the existing `session.jsonl` exceeds
`cfgMaxSessionLogBytes` (default 5 MB), it is rotated to
`session.jsonl.1` before new events are appended.  Only one backup
file is kept — a second rotation overwrites the previous `.1` file.
This keeps the active log bounded without losing recent history.
Set `cfgMaxSessionLogBytes` to `0` to disable rotation.

**Inspecting the session log.**  The `--show-session` flag prints a
concise summary of the current `session.jsonl` and exits (read-only,
no replay):

```
$ haskode --show-session
Session summary:
  Log:           /home/user/project/session.jsonl
  Total events:  13
  First event:   2025-06-06T10:30:00Z
  Last event:    2025-06-06T10:35:00Z
  user_message: 3
  assistant_reply: 3
  tool_call: 2
  tool_result: 2
  policy_decision: 2
  session_start: 1
  session_end: 0
  conversation_reset: 0
  conversation_compacted: 0
  run_limit_reached: 0
  Malformed:     0
  Backup:        (none)
```

The summary reports the inspected log path, total event count,
first/last timestamps, counts by event type, malformed line count
(always shown), and whether a rotated `session.jsonl.1` backup exists.
Only the active `session.jsonl` is inspected; any `.1` backup is
reported but not read.

**Resuming a session.**  The `--resume` flag loads safe text resume
context from `session.jsonl` without re-executing tools or provider
calls.  `user_message` and `assistant_reply` events after the last
`/new` or accepted `/compact` boundary are reconstructed.  An accepted
`conversation_compacted` event resumes as a single safe system-memory
message; older pre-compaction messages are not live context.  Tool calls,
tool results, policy decisions, and lifecycle events are skipped.  On
startup a concise summary is printed:

```
$ haskode --resume
Resumed from:   /home/user/project/session.jsonl
Messages:       6
Valid events:   14
Message events: 7
Skipped:        5
Malformed:      0
Reset boundary: yes
Compact boundary: no
```

If no `session.jsonl` exists, a "starting fresh" message is shown.
If the log exists but reconstructs zero messages, the summary is
printed and the session continues normally.  `/status` shows
`Resumed: yes/no`.  Full replay (re-running tools or provider calls)
is not implemented.

### Known limitations

- **Streaming providers** — OpenAI-compatible and Anthropic text deltas
  stream token-by-token to the terminal.  Tool-call deltas are assembled
  from fragments before execution.  Providers without streaming support
  fall back to the non-streaming `providerComplete` path.
- **Anthropic Messages API** — native Anthropic tool calls are supported
  through `tool_use` / `tool_result` blocks in both non-streaming and
  streaming responses.  Streamed `tool_use` input JSON fragments are
  assembled before tool execution.
- **Manual-only context compaction** — long conversations may exceed the
  model's context limit.  A conservative character-count guard
  (`cfgMaxContextChars`, default 120K chars / ~30K tokens) prevents
  oversized requests from reaching the provider and returns a clear
  error.  `/status` shows character-based context usage (current,
  max, remaining, percentage) so the user can monitor consumption.
  `/compact` can manually ask the current provider for a compact
  working-memory summary with tools disabled, then replace live context
  only after confirmation.  Haskode still has no automatic truncation,
  automatic summarization, token-aware pruning, vector memory, or
  automatic message deletion.
- **Limited session resume; no full replay** — `--resume` loads safe
  text resume context from `session.jsonl` (user messages, assistant
  replies, and accepted compact memory after the last `/new` or
  `/compact` boundary).  Tool calls, tool results, policy decisions,
  and lifecycle events are not reconstructed.  No tools or provider
  calls are re-executed.  Full replay (re-running tool calls or provider
  turns from the log) is not implemented.
- **Experimental TUI** - `--tui` provides a minimal Brick wrapper with a
  transcript, input line, status/tool area, and synchronous confirmation
  panel for confirmation-required tool and policy actions. It is still
  synchronous: provider calls do not redraw live, and streaming chunks appear
  after the running turn returns to Brick. The CLI remains the reference
  interface.
- **No batch rollback** — the agent can preview and apply
  multiple file changes via `preview_patch_batch` (read-only) and
  `apply_patch_batch` (confirmed write).  Rollback of batch operations
  is not implemented.
- **Shell output truncation** — stdout/stderr beyond 4096 characters
  is truncated with a metadata line reporting original length, returned
  length, and how many characters were dropped, e.g.
  `[truncated: returned 4096 of 5000 chars, 904 dropped]`.
- **Single model per session** — model cannot be changed mid-session.

## Roadmap

### 1.0 candidate scope

- [x] CLI-first coding loop
- [x] OpenAI-compatible provider
- [x] Anthropic provider
- [x] Native provider tool calls
- [x] Streaming text output
- [x] Read/search/glob/list tools
- [x] Shell tool with confirmation
- [x] Patch/write tools with confirmation
- [x] Session log, summary, and conservative resume
- [x] Manual `/compact`
- [x] Minimal experimental TUI
- [x] Compiled extension seam
- [ ] Basic inspectability polish
- [ ] Release docs: changelog, example config, security notes

### Post-1.0 candidates

- [ ] Conversation/session browsing
- [ ] Tool-call inspector
- [ ] Side-by-side diffs
- [ ] Packaging polish

### Intentionally out of core

- Runtime plugin marketplace
- Autonomous agent swarm
- Background job dashboard
- Hidden vector memory
- Full executable replay
- IDE workspace manager

## Philosophy

> Perfection is achieved, not when there is nothing more to add, but when
> there is nothing left to take away. — Antoine de Saint-Exupery

Haskode follows the suckless ethos: every line of code should earn its place.
If a feature can be a simple function rather than a framework, it is.
If a dependency can be avoided, it is. If a type can be a plain record
rather than a typeclass hierarchy, it is.

## Contributing

This is a fully open project. Read the code, learn from it, fork it,
make it your own. Pull requests welcome if they follow the existing
style: simple, readable, well-commented.

## License

0BSD — public domain equivalent. See [LICENSE](LICENSE).
