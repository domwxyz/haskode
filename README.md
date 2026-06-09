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
├── haskode.cabal          Build configuration
├── app/Main.hs            CLI entry point (optparse-applicative)
├── src/Haskode/
│   ├── Core.hs            Core data types (Role, Message, ToolCall, etc.)
│   ├── Config.hs          Config loading (haskode.json, defaults)
│   ├── Provider.hs        LLM provider interface + stub provider
│   ├── Tools.hs           Tool registry + built-in tools
│   ├── Policy.hs          Permission gate (allow/deny/ask)
│   ├── Session.hs         Session event log (JSONL audit trail)
│   ├── Patch.hs           File patch manager + diff display
│   └── Agent.hs           The agent loop (conversation state machine)
└── test/Spec.hs           Test suite (no external framework needed)
```

### Module responsibilities

| Module | Purpose |
|---|---|
| `Haskode.Core` | Shared vocabulary: `Role`, `Message`, `ToolCall`, `ToolResult`, `Conversation` |
| `Haskode.Config` | Load `haskode.json` / `haskode.jsonc` / `~/.config/haskode/config.json`; fall back to defaults |
| `Haskode.Provider` | Record-of-functions interface for LLM backends. Ships `stubProvider` (echo) and `scriptedProvider` (test replay) |
| `Haskode.Provider.OpenAI` | Real OpenAI-compatible HTTP provider (`/v1/chat/completions`). Works with OpenAI, Ollama, vLLM, LiteLLM, OpenRouter |
| `Haskode.Tools` | `Tool` type + `ToolRegistry` map. Built-in: `read_file`, `list_files`, `shell`, `glob`, `search`, `preview_patch`, `apply_patch`, `write_file` |
| `Haskode.Policy` | Composable rules: `Allow`, `Deny`, or `AskUser`. Default policy blocks `rm -rf /` |
| `Haskode.Session` | In-memory event log flushed to `session.jsonl` for audit trail |
| `Haskode.Patch` | Represent file changes, show unified diffs, apply/rollback |
| `Haskode.Agent` | The conversation state machine. Calls provider, processes tool calls, loops |

### Design principles

1. **Record-of-functions over typeclasses** — providers and tools are plain records, easy to swap at runtime and mock in tests
2. **Text, not String** — strict `Text` everywhere for performance
3. **JSON on the wire** — aeson instances follow the OpenAI chat-completion format
4. **Conservative policy** — unknown tool calls require user confirmation by default
5. **No framework** — just Cabal, just Haskell. No Template Haskell, no lens, no effect systems

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
# Interactive mode with stub provider (default config)
cabal run haskode

# Single-shot mode
cabal run haskode -- --prompt "What files are in the current directory?"

# With a real OpenAI-compatible provider
export OPENAI_API_KEY="sk-..."
cabal run haskode -- --provider openai --prompt "Say hello in one sentence."

# Override model and base URL from the command line
cabal run haskode -- --provider openai --model gpt-4o --prompt "Hello"

# Verbose mode (prints provider, model, base URL)
cabal run haskode -- --provider openai -v --prompt "Hello"

# Inspect session log (read-only, no replay)
cabal run haskode -- --show-session

# Show help
cabal run haskode -- --help
```

### Interactive commands

In interactive mode, the following slash commands are available:

| Command | Description |
|---------|-------------|
| `/help` | Show available commands |
| `/new` | Start a fresh conversation context |
| `/status` | Show current provider, model, working dir, and runtime info |
| `/exit` | Save session log and exit |
| `/quit` | Same as `/exit` |

Unknown slash commands print a short hint.  Anything without a leading `/` is sent to the agent as a normal prompt.

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
  "cfgMaxContextChars": 120000
}
```

Search order: `./haskode.json` → `./haskode.jsonc` → `~/.config/haskode/config.json` → defaults.

**Additional config fields** (config file only, no CLI override):

| Field | Type | Default | Description |
|---|---|---|---|
| `cfgMaxContextChars` | int | `120000` | Conservative context-window limit in characters (~30K tokens). When the estimated conversation size exceeds this, the provider call is blocked with a clear error. |
| `cfgMaxSessionLogBytes` | int | `5242880` | Maximum session.jsonl file size in bytes (5 MB). When the existing log exceeds this, it is rotated to `session.jsonl.1` before new events are appended. Set to `0` to disable rotation. |

**Provider names:** `openai`, `ollama`, `vllm`, `litellm`, `openrouter` (all
OpenAI-compatible HTTP endpoints), or `stub` (local echo for development).
Any other name produces a clear error.

**Base URL convention:** The provider appends `/v1/chat/completions` to
`pcBaseUrl`, so set it to the API root without `/v1`:

| Provider   | Example `pcBaseUrl`                    |
|------------|----------------------------------------|
| OpenAI     | `https://api.openai.com`               |
| Ollama     | `http://localhost:11434`               |
| vLLM       | `http://localhost:8000`                |
| LiteLLM    | `http://localhost:4000`                |
| OpenRouter | `https://openrouter.ai/api`            |

**API key resolution** (for OpenAI-compatible providers):

1. `--api-key` CLI flag (highest priority)
2. `OPENAI_API_KEY` environment variable
3. `pcApiKey` field in config file

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

You may also leave `pcApiKey` empty and rely on `OPENAI_API_KEY` directly
(the provider checks the environment variable when the config field is empty).

**CLI flags override config values:**

| Flag             | Overrides      |
|------------------|----------------|
| `--provider`, `-P` | `pcProvider` |
| `--model`, `-m`    | `pcModel`    |
| `--api-key`        | `pcApiKey`   |
| `--base-url`       | `pcBaseUrl`  |
| `--verbose`, `-v`  | `cfgVerbose` |
| `--show-session`   | *(n/a)*      |

## Smoke test with a real model

Verify that the OpenAI-compatible provider works end-to-end:

```sh
# 1. Set your API key
export OPENAI_API_KEY="sk-..."

# 2. Single-shot prompt (should print a real response)
cabal run haskode -- --provider openai --prompt "Say hello in one sentence."

# 3. With Ollama (no API key needed)
cabal run haskode -- --provider ollama --model llama3.1 --prompt "Say hello."

# 4. Interactive session with tools
cabal run haskode -- --provider openai --model gpt-4o-mini
# Then type: "List the files in the current directory"
# The agent should call list_files and report the results.

# 5. Interactive shell approval
# When the model calls the shell tool, the terminal prompts:
#   [confirm] Tool:     shell
#   [confirm] Args:     {"command":"ls -la"}
#   [confirm] Reason:   no policy rule matched; confirmation required
#   [confirm] Approve? (y/N)
# Type "y" to approve, anything else (or Enter) to reject.
```

If the API key is missing, Haskode prints a clear error explaining the
three ways to provide one (env var, config file, CLI flag).

### What works today

Haskode performs a real multi-step tool loop with OpenAI-compatible
providers.  A typical session looks like:

1. User asks: *"List the files in this repo, read the Cabal file, and
   run the tests."*
2. The model calls `list_files` and `read_file` (possibly in parallel).
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
working directory root.  The syntax is the same as `.gitignore` in
spirit: blank lines and `#` comments are ignored, and every other line
is a pattern.  A pattern without `/` (e.g. `build`, `*.log`) matches
the entry name at any depth; a pattern with `/` (e.g. `vendor/*`) is
matched against the full relative path.  Skipped entries are reported
in the traversal stats (e.g. `[skipped 2 by .agentignore]`).

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

Haskode now supports single-file confirmed patch application.  Multi-file
patch batching, session resume, and context window management are not
yet implemented.

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

All of this uses OpenAI's native `tool_calls` / `tool` message format
— no JSON-in-text hacks.

### Session log / audit trail

Every agent run records a sequence of session events in memory.  The
log can be flushed to `session.jsonl` (one JSON object per line) for
debugging, inspection, or fine-tuning data export.

**What gets recorded:**

| Event type | When | Data payload |
|---|---|---|
| `session_start` | Interactive or single-shot run begins | `"session started"` |
| `session_end` | Run exits normally (before log flush) | `"session ended"` |
| `conversation_reset` | `/new` resets the in-memory conversation | `"conversation reset by /new"` |
| `user_message` | User sends input | Raw user text |
| `assistant_reply` | Model replies | Reply text; when tool calls are present, a summary of call IDs and tool names is appended (e.g. `\| tool_calls: call_1=read_file, call_2=list_files`) |
| `tool_call` | Tool begins execution | Call ID, tool name, JSON arguments |
| `tool_result` | Tool finishes | Call ID, output text (or denial/error message) |
| `policy_decision` | Policy gate fires | Tool name and decision (`Allow`, `Deny`, `AskUser`); approval/rejection outcomes |

**What is NOT recorded:**

- API keys — they stay in the HTTP transport layer, never in event data
- System prompts — the system message is rebuilt each turn but not logged
- Raw provider request/response bodies — only the parsed summary is logged
- File contents on disk — `session.jsonl` is written once on CLI exit, not during the run

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
write-only audit trail and cannot restore sessions.  Lifecycle events
(`session_start`, `session_end`, `conversation_reset`) mark the
boundaries of each run and `/new` resets but are not used for replay
or resume.

**Empty and command-only sessions.**  A session is flushed only when
it contains at least one *content* event (`user_message`,
`assistant_reply`, `tool_call`, `tool_result`, or `policy_decision`).
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
  Total events:  12
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
```

The summary reports total event count, first/last timestamps, and
counts by event type.  Malformed lines (if any) are counted rather
than causing a crash.  Only the active `session.jsonl` is inspected;
any rotated `.1` backup is ignored.  This is inspection-only
groundwork — conversation restoration, replay, and provider/tool
re-execution are not implemented.

### Known limitations (Phase 1)

- **Streaming for OpenAI-compatible providers** — text deltas stream
  token-by-token to the terminal.  Tool-call deltas are assembled
  from fragments before execution.  Providers without streaming
  support fall back to the non-streaming `providerComplete` path.
- **No context window management** — long conversations may exceed the
  model's context limit.  A conservative character-count guard
  (`cfgMaxContextChars`, default 120K chars / ~30K tokens) prevents
  oversized requests from reaching the provider and returns a clear
  error.  No truncation, summarization, or automatic message deletion
  is performed — the user must start a new session to continue.
- **No session resume/replay** — the session log is write-only.
  Log rotation (`cfgMaxSessionLogBytes`) keeps the file bounded, and
  `--show-session` provides read-only inspection (event counts,
  timestamps, type breakdown), but there is no mechanism to load
  previous events back into a session or replay a conversation.
- **No multi-file patch batching** — the agent can apply patches to
  one file at a time via `apply_patch` and create new files via
  `write_file`, but cannot batch multiple file changes in one call.
- **Shell output truncation** — stdout/stderr beyond 4096 characters
  is truncated with a metadata line reporting original length, returned
  length, and how many characters were dropped, e.g.
  `[truncated: returned 4096 of 5000 chars, 904 dropped]`.
- **Single model per session** — model cannot be changed mid-session.

## Roadmap

### Phase 0 — Scaffold (this commit)
- [x] Project structure, Cabal build, test suite
- [x] Core data types with JSON instances
- [x] Config loading with defaults
- [x] Provider interface + stub provider
- [x] Tool registry + 3 built-in tools (stubs)
- [x] Policy gate with default rules
- [x] Session event log
- [x] Patch manager with diff display
- [x] Agent loop (single-turn, no tool execution)
- [x] CLI with interactive + single-shot modes

### Phase 1 — Working agent
- [x] OpenAI-compatible provider (real HTTP, non-streaming)
- [x] Provider selection from config / CLI
- [x] Helpful errors for missing API key / unknown provider
- [x] Tool execution in the agent loop (multi-step)
- [x] Native OpenAI tool_calls / tool message format
- [x] System prompt construction with tool schemas
- [x] Interactive policy confirmation (y/n prompts for shell)
- [x] Shell output with section markers and truncation metadata
- [x] `max_completion_tokens` for OpenAI, `max_tokens` for local providers
- [ ] Anthropic provider
- [x] `write_file` tool with patch integration
- [x] `search` / `glob` tools
- [ ] Token counting and context window management

### Phase 2 — Usable daily driver
- [x] Streaming token output
- [ ] Rich diff display (colored, side-by-side)
- [ ] Session save/resume
- [x] `.agentignore` file support (root-level, shared by `glob` and `search`)
- [ ] Multi-file patch batching
- [x] Environment variable expansion in config

### Phase 3 — TUI & polish
- [ ] Brick-based terminal UI
- [ ] Conversation history browsing
- [ ] Tool call inspector
- [ ] Config TUI editor
- [ ] Plugin system for custom tools

### Phase 4 — Advanced
- [ ] Multi-agent orchestration
- [ ] Fine-tuning data export from session logs
- [ ] Local model integration (llama.cpp / GGUF)
- [ ] MCP server support

## Philosophy

> Perfection is achieved, not when there is nothing more to add, but when
> there is nothing left to take away. — Antoine de Saint-Exupery

Haskode follows the suckless ethos: every line of code should earn its place.
If a feature can be a simple function rather than a framework, it is.
If a dependency can be avoided, it is. If a type can be a plain record
rather than a typeclass hierarchy, it is.

## Contributing

This is an educational project. Read the code, learn from it, fork it,
make it your own. Pull requests welcome if they follow the existing
style: simple, readable, well-commented.

## License

0BSD — public domain equivalent. See [LICENSE](LICENSE).
