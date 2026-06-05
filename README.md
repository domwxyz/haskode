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
| `Haskode.Tools` | `Tool` type + `ToolRegistry` map. Built-in: `read_file`, `list_files`, `shell` |
| `Haskode.Policy` | Composable rules: `Allow`, `Deny`, or `AskUser`. Default policy blocks `rm -rf /` |
| `Haskode.Session` | In-memory event log flushed to `session.jsonl` for audit/replay |
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
cabal test
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

# Show help
cabal run haskode -- --help
```

### Configuration

Create a `haskode.json` in your project root:

```json
{
  "cfgProvider": {
    "pcProvider": "openai",
    "pcModel": "gpt-4o-mini",
    "pcBaseUrl": "https://api.openai.com",
    "pcApiKey": ""
  },
  "cfgMaxTokens": 4096,
  "cfgVerbose": false,
  "cfgWorkingDir": "."
}
```

Search order: `./haskode.json` → `./haskode.jsonc` → `~/.config/haskode/config.json` → defaults.

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

**CLI flags override config values:**

| Flag             | Overrides      |
|------------------|----------------|
| `--provider`, `-P` | `pcProvider` |
| `--model`, `-m`    | `pcModel`    |
| `--api-key`        | `pcApiKey`   |
| `--base-url`       | `pcBaseUrl`  |
| `--verbose`, `-v`  | `cfgVerbose` |

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
```

If the API key is missing, Haskode prints a clear error explaining the
three ways to provide one (env var, config file, CLI flag).

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
- [ ] Anthropic provider
- [ ] Tool execution in the agent loop
- [ ] JSON argument extraction for tools
- [ ] `write_file` tool with patch integration
- [ ] `search` / `glob` tools
- [ ] Token counting and context window management
- [ ] System prompt construction with tool schemas

### Phase 2 — Usable daily driver
- [ ] Streaming token output
- [ ] Rich diff display (colored, side-by-side)
- [ ] Interactive policy confirmation (y/n prompts)
- [ ] Session save/resume
- [ ] `.haskodeignore` file support
- [ ] Multi-file patch batching
- [ ] Environment variable expansion in config

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
