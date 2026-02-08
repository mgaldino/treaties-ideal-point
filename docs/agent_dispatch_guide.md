# Agent Dispatch Guide

How to dispatch Codex and Gemini agents from Claude Code via bash.

## Codex CLI

**Version**: 0.98.0
**Default model**: `gpt-5.2-codex`
**Auth**: `~/.codex/auth.json`
**Project trust**: "Ideal point" is trusted in `~/.codex/config.toml`

### Non-interactive execution

```bash
# Short prompt
codex exec --full-auto \
  -C "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point" \
  -o /path/to/output.txt \
  "PROMPT TEXT"

# Long prompt from file (use - to read stdin)
cat docs/prompts/PROMPT_FILE.md | codex exec --full-auto \
  -C "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point" \
  -o /path/to/output.txt \
  -

# With specific model
codex exec --full-auto \
  -m gpt-5.2-codex \
  -C "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point" \
  "PROMPT TEXT"
```

### Key flags
- `--full-auto`: Sandboxed auto-execution (approves tool calls, workspace-write sandbox)
- `-C DIR`: Working directory
- `-o FILE`: Save last agent message to file
- `-m MODEL`: Override model (default: gpt-5.2-codex)
- `--json`: Output events as JSONL (for programmatic parsing)

## Gemini CLI

**Version**: 0.27.3
**Default model**: (auto-selected by Google)
**Auth**: OAuth personal (`~/.gemini/oauth_creds.json`)

### Non-interactive execution

```bash
# Short prompt
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point" && \
  gemini -p "PROMPT TEXT" -m gemini-3-pro-preview --yolo -o text

# Long prompt from file
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point" && \
  gemini -p "$(cat docs/prompts/PROMPT_FILE.md)" -m gemini-3-pro-preview --yolo -o text
```

### Key flags
- `-p "PROMPT"`: Non-interactive (headless) mode
- `-m MODEL`: Model selection (use `gemini-3-pro-preview`)
- `--yolo` or `--approval-mode yolo`: Auto-approve all tool calls
- `-o text`: Output format (text or json)

## Dispatch Pattern (from Claude Code)

To run agents in the background from Claude Code:

```bash
# Codex — run in background, capture output
codex exec --full-auto \
  -C "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point" \
  -o "logs/codex_TASK_output.txt" \
  "PROMPT" &

# Gemini — run in background, redirect output
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point" && \
  gemini -p "PROMPT" -m gemini-3-pro-preview --yolo -o text \
  > "logs/gemini_TASK_output.txt" 2>&1 &
```

## Notes

- Codex has ~10x more quota than Gemini → use Codex for most tasks
- Gemini `gemini-3-pro-preview` may hit rate limits; default model works as fallback
- Both CLIs have agentic capabilities (read/write files, run commands)
- Long prompts: prefer piping from file over inline strings to avoid shell escaping issues
