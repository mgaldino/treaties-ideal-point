# Tutorial: Claude Code como Orquestrador de Codex e Gemini CLI

**Data**: 2026-02-06
**Contexto**: Projeto Ideal Point Estimation for ILO

---

## 1. Por que orquestrar?

### Economia de tokens

Na abordagem manual (atual):
```
Você ↔ Claude: "O codex fez X, deu Y, o que acha?"  ← contexto cresce a cada mensagem
Você ↔ Codex: copiar prompt do Claude, colar resultado de volta
Você ↔ Gemini: idem
```
Cada ciclo adiciona centenas/milhares de tokens ao contexto do Claude (o plano mais escasso).

Na abordagem orquestrada:
```
Claude:  escreve prompt → arquivo no disco       (custo: 1 Write)
Claude:  codex exec < arquivo                    (custo: 1 Bash)
Claude:  tail logs/agent.log                     (custo: 1 Bash, leitura pontual)
Claude:  lê resultado final                      (custo: 1 Read)
```

**O contexto do Claude não cresce** com o trabalho dos agentes — os prompts vão para disco, a execução acontece fora, e só o resultado final entra no contexto. Estimativa: **3-5x menos tokens no Claude** para tarefas de coordenação.

### Quando vale a pena

| Cenário | Orquestrar? | Por quê |
|---------|-------------|---------|
| Tarefas independentes e bem definidas (V2, V3) | Sim | Despacha e monitora, zero contexto desperdiçado |
| Debugging interativo de um erro obscuro | Não | Melhor usar o agente diretamente |
| Análise exploratória / decisão arquitetural | Não | Requer seu julgamento humano no loop |
| Tarefas sequenciais com dependência (V3 estimação após V2) | Sim | Claude verifica V2, depois despacha V3 |

---

## 2. Pré-requisitos

```bash
# Verificar que os 3 CLIs estão instalados
which claude codex gemini

# Verificar autenticação
codex login       # deve mostrar que já está logado
# gemini: usa GEMINI_API_KEY ou gcloud auth
```

Versões testadas (fev/2026):
- `claude` (Anthropic CLI)
- `codex` (OpenAI Codex CLI) com subcomando `exec`
- `gemini` (Google Gemini CLI) com flag `-p`

---

## 3. Invocação não-interativa

### 3a. Codex

```bash
# Prompt curto (inline)
codex exec "Crie um arquivo hello.R que printa 'hello world'"

# Prompt longo (via arquivo)
codex exec < docs/v2_equivalence_prompt.md

# Prompt longo (via stdin com cat)
cat docs/v2_equivalence_prompt.md | codex exec

# Com opções úteis:
codex exec \
  -c 'model="o3"' \
  < docs/v2_equivalence_prompt.md \
  2>&1 | tee logs/codex_v2.log
```

**Flags importantes:**
| Flag | Efeito |
|------|--------|
| `--full-auto` | Auto-aprovar todas as ações (equivalente ao `--yolo` do Gemini) |
| `-c model="o3"` | Escolher modelo |
| `-c 'sandbox_permissions=["disk-full-read-access"]'` | Dar acesso total ao disco |
| `< arquivo.md` | Ler prompt de arquivo (evita problemas com aspas/escapes) |

### 3b. Gemini

```bash
# Prompt curto (inline)
gemini -p "Crie um arquivo hello.R que printa 'hello world'"

# Prompt longo (via flag -p com substituição)
gemini -p "$(cat docs/v3_data_prep_prompt.md)" --yolo -o text

# Via stdin
cat docs/v3_data_prep_prompt.md | gemini -p - --yolo -o text

# Com log:
gemini -p "$(cat docs/v3_data_prep_prompt.md)" \
  --yolo \
  -o text \
  2>&1 | tee logs/gemini_v3.log
```

**Flags importantes:**
| Flag | Efeito |
|------|--------|
| `-p "prompt"` | Modo não-interativo (headless) |
| `--yolo` | Auto-aprovar todas as ações (sem confirmação) |
| `--approval-mode yolo` | Equivalente a --yolo |
| `-o text` | Output em texto puro (sem JSON) |
| `-o json` | Output em JSON estruturado |
| `-m modelo` | Escolher modelo |

---

## 4. Execução em paralelo (via Claude Code)

Quando o Claude Code orquestra, ele lança os agentes em background e monitora:

### 4a. Despachar

```bash
# Lançar Codex (Agente 1) em background
codex exec < docs/v2_equivalence_prompt.md \
  2>&1 > logs/codex_v2.log &
CODEX_PID=$!

# Lançar Gemini (Agente 2) em background
gemini -p "$(cat docs/v3_data_prep_prompt.md)" --yolo -o text \
  2>&1 > logs/gemini_v3.log &
GEMINI_PID=$!

echo "Codex PID: $CODEX_PID, Gemini PID: $GEMINI_PID"
```

### 4b. Monitorar

```bash
# Verificar se ainda estão rodando
ps -p $CODEX_PID -o pid,etime,comm 2>/dev/null
ps -p $GEMINI_PID -o pid,etime,comm 2>/dev/null

# Ver últimas linhas do log
tail -20 logs/codex_v2.log
tail -20 logs/gemini_v3.log

# Verificar se outputs esperados foram criados
ls -la outputs/v2_equivalence/v2_results.rds 2>/dev/null
ls -la data/processed/us_congress_v3.rds 2>/dev/null
```

### 4c. Consolidar

Quando os processos terminam, o Claude Code:
1. Lê os resultados (`Read outputs/v2_equivalence/v2_results.rds`)
2. Avalia PASS/FAIL
3. Decide próximo passo (se V2 passou → despachar estimação V3)
4. Reporta ao usuário

---

## 5. Padrão de orquestração completo

### Fluxo tipo

```
[Claude Code]
  │
  ├── 1. Escrever prompt em arquivo (Write)
  │     docs/task_agent1.md
  │     docs/task_agent2.md
  │
  ├── 2. Despachar em paralelo (Bash, background)
  │     codex exec < docs/task_agent1.md &> logs/a1.log &
  │     gemini -p "$(cat docs/task_agent2.md)" --yolo &> logs/a2.log &
  │
  ├── 3. Monitorar (Bash, periódico)
  │     tail logs/a1.log
  │     ls outputs/expected_file.rds
  │
  ├── 4. Ler resultados (Read)
  │     Read outputs/a1_result.rds
  │     Read data/processed/a2_output.rds
  │
  ├── 5. Avaliar e decidir (raciocínio interno)
  │     "V2 PASSED → despachar V3 estimação"
  │     "V2 FAILED → investigar, ajustar prompt, re-despachar"
  │
  └── 6. Reportar ao usuário
        "V2 passou (r=0.997). V3 data prep concluído. Estimação V3 despachada."
```

### Tokens consumidos no Claude Code

| Etapa | Tokens estimados |
|-------|-----------------|
| Write prompt (1x por agente) | ~200 (tool call, não entra no contexto) |
| Bash despacho | ~50 |
| Bash monitoramento (3-4 checks) | ~200 |
| Read resultado | ~300 |
| Raciocínio + report | ~500 |
| **Total por ciclo de 2 agentes** | **~1.300 tokens** |

Comparado com a abordagem manual onde cada mensagem do usuário reportando resultados adiciona ~500-1000 tokens ao contexto, e cada resposta do Claude analisando adiciona ~1000-2000, um ciclo manual custa **~3.000-5.000 tokens**. A orquestração economiza **~60-70%**.

---

## 6. Troubleshooting

### Agente trava (sem output por > 5 min)

```bash
# Verificar se o processo ainda existe
ps -p $PID

# Ver se está esperando input (bug no prompt)
tail -50 logs/agent.log

# Matar e re-despachar
kill $PID
# Ajustar prompt e re-lançar
```

### Agente falha (erro no log)

```bash
# Ler o erro
grep -i "error\|fail\|stop" logs/agent.log | tail -10

# Causas comuns:
# - Pacote R não instalado → adicionar install.packages() ao prompt
# - Arquivo não encontrado → verificar working directory
# - Timeout de rede → prompt deve tratar fallback
```

### Conflito de arquivos entre agentes

Regra fundamental: **cada agente escreve em pastas diferentes**. Definir isso explicitamente no prompt:

```
# No prompt do Agente 1:
"Salve outputs em outputs/v2_equivalence/. NÃO modifique nenhum arquivo fora desta pasta."

# No prompt do Agente 2:
"Salve outputs em data/processed/ e scripts/R/v3_*. NÃO modifique nenhum outro arquivo."
```

### Prompt muito longo para argumento de linha de comando

Se o prompt tiver > 100KB (raro), o shell pode truncar. Usar stdin:

```bash
cat docs/long_prompt.md | codex exec
cat docs/long_prompt.md | gemini -p - --yolo
```

---

## 7. Template: prompt de orquestração

Ao escrever um prompt para um agente executar via CLI, incluir sempre:

```markdown
# Tarefa: [título curto]

## Objetivo
[1-2 frases]

## Pastas de trabalho
- Criar arquivos em: [pasta]
- NÃO modificar: [lista de arquivos/pastas protegidos]

## Passos
[numerados, com código completo]

## Validações
[checklist de verificação antes de terminar]

## Saídas esperadas
[lista de arquivos que devem existir ao final]

## Se falhar
[instruções de fallback — NÃO ficar em loop]
```

---

## 8. Limitações conhecidas

| Limitação | Workaround |
|-----------|------------|
| Codex sandbox tem 1 core | Tarefas CPU-intensivas melhor no Gemini ou local |
| Codex sandbox pode não ter rede | Dados grandes devem já estar no repo |
| Gemini `--yolo` aprova TUDO | Cuidado com prompts que podem deletar arquivos |
| Codex `--full-auto` aprova TUDO | Mesma cautela que `--yolo` |
| Prompts longos podem ser truncados via `-p "..."` | Usar stdin (`cat file | gemini -p -`) |
| Agentes não se comunicam entre si | Claude Code é o hub central; comunicação via filesystem |
| Claude Code não vê output em tempo real dos agentes | Monitorar via `tail` periódico nos logs |

---

## 9. Exemplo Concreto: V5 Per-Domain 2D Estimation

### Contexto

O script `scripts/R/v5_per_domain_2d.R` roda `dynIRT_KD(K=2)` em domínios de tratados. Aceita nomes de domínios como argumentos de linha de comando. Dividimos 6 domínios entre 2 agentes.

### Arquivos de prompt

- `docs/v5_prompt_codex.md` — Batch 1: investment, human_rights, arms_control
- `docs/v5_prompt_gemini.md` — Batch 2: security, environment, intellectual_property

### Despacho

```bash
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"

# Agente 1: Codex
codex exec --full-auto - < docs/v5_prompt_codex.md > logs/codex_v5.log 2>&1 &
PID_CODEX=$!

# Agente 2: Gemini
gemini -p "$(cat docs/v5_prompt_gemini.md)" --yolo > logs/gemini_v5.log 2>&1 &
PID_GEMINI=$!

echo "Despachados: Codex=$PID_CODEX, Gemini=$PID_GEMINI"
wait
echo "Ambos finalizaram."
```

### Saídas esperadas

| Arquivo | Conteúdo |
|---------|----------|
| `outputs/v5_per_domain_2d/{domain}_2d_results.rds` | Ideal points (N×K×T), item params, anchors |
| `logs/v5_per_domain_2d_*.log` | Log do Rscript (1 por batch) |
| `logs/codex_v5.log` | Log do agente Codex |
| `logs/gemini_v5.log` | Log do agente Gemini |

### Monitoramento

```bash
# Ver progresso do R (cada domínio imprime header e checkfreq a cada 25 iters)
tail -5 logs/v5_per_domain_2d_*.log

# Ver se o agente ainda está ativo
ps -p $PID_CODEX $PID_GEMINI -o pid,etime,comm 2>/dev/null

# Verificar outputs produzidos
ls -la outputs/v5_per_domain_2d/
```
