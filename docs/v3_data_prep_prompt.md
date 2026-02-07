# Prompt para Agente 2 — V3 Data Prep: US Congress (Voteview)

## Objetivo

Preparar os dados do US Congress para a Phase V3 do plano de verificação (benchmark externo contra DW-NOMINATE). Este agente faz **apenas preparação de dados** — a estimação será rodada depois, quando V2 passar.

**Referência**: `docs/estimation_plan_2d.md`, Section 15 → Phase V3 (linhas 943-1018).

## Pastas de trabalho

- **Dados brutos**: salvar em `data/raw/us_congress/`
- **Dados processados**: salvar em `data/processed/`
- **Script**: salvar em `scripts/R/v3_prepare_congress.R`
- **Log**: salvar em `logs/v3_data_prep.log`

Criar os diretórios se não existirem:
```bash
mkdir -p data/raw/us_congress
mkdir -p outputs/v3_congress
```

## Arquivos que NÃO devem ser modificados

Nenhum arquivo existente no repositório deve ser modificado. Este agente só **cria** arquivos novos.

---

## Passo 1: Baixar dados do Voteview

Baixar dois CSVs do Voteview (UCLA NOMINATE project):

```bash
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"

# Roll call votes (~500MB, pode demorar)
curl -L -o data/raw/us_congress/HSall_votes.csv \
  "https://voteview.com/static/data/out/votes/HSall_votes.csv"

# Member info + DW-NOMINATE scores (~30MB)
curl -L -o data/raw/us_congress/HSall_members.csv \
  "https://voteview.com/static/data/out/members/HSall_members.csv"
```

**Se curl falhar** (sandbox sem rede): reportar ao usuário e parar. O usuário pode baixar manualmente e colocar em `data/raw/us_congress/`.

**Verificação**: confirmar que os arquivos existem e têm tamanho razoável:
```bash
ls -lh data/raw/us_congress/
# HSall_votes.csv deve ter > 100MB
# HSall_members.csv deve ter > 5MB
```

### Estrutura esperada dos CSVs

**HSall_votes.csv** — uma linha por voto de cada legislador em cada roll call:
| Coluna | Tipo | Descrição |
|--------|------|-----------|
| `congress` | int | Número do Congress (e.g., 105) |
| `chamber` | str | "House" ou "Senate" |
| `rollnumber` | int | Número do roll call dentro do Congress |
| `icpsr` | int | ID único do legislador (ICPSR) |
| `cast_code` | int | Código do voto (1-3 = Yea, 4-6 = Nay, 7-9 = Missing/Abstain/Not voting) |

**HSall_members.csv** — uma linha por legislador-Congress:
| Coluna | Tipo | Descrição |
|--------|------|-----------|
| `congress` | int | Número do Congress |
| `chamber` | str | "House" ou "Senate" |
| `icpsr` | int | ID único do legislador |
| `bioname` | str | Nome do legislador |
| `party_code` | int | Código do partido (100=Dem, 200=Rep, outros=terceiros) |
| `nominate_dim1` | float | DW-NOMINATE 1a dimensão |
| `nominate_dim2` | float | DW-NOMINATE 2a dimensão |

---

## Passo 2: Selecionar Congresses e filtrar

Criar `scripts/R/v3_prepare_congress.R`:

### 2a. Carregar e filtrar

```r
# ---- Configuração ----
CONGRESSES <- 105:112   # 105th (1997-98) a 112th (2011-12), T = 8
CHAMBER <- "Senate"      # Senate é menor (~100 membros), mais tratável
MIN_CONGRESSES <- 2      # Legislador deve servir em >= 2 Congresses

cat(sprintf("Selecionando %s, Congresses %d-%d (T=%d)\n",
            CHAMBER, min(CONGRESSES), max(CONGRESSES), length(CONGRESSES)))

# Carregar dados
votes_raw <- read.csv("data/raw/us_congress/HSall_votes.csv",
                       stringsAsFactors = FALSE)
members_raw <- read.csv("data/raw/us_congress/HSall_members.csv",
                          stringsAsFactors = FALSE)

# Filtrar por chamber e congresses
votes <- votes_raw[votes_raw$congress %in% CONGRESSES &
                    votes_raw$chamber == CHAMBER, ]
members <- members_raw[members_raw$congress %in% CONGRESSES &
                        members_raw$chamber == CHAMBER, ]

cat(sprintf("Votes after filter: %d rows\n", nrow(votes)))
cat(sprintf("Members after filter: %d rows (legislator-congress pairs)\n", nrow(members)))
```

### 2b. Recodificar votos

```r
# cast_code: 1-3 = Yea → +1, 4-6 = Nay → -1, 7-9 = Missing → 0
votes$vote <- ifelse(votes$cast_code %in% 1:3, 1L,
              ifelse(votes$cast_code %in% 4:6, -1L, 0L))
```

### 2c. Identificar legisladores com >= 2 Congresses

```r
# Contar Congresses por legislador
legis_congresses <- tapply(members$congress, members$icpsr,
                            function(x) length(unique(x)))
keep_icpsr <- as.integer(names(legis_congresses[legis_congresses >= MIN_CONGRESSES]))
cat(sprintf("Legislators serving in >= %d Congresses: %d (of %d total)\n",
            MIN_CONGRESSES, length(keep_icpsr), length(unique(members$icpsr))))

# Filtrar votes e members
votes <- votes[votes$icpsr %in% keep_icpsr, ]
members <- members[members$icpsr %in% keep_icpsr, ]
```

### 2d. Construir a matrix rc (N legislators × J roll calls)

```r
# Unique legislators and roll calls
legislators <- sort(unique(votes$icpsr))
N <- length(legislators)
leg_idx <- setNames(seq_len(N), as.character(legislators))

# Roll calls: unique (congress, rollnumber) pairs
rollcalls <- unique(votes[, c("congress", "rollnumber")])
rollcalls <- rollcalls[order(rollcalls$congress, rollcalls$rollnumber), ]
J <- nrow(rollcalls)
rc_idx <- seq_len(J)

cat(sprintf("Building rc matrix: N=%d legislators × J=%d roll calls\n", N, J))

# Inicializar com 0 (missing)
rc <- matrix(0L, nrow = N, ncol = J)

# Criar lookup: para cada roll call, qual é o índice j?
rollcalls$j <- seq_len(J)
votes <- merge(votes, rollcalls[, c("congress", "rollnumber", "j")],
               by = c("congress", "rollnumber"))

# Preencher a matrix
for (row in seq_len(nrow(votes))) {
  i <- leg_idx[as.character(votes$icpsr[row])]
  j <- votes$j[row]
  v <- votes$vote[row]
  if (!is.na(i) && !is.na(j) && v != 0L) {
    rc[i, j] <- v
  }
}
# NOTA: esta implementação com loop é lenta para milhões de votos.
# Alternativa vetorizada (preferida):
#   i_vec <- leg_idx[as.character(votes$icpsr)]
#   j_vec <- votes$j
#   v_vec <- votes$vote
#   valid <- !is.na(i_vec) & !is.na(j_vec) & v_vec != 0L
#   rc[cbind(i_vec[valid], j_vec[valid])] <- v_vec[valid]
# Use a versão vetorizada! A versão com loop está aqui só para clareza.

cat(sprintf("rc sparsity (missing): %.1f%%\n", mean(rc == 0) * 100))
```

**IMPORTANTE**: Use a versão vetorizada (comentada acima), não o loop. O loop em milhões de linhas será muito lento.

### 2e. Construir vetores auxiliares

```r
T_periods <- length(CONGRESSES)

# bill.session[j]: 0-indexed Congress index for each roll call
# Congress 105 → 0, 106 → 1, ..., 112 → 7
congress_to_idx <- setNames(seq_along(CONGRESSES) - 1L, as.character(CONGRESSES))
bill_session <- as.integer(congress_to_idx[as.character(rollcalls$congress)])

# startlegis[i]: first Congress (0-indexed) in which legislator i served
# endlegis[i]: last Congress (0-indexed)
startlegis <- integer(N)
endlegis   <- integer(N)
for (idx in seq_len(N)) {
  icpsr_i <- legislators[idx]
  congs_i <- members$congress[members$icpsr == icpsr_i]
  startlegis[idx] <- congress_to_idx[as.character(min(congs_i))]
  endlegis[idx]   <- congress_to_idx[as.character(max(congs_i))]
}

cat(sprintf("T=%d periods, startlegis range: [%d, %d], endlegis range: [%d, %d]\n",
            T_periods, min(startlegis), max(startlegis), min(endlegis), max(endlegis)))
```

---

## Passo 3: Selecionar 3 anchors para K=2

Precisamos de 3 legisladores cujas posições 2D span R² (não colineares). Usar os scores DW-NOMINATE como guia.

```r
# Agregar DW-NOMINATE scores por legislador (média ao longo dos Congresses)
dw_scores <- aggregate(
  cbind(nominate_dim1, nominate_dim2) ~ icpsr + bioname + party_code,
  data = members,
  FUN = mean, na.rm = TRUE
)

# Filtrar para legisladores no nosso dataset
dw_scores <- dw_scores[dw_scores$icpsr %in% legislators, ]
dw_scores$idx <- leg_idx[as.character(dw_scores$icpsr)]

cat(sprintf("DW-NOMINATE scores available for %d of %d legislators\n",
            nrow(dw_scores), N))

# ---- Seleção de anchors ----
# a0: Liberal forte (dim1 muito negativo no DW-NOMINATE)
# a1: Conservador forte (dim1 muito positivo)
# a2: Off-diagonal (dim2 extremo, para resolver a rotação)

# Candidatos para a0 (liberal):
# Ted Kennedy (ICPSR 14226), Patrick Leahy (ICPSR 14441),
# Barbara Boxer (ICPSR 40105)
# Candidatos para a1 (conservador):
# Jim DeMint (ICPSR 20534), Tom Coburn (ICPSR 29731),
# Jeff Sessions (ICPSR 29544)
# Candidatos para a2 (off-diagonal):
# Buscar legislador com |dim2| alto e |dim1| moderado

# Estratégia automatizada: selecionar pelos scores
a0_row <- dw_scores[which.min(dw_scores$nominate_dim1), ]
a1_row <- dw_scores[which.max(dw_scores$nominate_dim1), ]

# Para a2: maximizar |dim2| entre os que têm |dim1| < mediana
median_dim1 <- median(abs(dw_scores$nominate_dim1), na.rm = TRUE)
moderate_dim1 <- dw_scores[abs(dw_scores$nominate_dim1) < median_dim1, ]
a2_row <- moderate_dim1[which.max(abs(moderate_dim1$nominate_dim2)), ]

cat(sprintf("\nAnchor a0 (liberal):      %s (ICPSR %d), DW=(%+.3f, %+.3f)\n",
            a0_row$bioname, a0_row$icpsr, a0_row$nominate_dim1, a0_row$nominate_dim2))
cat(sprintf("Anchor a1 (conservative): %s (ICPSR %d), DW=(%+.3f, %+.3f)\n",
            a1_row$bioname, a1_row$icpsr, a1_row$nominate_dim1, a1_row$nominate_dim2))
cat(sprintf("Anchor a2 (off-diagonal): %s (ICPSR %d), DW=(%+.3f, %+.3f)\n",
            a2_row$bioname, a2_row$icpsr, a2_row$nominate_dim1, a2_row$nominate_dim2))

# ---- Definir posições-alvo dos anchors no nosso sistema de coordenadas ----
# Convenção: escalar DW-NOMINATE para ±2
# dim1: liberal = +2, conservative = -2 (invertemos o sinal do DW-NOMINATE,
#   onde liberal é negativo; OU mantemos a convenção DW. Manter DW é mais simples.)
#
# DECISÃO: Manter a convenção DW-NOMINATE (liberal = negativo, conservative = positivo).
# As posições-alvo são baseadas nos scores DW reais, escalados a ±2:

scale_to_range <- function(x, target_range = c(-2, 2)) {
  x_range <- range(x, na.rm = TRUE)
  (x - x_range[1]) / diff(x_range) * diff(target_range) + target_range[1]
}

# Escalar ambas dimensões para [-2, +2]
all_dim1_scaled <- scale_to_range(dw_scores$nominate_dim1)
all_dim2_scaled <- scale_to_range(dw_scores$nominate_dim2)

anchor_positions <- matrix(NA_real_, nrow = 3, ncol = 2)
anchor_icpsr <- c(a0_row$icpsr, a1_row$icpsr, a2_row$icpsr)
anchor_names <- c(a0_row$bioname, a1_row$bioname, a2_row$bioname)
anchor_leg_idx <- leg_idx[as.character(anchor_icpsr)]

for (a in 1:3) {
  row_in_dw <- which(dw_scores$icpsr == anchor_icpsr[a])
  anchor_positions[a, 1] <- all_dim1_scaled[row_in_dw]
  anchor_positions[a, 2] <- all_dim2_scaled[row_in_dw]
}

cat("\nAnchor positions (scaled to [-2, +2]):\n")
for (a in 1:3) {
  cat(sprintf("  a%d: %s → (%+.3f, %+.3f)\n",
              a - 1, anchor_names[a], anchor_positions[a, 1], anchor_positions[a, 2]))
}

# ---- Verificar não-colinearidade ----
# Área do triângulo formado pelos 3 anchors
area_triangle <- 0.5 * abs(
  anchor_positions[1,1] * (anchor_positions[2,2] - anchor_positions[3,2]) +
  anchor_positions[2,1] * (anchor_positions[3,2] - anchor_positions[1,2]) +
  anchor_positions[3,1] * (anchor_positions[1,2] - anchor_positions[2,2])
)
cat(sprintf("\nTriangle area: %.3f (must be > 0 for non-collinearity)\n", area_triangle))
stopifnot(area_triangle > 0.1)  # margem de segurança
```

---

## Passo 4: Montar a lista final e salvar

```r
# ---- Construir DW-NOMINATE benchmark (para comparação futura em V3.5) ----
# Matrix N×2 com scores DW-NOMINATE médios por legislador (na mesma ordem que rc)
dw_benchmark <- matrix(NA_real_, nrow = N, ncol = 2)
for (idx in seq_len(N)) {
  row_in_dw <- which(dw_scores$icpsr == legislators[idx])
  if (length(row_in_dw) == 1) {
    dw_benchmark[idx, 1] <- dw_scores$nominate_dim1[row_in_dw]
    dw_benchmark[idx, 2] <- dw_scores$nominate_dim2[row_in_dw]
  }
}
colnames(dw_benchmark) <- c("nominate_dim1", "nominate_dim2")

cat(sprintf("DW-NOMINATE benchmark: %d of %d legislators have scores\n",
            sum(complete.cases(dw_benchmark)), N))

# ---- Lookup para nomes ----
legislator_names <- character(N)
for (idx in seq_len(N)) {
  row_in_members <- which(members$icpsr == legislators[idx])[1]
  legislator_names[idx] <- members$bioname[row_in_members]
}

party_codes <- integer(N)
for (idx in seq_len(N)) {
  # Partido mais frequente deste legislador
  row_in_members <- which(members$icpsr == legislators[idx])
  party_codes[idx] <- as.integer(names(sort(table(members$party_code[row_in_members]),
                                             decreasing = TRUE))[1])
}

# ---- Salvar ----
congress_data <- list(
  # Dados para dynIRT_KD
  rc           = rc,
  startlegis   = as.integer(startlegis),
  endlegis     = as.integer(endlegis),
  bill.session = as.integer(bill_session),
  T            = T_periods,

  # Metadados
  icpsr            = legislators,
  legislator_names = legislator_names,
  party_codes      = party_codes,
  congresses       = CONGRESSES,
  chamber          = CHAMBER,

  # Anchors para K=2
  anchor_icpsr     = anchor_icpsr,
  anchor_names     = anchor_names,
  anchor_positions = anchor_positions,  # 3×2 matrix
  anchor_leg_idx   = anchor_leg_idx,    # posições no rc (row indices)

  # Benchmark DW-NOMINATE
  dw_benchmark     = dw_benchmark       # N×2 matrix
)

saveRDS(congress_data, "data/processed/us_congress_v3.rds")
cat(sprintf("\nSaved to data/processed/us_congress_v3.rds\n"))

# ---- Resumo final ----
cat("\n===== RESUMO =====\n")
cat(sprintf("Chamber: %s\n", CHAMBER))
cat(sprintf("Congresses: %d-%d (T=%d)\n", min(CONGRESSES), max(CONGRESSES), T_periods))
cat(sprintf("Legislators (>= %d Congresses): N=%d\n", MIN_CONGRESSES, N))
cat(sprintf("Roll calls: J=%d\n", J))
cat(sprintf("rc sparsity: %.1f%%\n", mean(rc == 0) * 100))
cat(sprintf("Anchors:\n"))
for (a in 1:3) {
  cat(sprintf("  a%d: %s (row %d) → (%+.3f, %+.3f)\n",
              a - 1, anchor_names[a], anchor_leg_idx[a],
              anchor_positions[a, 1], anchor_positions[a, 2]))
}
cat(sprintf("Triangle area: %.3f\n", area_triangle))
cat(sprintf("DW-NOMINATE benchmark: %d legislators with scores\n",
            sum(complete.cases(dw_benchmark))))
cat("\nData prep complete. Ready for estimation after V2 passes.\n")
```

---

## Instrução de execução

```bash
cd "/Users/manoelgaldino/Documents/DCP/Papers/Ideal point"
Rscript scripts/R/v3_prepare_congress.R 2>&1 | tee logs/v3_data_prep.log
```

---

## Validações que o agente deve fazer

Antes de salvar, confirmar:

| Check | Critério | Ação se falhar |
|-------|----------|----------------|
| `rc` tem dimensões razoáveis | N > 50, J > 500 | Verificar filtros |
| Sparsity < 80% | Muitos zeros = pouco sinal | Aumentar range de Congresses |
| Todos os legislators têm >= 1 voto | `all(rowSums(rc != 0) > 0)` | Remover linhas vazias |
| Todos os roll calls têm >= 1 voto | `all(colSums(rc != 0) > 0)` | Remover colunas vazias |
| `bill.session` range = [0, T-1] | Consistência com T | Bug na indexação |
| `startlegis <= endlegis` para todos | Lógica temporal | Bug na construção |
| Triangle area > 0.1 | Anchors não-colineares | Escolher anchor a2 diferente |
| DW-NOMINATE disponível para > 90% dos legislators | Benchmark utilizável | OK se > 80%, warn se < 80% |

---

## Saídas esperadas

| Arquivo | Descrição |
|---------|-----------|
| `data/raw/us_congress/HSall_votes.csv` | Dados brutos de votos |
| `data/raw/us_congress/HSall_members.csv` | Dados brutos de membros + DW-NOMINATE |
| `data/processed/us_congress_v3.rds` | Lista R com rc, startlegis, endlegis, bill.session, T, anchors, dw_benchmark |
| `scripts/R/v3_prepare_congress.R` | Script de preparação |
| `logs/v3_data_prep.log` | Log de execução |

---

## Q&A

### Q1: Por que Senate e não House?
O Senate tem ~100 membros por Congress, gerando uma rc matrix de ~150-200 legislators × ~2000-4000 roll calls. A House tem ~435 membros e ~1000+ roll calls por Congress, gerando uma matrix muito maior (~600 × ~5000+). Para V3 (validação do algoritmo), o Senate é suficiente e mais rápido. Pode-se rodar House depois como robustez.

### Q2: O HSall_votes.csv é muito grande. Posso filtrar durante o download?
Não — o Voteview serve o CSV completo. Baixe tudo e filtre em R. O arquivo cabe em memória (~500MB → ~2GB em R como data.frame).

### Q3: Devo instalar pacotes?
Não. O script usa apenas base R (`read.csv`, `merge`, `tapply`, `aggregate`). Nenhum pacote extra necessário para a preparação de dados.

### Q4: E se um anchor candidato (e.g., Ted Kennedy) não estiver nos Congresses selecionados?
A seleção automatizada (min/max de `nominate_dim1`, max |`nominate_dim2`|) escolhe entre os legisladores presentes no dataset. Não depende de nomes específicos. Se quiser forçar um legislador específico, verificar se o ICPSR está em `legislators`.

### Q5: E se o download falhar?
Se `curl` falhar, tentar:
```bash
wget -O data/raw/us_congress/HSall_votes.csv \
  "https://voteview.com/static/data/out/votes/HSall_votes.csv"
```
Se ambos falharem, reportar ao usuário — provavelmente o sandbox não tem acesso à rede.

### Q6: O output `us_congress_v3.rds` será usado diretamente por `dynIRT_KD`?
Sim. A lista contém `rc`, `startlegis`, `endlegis`, `bill.session`, `T` — exatamente o formato que `dynIRT_KD(.data = ...)` espera. Os anchors e o benchmark são metadados para o script de estimação (que será escrito depois).

### Q7: Devo rodar a estimação?
**Não.** Este prompt é só preparação de dados. A estimação depende de V2 ter passado.

### Q8: Posso usar `readr::read_csv()` em vez de `read.csv()`?
Pode, mas `read.csv()` evita dependências extras. Se o CSV for muito lento para carregar, `data.table::fread()` é mais rápido, mas adiciona dependência. Priorize `read.csv()`.

### Q9: A inversão de convenção no dim1 (liberal=negativo no DW-NOMINATE) importa?
Não para V3. A comparação com DW-NOMINATE usa Procrustes rotation (V3.5), que absorve reflexões. Mantenha a convenção DW-NOMINATE original nos dados.
