# V3 Data Prep: US Congress (Voteview) — Senate, Congresses 105-112
# Fonte: https://voteview.com (UCLA NOMINATE project)
# Acesso: registrado em tempo de execução

suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Pacote 'dplyr' é necessário. Instale com install.packages('dplyr').")
  }
})

# ---- Configuração ----
CONGRESSES <- 105:112   # 105th (1997-98) a 112th (2011-12), T = 8
CHAMBER <- "Senate"      # Senate é menor (~100 membros)
MIN_CONGRESSES <- 2      # Legislador deve servir em >= 2 Congresses

cat(sprintf("Selecionando %s, Congresses %d-%d (T=%d)\n",
            CHAMBER, min(CONGRESSES), max(CONGRESSES), length(CONGRESSES)))
cat(sprintf("Fonte: voteview.com | Acesso: %s\n", as.character(Sys.Date())))

# ---- Carregar dados ----
votes_raw <- read.csv("data/raw/us_congress/HSall_votes.csv",
                      stringsAsFactors = FALSE)
members_raw <- read.csv("data/raw/us_congress/HSall_members.csv",
                        stringsAsFactors = FALSE)

# ---- Filtrar por chamber e congresses ----
votes <- votes_raw[votes_raw$congress %in% CONGRESSES &
                     votes_raw$chamber == CHAMBER, ]
members <- members_raw[members_raw$congress %in% CONGRESSES &
                         members_raw$chamber == CHAMBER, ]

cat(sprintf("Votes after filter: %d rows\n", nrow(votes)))
cat(sprintf("Members after filter: %d rows (legislator-congress pairs)\n", nrow(members)))

# ---- Recodificar votos ----
# cast_code: 1-3 = Yea → +1, 4-6 = Nay → -1, 7-9 = Missing → 0
votes$vote <- ifelse(votes$cast_code %in% 1:3, 1L,
              ifelse(votes$cast_code %in% 4:6, -1L, 0L))

# ---- Legisladores com >= 2 Congresses ----
legis_congresses <- tapply(members$congress, members$icpsr,
                           function(x) length(unique(x)))
keep_icpsr <- as.integer(names(legis_congresses[legis_congresses >= MIN_CONGRESSES]))
cat(sprintf("Legislators serving in >= %d Congresses: %d (of %d total)\n",
            MIN_CONGRESSES, length(keep_icpsr), length(unique(members$icpsr))))

votes <- votes[votes$icpsr %in% keep_icpsr, ]
members <- members[members$icpsr %in% keep_icpsr, ]

# ---- Construir a matrix rc (N x J) ----
legislators <- sort(unique(votes$icpsr))
N <- length(legislators)
leg_idx <- setNames(seq_len(N), as.character(legislators))

# Roll calls: unique (congress, rollnumber) pairs
rollcalls <- unique(dplyr::select(votes, congress, rollnumber))
rollcalls <- rollcalls[order(rollcalls$congress, rollcalls$rollnumber), ]
J <- nrow(rollcalls)

cat(sprintf("Building rc matrix: N=%d legislators × J=%d roll calls\n", N, J))

rc <- matrix(0L, nrow = N, ncol = J)

# Criar lookup para roll calls
rollcalls$j <- seq_len(J)
votes <- merge(votes, dplyr::select(rollcalls, congress, rollnumber, j),
               by = c("congress", "rollnumber"))

# Preencher a matrix (vetorizado)
i_vec <- leg_idx[as.character(votes$icpsr)]
j_vec <- votes$j
v_vec <- votes$vote
valid <- !is.na(i_vec) & !is.na(j_vec) & v_vec != 0L
rc[cbind(i_vec[valid], j_vec[valid])] <- v_vec[valid]

cat(sprintf("rc sparsity (missing): %.1f%%\n", mean(rc == 0) * 100))

# ---- Vetores auxiliares ----
T_periods <- length(CONGRESSES)

congress_to_idx <- setNames(seq_along(CONGRESSES) - 1L, as.character(CONGRESSES))
bill_session <- as.integer(congress_to_idx[as.character(rollcalls$congress)])

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

# ---- Selecionar 3 anchors para K=2 ----
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

# Estratégia automatizada: extremos de dim1 e off-diagonal em dim2
#a0 liberal (dim1 mínimo), a1 conservador (dim1 máximo), a2 off-diagonal
 a0_row <- dw_scores[which.min(dw_scores$nominate_dim1), ]
 a1_row <- dw_scores[which.max(dw_scores$nominate_dim1), ]

median_dim1 <- median(abs(dw_scores$nominate_dim1), na.rm = TRUE)
moderate_dim1 <- dw_scores[abs(dw_scores$nominate_dim1) < median_dim1, ]
a2_row <- moderate_dim1[which.max(abs(moderate_dim1$nominate_dim2)), ]

cat(sprintf("\nAnchor a0 (liberal):      %s (ICPSR %d), DW=(%+.3f, %+.3f)\n",
            a0_row$bioname, a0_row$icpsr, a0_row$nominate_dim1, a0_row$nominate_dim2))
cat(sprintf("Anchor a1 (conservative): %s (ICPSR %d), DW=(%+.3f, %+.3f)\n",
            a1_row$bioname, a1_row$icpsr, a1_row$nominate_dim1, a1_row$nominate_dim2))
cat(sprintf("Anchor a2 (off-diagonal): %s (ICPSR %d), DW=(%+.3f, %+.3f)\n",
            a2_row$bioname, a2_row$icpsr, a2_row$nominate_dim1, a2_row$nominate_dim2))

scale_to_range <- function(x, target_range = c(-2, 2)) {
  x_range <- range(x, na.rm = TRUE)
  (x - x_range[1]) / diff(x_range) * diff(target_range) + target_range[1]
}

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
area_triangle <- 0.5 * abs(
  anchor_positions[1,1] * (anchor_positions[2,2] - anchor_positions[3,2]) +
  anchor_positions[2,1] * (anchor_positions[3,2] - anchor_positions[1,2]) +
  anchor_positions[3,1] * (anchor_positions[1,2] - anchor_positions[2,2])
)
cat(sprintf("\nTriangle area: %.3f (must be > 0 for non-collinearity)\n", area_triangle))
stopifnot(area_triangle > 0.1)

# ---- Benchmark DW-NOMINATE (para comparação futura) ----
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

# ---- Lookup para nomes e partidos ----
legislator_names <- character(N)
for (idx in seq_len(N)) {
  row_in_members <- which(members$icpsr == legislators[idx])[1]
  legislator_names[idx] <- members$bioname[row_in_members]
}

party_codes <- integer(N)
for (idx in seq_len(N)) {
  row_in_members <- which(members$icpsr == legislators[idx])
  party_codes[idx] <- as.integer(names(sort(table(members$party_code[row_in_members]),
                                            decreasing = TRUE))[1])
}

# ---- Validações ----
if (N <= 50) stop("N muito pequeno após filtros.")
if (J <= 500) stop("J muito pequeno após filtros.")
if (mean(rc == 0) >= 0.8) warning("Sparsity >= 80%: possível pouco sinal.")
if (!all(rowSums(rc != 0) > 0)) stop("Há legisladores sem votos.")
if (!all(colSums(rc != 0) > 0)) stop("Há roll calls sem votos.")
if (min(bill_session) != 0L || max(bill_session) != (T_periods - 1L)) {
  stop("bill.session fora do intervalo esperado.")
}
if (!all(startlegis <= endlegis)) stop("startlegis > endlegis detectado.")

# ---- Salvar ----
congress_data <- list(
  rc           = rc,
  startlegis   = as.integer(startlegis),
  endlegis     = as.integer(endlegis),
  bill.session = as.integer(bill_session),
  T            = T_periods,

  icpsr            = legislators,
  legislator_names = legislator_names,
  party_codes      = party_codes,
  congresses       = CONGRESSES,
  chamber          = CHAMBER,

  anchor_icpsr     = anchor_icpsr,
  anchor_names     = anchor_names,
  anchor_positions = anchor_positions,
  anchor_leg_idx   = anchor_leg_idx,

  dw_benchmark     = dw_benchmark
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
cat("Anchors:\n")
for (a in 1:3) {
  cat(sprintf("  a%d: %s (row %d) → (%+.3f, %+.3f)\n",
              a - 1, anchor_names[a], anchor_leg_idx[a],
              anchor_positions[a, 1], anchor_positions[a, 2]))
}
cat(sprintf("Triangle area: %.3f\n", area_triangle))
cat(sprintf("DW-NOMINATE benchmark: %d legislators with scores\n",
            sum(complete.cases(dw_benchmark))))
cat("\nData prep complete. Ready for estimation after V2 passes.\n")
