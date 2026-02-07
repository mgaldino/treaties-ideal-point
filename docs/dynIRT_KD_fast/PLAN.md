# Plano: dynIRT_KD_fast (SQUAREM + Aitken)

## Objetivo
Criar uma versão alternativa do entry point do EM dinâmico K-dimensional com aceleração SQUAREM e critério de parada de Aitken, sem modificar os arquivos originais.

## Fases

### Fase 0 — Preparação
- Criar pasta de trabalho `docs/dynIRT_KD_fast/`.
- Registrar este plano.

### Fase 1 — Leitura e mapeamento do código existente
- Inspecionar os arquivos:
  - `scripts/R/da_step.R`
  - `scripts/R/kalman.R`
  - `scripts/R/m_step.R`
  - `scripts/R/dynIRT_KD.R`
- Mapear:
  - Interface do `dynIRT_KD()`
  - Estrutura de dados esperada (`.data`, `.starts`, `.priors`, `.control`)
  - Funções e retornos de `da_step`, `kalman`, `m_step`
  - Cálculo de log-likelihood usado no EM

### Fase 2 — Implementação do `dynIRT_KD_fast()`
- Criar `scripts/R/dynIRT_KD_fast.R` com a mesma interface e formato de retorno.
- Adicionar `.control` com defaults:
  - `use_squarem = TRUE`
  - `thresh_aitken = 1e-4`
- Implementar empacotamento e desempacotamento de parâmetros `theta`:
  - `alpha` (J)
  - `beta` (J × K)
  - `x` (N × K × T)
- Implementar função `f(theta)` que faz 1 ciclo completo do EM:
  - `da_step` → `kalman_smoother` por país → `m_step`
- Integrar `SQUAREM::squarem()` quando `use_squarem=TRUE`.
- Implementar clamp e fallback para EM vanilla, documentando critérios.
- Implementar critério de parada de Aitken sobre log-likelihood.

### Fase 3 — Testes
- Criar `tests/test_dynIRT_KD_fast.R`.
- Incluir `simulate_test_data()` fornecida.
- Garantir cobertura dos critérios A–E:
  - Equivalência (correlações > 0.99)
  - Speedup (menos avaliações de f(theta) ou menor tempo; alvo 2x)
  - Qualidade (log-lik >= vanilla - tol; recovery >= 0.93/0.90)
  - Robustez (3 seeds; K=1 e K=2)
  - Aitken (para antes do maxit sem piorar recovery)

### Fase 4 — Execução e validação
- Rodar os testes de forma extensiva.
- Se speedup < 2x, documentar causas e sugerir alternativa.
- Ajustar implementação se necessário.

### Fase 5 — Nota técnica
- Escrever nota técnica em `docs/dynIRT_KD_fast/nota_tecnica.md`.
- Documentar: SQUAREM, clamp/fallback, Aitken, controle de extrapolação e limites.

### Fase 6 — Validação em dados reais (environment, fast)
- Criar script `scripts/R/dynIRT_KD_fast/run_real_validation.R`.
- Rodar `dynIRT_KD_fast()` para a área `environment` (K=1).
- Salvar resultados em `outputs/dynIRT_KD_fast/` (ideal points, item params, runtime).

## Critérios de conclusão
- Arquivos novos criados conforme pedido.
- Testes passam e demonstram speedup e equivalência.
- Registro no `HANDOFF.md` após cada fase concluída.
