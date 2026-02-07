# Nota técnica — dynIRT_KD_fast (SQUAREM + Aitken)

## Objetivo
Acelerar a convergência do EM no IRT dinâmico K-dimensional usando extrapolação quadrática (SQUAREM) e um critério de parada de Aitken, sem alterar a solução-alvo do EM e mantendo a mesma interface do `dynIRT_KD()`.

## Desenho do acelerador
- Cada iteração do EM é encapsulada como um mapa de ponto fixo `theta_new = f(theta_old)`.
- O vetor `theta` concatena `alpha`, `beta` e `x` (achatado).
- A função `f(theta)` executa um ciclo completo: `da_step` → `kalman_smoother_country` → `m_step_items` (e `m_step_Omega` se habilitado).

## Controle do SQUAREM
- O SQUAREM é chamado com `squarem(maxiter = 1)` dentro do loop principal do EM, gerando uma extrapolação por iteração.
- Por padrão, `step.max0 = 4` permite extrapolação. Com `step.max0 = 1`, o SQUAREM não extrapola e não acelera.
- Os parâmetros podem ser ajustados via `.control$squarem_control`.

## Clamp e fallback
- Para estabilidade, aplica-se *clamp* conservador durante avaliações aceleradas: `|param| <= clamp_limit` (default 6).
- Se a extrapolação produzir valores não finitos ou reduzir a log-likelihood além de `ll_drop_tol` (default 1e-6), a iteração faz *fallback* para EM vanilla.
- Eventos de clamp e fallback são registrados em `runtime$events`.

## Parada por Aitken
- O critério de Aitken usa:
- `a_m = (ℓ_m - ℓ_{m-1}) / (ℓ_{m-1} - ℓ_{m-2})`
- `ℓ_inf = ℓ_{m-1} + (ℓ_m - ℓ_{m-1}) / (1 - a_m)`
- A execução para quando `|ℓ_inf - ℓ_m| < thresh_aitken` (default 1e-4).
- O índice de parada é registrado em `runtime$aitken_iter`.

## Parâmetros adicionais em .control
- `use_squarem` (default TRUE)
- `thresh_aitken` (default 1e-4)
- `clamp_limit` (default 6)
- `ll_drop_tol` (default 1e-6)
- `squarem_control` (lista; default `step.max0 = 4`, `mstep = 4`, `method = 3`)

## Limitações e observações
- Quando `estimate_omega = TRUE`, o SQUAREM é desativado para manter o mapa de ponto fixo bem-definido.
- O EM usado é uma aproximação do tipo *classification EM*; a log-likelihood observada tende a ser não-decrescente, mas pequenas flutuações podem ocorrer.
- Aitken é um critério heurístico; por isso o default é conservador.

## Saídas extras
- `runtime$em_evals`: número de avaliações de `f(theta)`.
- `runtime$events`: log de clamp/fallback e mudanças operacionais.
- `runtime$aitken_iter`: iteração de parada por Aitken.
