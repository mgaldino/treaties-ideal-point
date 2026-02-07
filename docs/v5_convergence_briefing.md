# Briefing: V5 Convergência — 3 Domínios Não-Convergidos

## Situação

Rodamos `dynIRT_KD(K=2)` em 6 domínios de tratados internacionais. 3 convergiram, 3 não:

| Domínio | J (items) | Conv | Iters | Tempo | ΔLL/iter (final) |
|---------|-----------|------|-------|-------|-------------------|
| investment | 2,629 | **NÃO** | 500 | 111s | ~0.95 |
| security | 862 | **NÃO** | 500 | 39s | ~0.16 |
| environment | 1,849 | **NÃO** | 500 | 81s | ~2.31 |
| human_rights | 35 | SIM | 68 | 3s | — |
| arms_control | 26 | SIM | 119 | 6s | — |
| intellectual_property | 26 | SIM | 108 | 4s | — |

Os 3 que falharam são exatamente os domínios com muitos itens (J > 800).

## O que já sabemos (NÃO redescobrir)

1. **Convergência lenta de EM com data augmentation é esperada**, não é bug. Documentado em `docs/v2_response_to_reviews.md` (Seção 2.2, Ponto 1). Van Dyk & Meng (2001) mostram que EM com augmented data converge como O(1/√iter) quando a fração de dados latentes é grande.

2. **A LL é monotonicamente crescente** em todos os 3 domínios — o EM está correto.

3. **O Aitken extrapolation dá estimativas inconsistentes** (LL_inf < LL atual) porque o regime de convergência ainda não é geométrico nos 3 domínios. O `thresh_aitken=1e-4` não dispara.

4. **O thresh de parâmetro (delta)** mede `max|θ_new - θ_old|` e está em ~1e-2, longe do thresh=1e-4.

## Parâmetros de controle relevantes (em dynIRT_KD.R)

```r
.control = list(
  thresh         = 1e-4,      # param change threshold (delta)
  maxit          = 500L,       # máximo de iterações
  thresh_aitken  = 1e-4,      # Aitken early stopping
  checkfreq      = 25L,       # frequência de impressão
  estimate_omega = FALSE,      # Ω fixo em 0.1*I_K
  ncores         = 4L          # cores para Kalman paralelo
)
```

## Opções a investigar

1. **Aumentar maxit** (1500? 2000?) — abordagem bruta, pode funcionar para security
2. **Relaxar thresh_aitken** para 1e-3 ou 1e-2 — aceitar convergência prática
3. **Verificar estabilidade dos ideal points** — talvez os x_i já estejam estáveis mesmo com LL ainda subindo. Comparar x[iter=250] vs x[iter=500] (requer modificar o script para salvar snapshots)
4. **Relaxar thresh (delta)** para 1e-3 — a mudança de parâmetro pode estar dominada por itens pouco informativos

## Arquivos-chave

- `scripts/R/v5_per_domain_2d.R` — script que foi rodado
- `scripts/R/dynIRT_KD.R` — função principal (params de controle, EM loop)
- `outputs/v5_per_domain_2d/{domain}_2d_results.rds` — resultados salvos (contêm `runtime$loglik_trace`)
- `logs/v5_per_domain_2d_20260207T130110Z.log` — log batch 1
- `logs/v5_per_domain_2d_20260207T130226Z.log` — log batch 2

## Restrição

- **NÃO modificar** `dynIRT_KD.R`, `da_step.R`, `kalman.R`, `m_step.R` — são código de produção validado (V1-V4)
- Modificações devem ser no **script de chamada** (`v5_per_domain_2d.R`) ou em um **script de diagnóstico separado**
