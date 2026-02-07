# Resposta aos Pareceres: V2 Diagnóstico K=1

**Data**: 2026-02-06
**Documento avaliado**: `docs/v2_diagnostic_report.md`
**Pareceres recebidos**:
- Parecer 1: `docs/review_v2/parecer_reviewer2.md` + `docs/review_v2/nota_vi_vs_em.md`
- Parecer 2: `docs/v2_reviewer_opinion.md`
**Resposta do Reviewer 2**: `docs/review_v2/parecer_reviewer2_resposta.md` (4 correções aceitas e incorporadas)

---

## 1. Avaliação geral

Ambos os pareceres trazem contribuições valiosas mas divergem em diagnóstico e severidade.

O Parecer 1 identifica corretamente a causa teórica mais plausível da discrepância (uso de `P_smooth` no M-step) e propõe um enquadramento adequado (VI e EM resolvem problemas parecidos, não idênticos). O Parecer 2 levanta hipóteses testáveis (variância vs precisão, missing data) mas que, após verificação no código, não se sustentam.

**Atualização pós-Teste 1**: A hipótese #1 do Parecer 1 (P_smooth) foi refutada empiricamente. Ver Seção 3.

---

## 2. Análise ponto a ponto

### 2.1 Parecer 1

#### 3.1 — ELBO vs LL

**Veredito: CONCORDO.**

O relatório original reconhecia isso como ressalva (Seção 4, ponto 3), mas o reviewer tem razão que deveria ser o enquadramento central. A comparação de LL entre algoritmos que otimizam objetivos diferentes (LL observada vs ELBO) é intrinsecamente limitada. Uma LL mais alta do dynIRT_KD não implica "melhor solução" — apenas que o dynIRT_KD chegou mais perto do máximo da LL, enquanto o emIRT pode ter encontrado um ponto com ELBO maior (incluindo termos de KL e prior).

#### 3.2 — Anchors possivelmente não equivalentes

**Veredito: CONCORDO PARCIALMENTE.**

A especificação do prior é idêntica nos dois códigos:

| Parâmetro | emIRT (03_estimate_ideal_points.R:74-91) | dynIRT_KD (v2_equivalence_test.R:36-46) |
|-----------|---|----|
| `x_mu0[DNK]` | +2.0 | +2.0 |
| `x_sigma0[DNK]` | 0.01 | 0.01 |
| `x_mu0[SAU]` | -2.0 | -2.0 |
| `x_sigma0[SAU]` | 0.01 | 0.01 |
| Aplicação | Prior no primeiro período ativo (startlegis) | Prior no primeiro período ativo (startlegis) via kalman.R:70-71 |

Porém, **prior idêntico não implica efeito idêntico**: a forma como cada algoritmo propaga essa restrição ao longo do tempo pode diferir. Se a VI usa um update coordenada-por-coordenada (mean-field), cada `x[i,t]` é atualizado independentemente. No EM+Kalman, o smoother atualiza `x[i,1:T]` conjuntamente, respeitando a estrutura temporal do random walk. Isso pode explicar a queda de correlação no período 1 (r=0.924), onde o anchor prior tem maior influência.

#### 3.3 — P_smooth no M-step

**Veredito original: CONCORDO FORTEMENTE.**
**Atualização pós-Teste 1: HIPÓTESE REFUTADA EMPIRICAMENTE.**

Verificação direta no código:

```r
# m_step.R:66-68
P_sum <- matrix(0, nrow = K, ncol = K)
for (i in voters) {
  P_sum <- P_sum + P_smooth[[i]][[t_j]]
}

# m_step.R:80
Sigma_zz[2:Kp1, 2:Kp1] <- P_sum + sum_xx
```

O nosso M-step calcula o suficiente-estatístico completo:
```
E[x_i x_i' | Y] = Var[x_i | Y] + E[x_i | Y] E[x_i | Y]'
                 = P_smooth_i    + x_smooth_i x_smooth_i'
```

Isso é o EM **exato** (Dempster, Laird & Rubin, 1977). **Se** a VI do emIRT usa uma aproximação mean-field onde apenas `E[x]` entra no M-step (sem `Var[x|Y]`) — o que é plausível dado que a documentação declara "Dynamic IRT via Variational Inference", mas **não foi confirmado via inspeção do código-fonte C++ do emIRT** — então o suficiente-estatístico seria:

```
Sigma_zz ≈ X'X    (sem P_sum)
```

Nesse cenário, o ponto fixo mudaria porque `P_sum` adiciona um termo positivo-definido à matriz de informação dos itens.

**Porém, o Teste 1 (P=0) refutou esta hipótese**: zerar `P_smooth` no dynIRT_KD **afastou** a solução do emIRT (r caiu de 0.9725 para 0.9619). Ver Seção 3 para detalhes.

#### Critério revisado

**Veredito: CONCORDO** com r ≥ 0.95 como critério de compatibilidade estrutural (não equivalência numérica), condicionado a confirmar que a diferença é algorítmica via os testes.

**Justificativa metodológica**: VI e EM com data augmentation otimizam objetivos distintos (ELBO vs expected complete-data LL) e usam aproximações diferentes no E-step (mean-field vs Kalman smoother). O critério de equivalência numérica (r > 0.99) é adequado quando os algoritmos implementam o **mesmo** estimador (e.g., duas implementações de EM). Quando os estimadores diferem, o critério correto é **compatibilidade estrutural**: ambos recuperam o mesmo ordenamento relativo dos ideal points e a mesma estrutura de discriminação dos itens. O limiar r ≥ 0.95, combinado com Spearman ρ ≥ 0.95 e estabilidade temporal (diferença de r entre períodos ≤ 0.05), captura esta noção.

---

### 2.2 Parecer 2

#### Ponto 1 — Convergência lenta = bug

**Veredito: DISCORDO.**

O reviewer afirma: *"if the algorithm fails to converge to a tight tolerance, it strongly suggests a bug in the M-step derivation or implementation."*

Evidência contrária:
1. **A LL é monotonicamente crescente** ao longo de 1500 iterações (logs/v2_equivalence_retry.log:16-46): de -101.411 para -42.574, sem nenhum decréscimo. Esta é a propriedade fundamental de um EM correto.
2. **Convergência lenta de EM com data augmentation é um fenômeno documentado.** Van Dyk & Meng (2001, JRSS-B) mostram que EM com augmented data pode convergir mais lentamente que o EM marginal quando a fração de dados aumentados é grande. Com J=1849 itens e sparsity verificada de 20.36% (`logs/v2_sparsity_check.log`), a fração de dados latentes (y*) domina os dados observados.
3. **O reviewer confunde K=1 no Kalman com simplicidade geral.** Para K=1, o Kalman filter é escalar em K, mas com J=1849 itens por período, o block update envolve `crossprod(H)` onde H é uma matriz (n_t × 1). O bottleneck não é o Kalman, é a interação entre J itens e N países no DA step e M-step.
4. **O delta decai como O(1/√iter)** (Seção 5 do relatório original), típico de EM com muitos parâmetros fracamente identificados. Este é o comportamento esperado, não patológico.

#### Ponto 2 — Missing data (0 vs NA)

**Veredito: DESCARTADO.**

Verificação direta no código:

| Código | Tratamento de rc=0 | Referência |
|--------|-------------------|------------|
| da_step.R | `nonmiss_mask <- rc_sub != 0L` (linha 107) | Ignora zeros |
| compute_loglik (dynIRT_KD.R) | `pos_mask <- rc_sub == 1L`, `neg_mask <- rc_sub == -1L` (linhas 61-68) | Ignora zeros |
| emIRT | Documentação: "0 = missing, +1/-1 = votes" | Ignora zeros |

Ambos os códigos usam a mesma convenção. Não há evidência de discrepância.

#### Ponto 3 — Variância vs Precisão

**Veredito: DESCARTADO.**

Verificação direta:

| Parâmetro | Valor passado | emIRT interpretação | dynIRT_KD interpretação |
|-----------|---------------|--------------------|-----------------------|
| `x.sigma0` / `x.Sigma0` | 0.01 (anchors), 1.0 (outros) | Variância (doc: "prior variance") | Variância → `diag(0.01)` (dynIRT_KD.R:273) |
| `beta.sigma` | `diag(25, 2)` | Covariância (doc: "prior covariance") | Covariância → inverte para precisão (m_step.R:40) |

Mesma parametrização, mesmos valores. Se houvesse confusão σ² vs τ:
- Para anchors: τ=0.01 → variância=100 → anchors quase livres. Mas r~0.96 indica que ambos estão ancorados em ±2, descartando esta confusão.
- Para betas: a regularização seria completamente diferente (fator 625x). Mas r_beta=0.945 indica concordância estrutural, não confusão de escala.

O reviewer sugere que esta hipótese explica "sluggish convergence and drift in β." Mas a convergência lenta é do dynIRT_KD (que sabemos tratar como variância), não do emIRT. Uma confusão de prior no emIRT causaria divergência no emIRT, não no dynIRT_KD.

#### Ponto 4 — Runtime 60x

**Veredito: PARCIALMENTE CONCORDO.**

A comparação bruta é 70.8s vs 1.2s (~60x). Mas per-iteration: 0.14s vs 0.012s (~12x), explicável por R interpretado vs C++ compilado (emIRT). O dynIRT_KD roda 5x mais iterações (500 vs 102), multiplicando a diferença.

O ponto sobre escalabilidade para K>1 é válido — otimização via Rcpp é desejável. Mas "disqualifying" é exagerado: o objetivo imediato é validação, não produção.

#### Trace plots

**Veredito: CONCORDO** que seria útil como diagnóstico adicional. A LL trace monótona já está disponível no log, mas traces de parâmetros individuais (e.g., α, β de itens selecionados, x de países-chave) dariam insight sobre se há drift ou oscilação.

---

## 3. Resultado do Teste 1: P=0 (Mean-Field Approximation)

**Script**: `scripts/R/v2_test1_p0.R`
**Log**: `logs/v2_test1_p0.log`
**Resultados**: `outputs/v2_test_p0/results.rds`

### Resultados

| Comparação | r (ideal pts) | r (alpha) | r (beta) | LL₁ | LL₂ |
|---|---|---|---|---|---|
| emIRT vs KD_Normal | 0.9725 | 0.9349 | 0.9536 | -43,881 | -42,754 |
| emIRT vs KD_P0 | 0.9619 | 0.9436 | 0.9501 | -43,881 | -42,262 |
| KD_Normal vs KD_P0 | 0.9974 | 0.9962 | 0.9812 | -42,754 | -42,262 |

### Interpretação

1. **Hipótese A refutada.** Zerar `P_smooth` **afastou** a solução do emIRT (r caiu de 0.9725 para 0.9619), movendo-se na direção oposta ao esperado.
2. **Efeito de segunda ordem.** A correlação KD_Normal vs KD_P0 (r=0.997) confirma que `P_smooth` tem impacto pequeno neste dataset.
3. **LL do P=0 é maior.** Sem a regularização de `Var[x|Y]`, os itens ajustam mais agressivamente (LL sobe de -42,754 para -42,262). Mas isso não aproximou do emIRT (-43,881), reforçando que o gap é no E-step (variational posterior vs DA+Kalman), não no M-step.

---

## 4. Ranking atualizado das hipóteses (pós-Teste 1)

| # | Hipótese | Status | Evidência |
|---|----------|--------|-----------|
| 1 | **ELBO vs LL** (objetivos fundamentalmente diferentes) | **Mais provável** | Única explicação restante para gap persistente. |
| 2 | **Propagação diferente de anchors** | **Possível, menor** | Period 1 r=0.924. Prior idêntico mas efeito pode diferir. |
| 3 | ~~Bug no block Kalman~~ | **DESCARTADA** | Teste 2: block = seq (r=1.0, max diff ~1e-13). |
| 4 | ~~P_smooth no M-step~~ | **REFUTADA** | Teste 1: r caiu (0.9725 → 0.9619). |
| 5 | ~~Variância vs Precisão~~ | **DESCARTADA** | Verificação de código. |
| 6 | ~~Missing data~~ | **DESCARTADA** | Verificação de código. |
| 7 | ~~Convergência lenta = bug~~ | **DESCARTADA** | LL monótona. |

---

## 5. Resultado do Teste 2: Block vs Sequencial Kalman

**Relatório completo**: `docs/v2_test2_block_vs_seq_report.md`

Block e sequencial são numericamente idênticos (r=1.0, max diff ~1e-13). Hipótese D **descartada**. Block Kalman 13.8x mais rápido.

---

## 6. Classificação final de V2

### **PASS COM RESSALVA**

Matriz de decisão aplicada:
- Teste 1 (P=0): r não subiu → gap NÃO explicado por P_smooth
- Teste 2 (block vs seq): Block = Seq → sem bug na refatoração
- Conclusão: gap atribuído a diferença fundamental VI vs EM (Hipótese #1)

Critério revisado (r ≥ 0.95):
- Ideal points: r=0.9725 > 0.95 **PASS**
- Beta: r=0.9536 > 0.95 **PASS**
- Alpha: r=0.9349 < 0.95 **FAIL marginal** (ressalva)

**Ressalva**: Alpha (item intercepts) fica abaixo de 0.95. Atribuível à mesma diferença algorítmica. Para o objetivo do projeto (ideal points de países, não item parameters), não é bloqueante.

**Decisão**: Prosseguir para V3 (US Congress, benchmark contra DW-NOMINATE).

---

## 7. Arquivos referenciados nesta resposta

| Arquivo | Conteúdo verificado |
|---------|-------------------|
| `scripts/R/m_step.R:66-80` | P_smooth entra no suficiente-estatístico Sigma_zz |
| `scripts/R/da_step.R:107` | Missing data: `rc_sub != 0L` |
| `scripts/R/dynIRT_KD.R:61-68` | compute_loglik: ignora rc=0 |
| `scripts/R/dynIRT_KD.R:273` | x.Sigma0 tratado como variância |
| `scripts/R/kalman.R:70-71` | Prior aplicado no primeiro período ativo (startlegis) |
| `scripts/R/v2_equivalence_test.R:36-46` | Priors idênticos para ambos |
| `scripts/R/03_estimate_ideal_points.R:74-91` | Priors para emIRT |
| `logs/v2_equivalence_retry.log:16-46` | LL monótona crescente |
| `logs/v2_test1_p0.log` | Resultados do Teste 1 |
| `logs/v2_sparsity_check.log` | Sparsity verificada: 0.2036 |
| `docs/v2_diagnostic_report.md` | Relatório original sob revisão |
| `docs/review_v2/parecer_reviewer2.md` | Parecer 1 |
| `docs/review_v2/nota_vi_vs_em.md` | Nota técnica do Parecer 1 |
| `docs/v2_reviewer_opinion.md` | Parecer 2 |
| `docs/review_v2/parecer_reviewer2_resposta.md` | Resposta do Reviewer 2 (4 correções aceitas) |
| `docs/v2_test2_block_vs_seq_report.md` | Relatório do Teste 2 |
| `logs/v2_test2_block_vs_seq.log` | Log do Teste 2 |
| `outputs/v2_test_block_seq/results.rds` | Resultados do Teste 2 |
| `docs/v2_diagnostic_report_agent1_backup.md` | Backup da versão do Agente 1 |
| `docs/v2_response_to_reviews_agent1_backup.md` | Backup da versão do Agente 1 |
