# Nota Técnica — VI vs EM em dynIRT (K=1)

**Data**: 2026-02-06  
**Objetivo**: Explicar por que VI e EM + Kalman podem convergir para soluções diferentes em K=1, mesmo com priors idênticos.

## 1. Objetivos distintos

- **VI** maximiza uma **ELBO** (Evidence Lower Bound), que combina verossimilhança esperada + termo de divergência KL entre a distribuição aproximada e o posterior verdadeiro.
- **EM** (com data augmentation + Kalman) maximiza a **verossimilhança** (ou a expectativa da completa) de forma exata/mais próxima do modelo gerador.

Consequência: **os ótimos não precisam coincidir**. Uma ELBO mais alta não implica LL mais alta, e vice‑versa.

## 2. Diferenças na atualização de x

- Em EM, o Kalman usa **E[x]** e **Var[x]** (via `P_smooth`) no M‑step.
- Em muitas variantes variacionais, o update é tipo **mean‑field**, usando apenas **E[x]** e ignorando `Var[x]`.

Isso altera o ponto fixo, especialmente quando os itens são muitos e a informação é heterogênea no tempo.

## 3. Anchors e condições iniciais

A ancoragem via prior forte em t=1 (EM) **não é equivalente** a fixar a posição em todos os t (VI). Esse detalhe afeta principalmente o período 1 e explica a queda de correlação nesse período.

## 4. Implicação para V2

Portanto, V2 não deve exigir equivalência numérica estrita (r>0.99) entre VI e EM. O critério correto é **compatibilidade estrutural** (ex.: r≥0.95, Spearman ≥0.95, consistência por período), com alinhamento de sinal e anchors equivalentes.

## 5. O que seria uma comparação justa?

- Alinhar anchors (mesma regra e mesma força).  
- Comparar com EM “aproximado” (ignorar `P_smooth`) para aproximar o VI.  
- Avaliar correlação estrutural, não igualdade ponto‑a‑ponto.  

---

**Resumo**: VI e EM resolvem problemas **parecidos**, não **idênticos**. V2 deve refletir essa diferença para evitar falsos negativos.
