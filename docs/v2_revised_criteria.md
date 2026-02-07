# Critério Revisado — V2 (Compatibilidade Estrutural, não Equivalência Numérica)

**Data**: 2026-02-06  
**Motivação**: `emIRT::dynIRT()` e `dynIRT_KD()` otimizam objetivos distintos (ELBO/VI vs EM+Kalman), portanto **equivalência numérica estrita** (r > 0.99) não é um critério adequado. O objetivo da V2 passa a ser **compatibilidade estrutural**.

---

## 1. Escopo

Este critério substitui a exigência de “equivalência r>0.99” e passa a avaliar se as soluções apresentam **a mesma estrutura latente**, dentro de tolerâncias realistas para algoritmos com objetivos diferentes.

---

## 2. Critérios propostos

### 2.1 Ideal points (não‑anchors)

- **Correlação por período**: r ≥ **0.95** em pelo menos 4 de 6 períodos, e r ≥ **0.93** em todos os períodos.  
- **Correlação global (todos os períodos empilhados)**: r ≥ **0.95**.  
- **Spearman ρ global**: ρ ≥ **0.95**.

### 2.2 Parâmetros de itens

- **Alpha**: r ≥ **0.95**.  
- **Beta**: r ≥ **0.95**.  

### 2.3 Estabilidade temporal

- Diferença entre o maior e o menor r por período ≤ **0.06**.  
  (Evita soluções que se alinham apenas em um subconjunto de períodos.)

---

## 3. Justificativa metodológica

- A VI otimiza uma **ELBO**, não a LL; o EM+Kalman otimiza a LL (ou completa).  
- Objetivos diferentes ⇒ **ótimos diferentes** são esperados.  
- O critério revisado mede **coerência estrutural** (ordenação e direção no espaço latente) e não igualdade numérica ponto‑a‑ponto.

---

## 4. Observações

- Antes de aplicar este critério, deve‑se verificar se a discrepância não é causada por bugs (ex.: teste block vs sequencial Kalman).  
- O critério deve ser aplicado **após alinhamento de sinal** (K=1) e exclusão dos anchors na correlação dos ideal points.

---

## 5. Decisão

Se todos os critérios acima forem atendidos, a V2 deve ser classificada como **PASS (compatibilidade estrutural)**. Caso contrário, a V2 permanece **INCONCLUSIVA** e requer testes adicionais.
