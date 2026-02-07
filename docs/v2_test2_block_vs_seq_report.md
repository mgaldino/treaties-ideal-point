# Relatório: V2 Teste 2 — Block vs Sequencial Kalman

**Data**: 2026-02-06  
**Objetivo**: Verificar se a refatoração do Kalman (block update) introduziu regressão numérica em relação ao Kalman sequencial (item‑por‑item).

---

## 1. O que foi rodado

- **Script**: `scripts/R/v2_test2_block_vs_seq.R`
- **Kalman sequencial**: `scripts/R/kalman_sequential.R` (drop‑in replacement)
- **Log**: `logs/v2_test2_block_vs_seq.log`
- **Saída**: `outputs/v2_test_block_seq/results.rds`

Dados e configuração:
- Dataset: `data/processed/environment_flow_matrix.rds` (N=206, J=1849, T=6)
- `K = 1`
- Starts e priors idênticos para block e sequencial (`set.seed(123)`)
- `ncores = 1L` (determinismo)
- `maxit = 500` (sem convergência formal)

---

## 2. Resultados

### 2.1 Ideal points
- **Correlação por período (non‑anchors)**: 1.0000000000 em todos os períodos
- **Correlação global**: 1.0000000000
- **Máxima diferença absoluta**: 9.95e‑14

### 2.2 Parâmetros de itens
- **Alpha**: r = 1.0000000000; max |diff| = 1.59e‑13
- **Beta**: r = 1.0000000000; max |diff| = 4.53e‑14

### 2.3 Log‑likelihood
- **LL block** = −42923.8740
- **LL seq** = −42923.8740
- **Diferença** = 0.0000

### 2.4 Tempo de execução
- **Block**: 76.0s
- **Sequencial**: 1049.2s
- **Speedup**: ~13.8x

---

## 3. Veredito

**PASS** — Block e sequencial são numericamente equivalentes.  
**Conclusão**: A refatoração do block Kalman **não** introduziu regressão.

---

## 4. Implicações para V2

A hipótese de bug no block Kalman (Hipótese B do relatório V2) pode ser **descartada**. A discrepância com `emIRT::dynIRT()` deve ser atribuída a diferenças algorítmicas (VI vs EM, uso de `P_smooth`, etc.), não a erro na refatoração do Kalman.

---

## 5. Arquivos referenciados

| Arquivo | Descrição |
|---------|-----------|
| `scripts/R/kalman_sequential.R` | Implementação sequencial item‑por‑item |
| `scripts/R/v2_test2_block_vs_seq.R` | Script do teste | 
| `outputs/v2_test_block_seq/results.rds` | Resultados serializados |
| `logs/v2_test2_block_vs_seq.log` | Log completo da execução |
