# Parecer Técnico (Reviewer 2) — Avaliação da Resposta aos Pareceres (V2)

**Autor**: Reviewer 2 (pontos ideais, variational Bayes, EM)  
**Data**: 2026-02-06  
**Documento avaliado**: `docs/v2_response_to_reviews.md`

---

## 1. Veredito executivo

A resposta é bem estruturada, responde de forma substantiva aos dois pareceres e estabelece uma hierarquia de hipóteses coerente. Entretanto, há **duas afirmações centrais tratadas como fatos sem verificação no emIRT**, e um detalhe técnico sobre “t=1” que precisa ser corrigido. Com pequenos ajustes de linguagem e precisão, a resposta fica sólida.

---

## 2. Pontos fortes

1. **Enquadramento correto (ELBO vs LL)**: a resposta reconhece que os objetivos diferem e evita uma leitura ingênua de LL.  
2. **Diagnóstico central plausível**: o papel do `P_smooth` no M‑step é bem identificado como fonte provável do desvio.  
3. **Ranking de hipóteses**: a priorização faz sentido e é útil para orientar testes.  

---

## 3. Pontos que exigem correção (alta prioridade)

### 3.1 “Prior em t=1 apenas” — imprecisão
No `dynIRT_KD`, o prior é aplicado **no primeiro período ativo de cada unidade**, não necessariamente t=1 global. A resposta afirma “t=1 apenas”, o que pode distorcer a comparação com o emIRT.

**Correção sugerida**: substituir por “prior aplicado no primeiro período ativo (startlegis)”.

### 3.2 Inferência variacional no emIRT não é verificada
A resposta assume que o emIRT usa **mean‑field** (ou atualização que ignora `Var[x]`) e trata isso como base factual para concluir que `P_smooth` explica o gap. Isso é **plausível**, mas não foi confirmado via inspeção do emIRT.

**Correção sugerida**: reescrever com linguagem condicional (“se o emIRT ignora Var[x], então…”), ou adicionar uma verificação explícita do código-fonte do emIRT.

### 3.3 Anchors “idênticos” pode ser uma equivalência formal, não operacional
Mesmo com `x.mu0` e `x.sigma0` idênticos, a forma como a VI propaga esse prior ao longo do tempo pode diferir. A resposta já reconhece isso parcialmente, mas precisa deixar claro que “prior idêntico” **não implica** “efeito idêntico”.

---

## 4. Sparsity verificada (incluir no texto)

A resposta menciona “sparsity ~20%”. Isso **foi verificado** diretamente nos dados V2:

- **Dataset**: `data/processed/environment_flow_matrix.rds`  
- **Resultado**: `sparsity = 0.2036` (20.36% de zeros)  
- **Log**: `logs/v2_sparsity_check.log`  

Sugestão: substituir a frase informal por este número com referência explícita ao log.

---

## 5. Mérito do critério revisado (r ≥ 0.95)

O critério revisado é razoável, mas deve ser justificado com 1–2 frases metodológicas (VI vs EM = objetivos distintos; foco em compatibilidade estrutural). Isso transforma um critério “ad hoc” em uma decisão defensável.

---

## 6. Veredito final

**A resposta é boa**, mas precisa de ajustes de precisão técnica (t=1 vs startlegis; natureza do VI) e explicitação das hipóteses não verificadas. Com essas correções, torna‑se um documento robusto para reclassificar a V2 como “inconclusiva” ou “compatível estruturalmente”.

---

## 7. Recomendações pontuais de edição

1. Trocar “prior em t=1 apenas” por “prior no primeiro período ativo (startlegis)”.
2. Substituir afirmações assertivas sobre mean‑field por linguagem condicional.
3. Inserir o valor de sparsity verificado (0.2036) com referência ao log.
4. Acrescentar 1–2 linhas justificando o critério r ≥ 0.95.

