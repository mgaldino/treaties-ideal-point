# Parecer Técnico (Reviewer 2) — V2 Diagnóstico K=1

**Autor**: Reviewer 2 (especialista em pontos ideais, variational Bayes e EM)  
**Data**: 2026-02-06  
**Documento avaliado**: `docs/v2_diagnostic_report.md`

---

## 1. Veredito executivo

O diagnóstico é tecnicamente competente e honesto, mas a classificação “FALHOU” está **super‑determinada** para o tipo de comparação feita. O relatório reconhece que os algoritmos otimizam objetivos distintos (VI vs EM + Kalman), o que torna a meta **r > 0.99** inadequada como teste de equivalência. Antes de qualquer rótulo conclusivo, são necessários testes de comparabilidade e um critério de avaliação revisado.

**Conclusão recomendada**: **“INCONCLUSIVO / NÃO COMPARÁVEL DIRETAMENTE”** até realizar testes mínimos de alinhamento algorítmico e de anchors.

---

## 2. Pontos fortes

- Diagnóstico distingue corretamente **VI vs EM** e aponta que os algoritmos podem convergir a pontos fixos diferentes.
- Log‑likelihood é analisada com cuidado, evitando interpretar LL maior como “melhor solução”.
- Reconhece a necessidade de verificar o efeito do Kalman em bloco (potencial regressão).

---

## 3. Questões críticas (devem ser endereçadas)

### 3.1 Objetivos distintos: LL vs ELBO
A comparação usa a função `compute_loglik()` do `dynIRT_KD` aplicada às estimativas do `emIRT`. Isso **não** é um teste justo se o `emIRT` otimiza uma ELBO. Uma ELBO inclui termos de KL e prior e, portanto, **maximizar ELBO não maximiza LL** necessariamente. Conclusão: LL maior do `dynIRT_KD` não implica “melhor”, apenas **objetivo diferente**.

### 3.2 Anchors possivelmente não equivalentes
No `dynIRT_KD`, anchors entram como **prior forte em t=1** (posição inicial). No `emIRT`, frequentemente há ancoragem **fixa ou implícita** em todos os períodos. A pior correlação no período 1 é um indício de assimetria no tratamento dos anchors. A comparação deve ser refeita com ancoragem equivalente (mesma regra, mesmo período, mesma força).

### 3.3 Uso de `P_smooth` no M‑step
O `dynIRT_KD` usa `E[x]` e `Var[x]` via `P_smooth`. Muitos esquemas variacionais usam apenas `E[x]`. Isso muda o ponto fixo e **explica r≈0.96** sem exigir bug.

---

## 4. Interpretação recomendada da V2

- A V2 **não deve ser tratada como teste de correção** enquanto a comparação for entre objetivos diferentes.
- O critério r>0.99 é **inadequado** para VI vs EM. Deve ser substituído por **comparabilidade estrutural** com tolerância explícita.

---

## 5. Testes mínimos necessários (curtos e decisivos)

1. **Teste “P=0”** no `dynIRT_KD`: rodar M‑step usando apenas `E[x]` (ignorar `P_smooth`) para aproximar o comportamento variacional. Se a correlação subir, o gap é **algorítmico**.
2. **Teste de anchors equivalentes**: alinhar a regra de ancoragem do `emIRT` (fixo vs prior, todos t vs t=1) e repetir a V2.
3. **Teste Kalman sequencial (pré‑block)**: garantir que o bloco não introduziu regressão. Se resultados forem idênticos, descarta‑se bug na refatoração.
4. **Simulação controlada K=1**: comparar VI vs EM em dados simulados, com verdade conhecida, para separar efeitos de inferência de efeitos de dados reais.

---

## 6. Critério revisado sugerido para V2

Substituir “equivalência r>0.99” por um critério **compatível com objetivos distintos**:

- **Correlação média** dos ideal points por período (excluindo anchors): **r ≥ 0.95**
- **Correlação global** (todos os períodos empilhados): **r ≥ 0.95**
- **Correlação de α e β**: **r ≥ 0.95** após alinhamento de sinal (K=1)
- **Checagem de ordenação**: Spearman ρ ≥ 0.95 para países não‑anchor
- **Estabilidade temporal**: diferença média de r entre períodos ≤ 0.05

Esses critérios refletem “mesma estrutura latente” sem exigir igualdade numérica ponto‑a‑ponto.

---

## 7. Recomendação final

A V2 deve ser reclassificada como **“inconclusiva”** até que os testes acima sejam rodados. Se os testes confirmarem que a discrepância é explicada por diferenças de objetivo e anchors, o correto é **passar a V2 com critério revisado**, explicitando que a comparação é apenas estrutural.

---

## 8. Sugestões editoriais

- Separar claramente as mensagens “falha” e “não comparável”.
- Incluir uma subseção “critério revisado” com justificativa metodológica.
- Explicitar que o objetivo da V2 é **verificação de consistência**, não equivalência numérica.
