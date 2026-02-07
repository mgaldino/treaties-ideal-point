# Análise dos Resultados V3: O Papel das Âncoras na Identificação Dimensional

**Data:** 06 de Fevereiro de 2026
**Objeto:** Interpretação dos resultados da validação do Congresso Americano (105º-112º).
**Resultado:** Dimensão 1 (r=0.98), Dimensão 2 (r=0.35).

## 1. A Geometria das Âncoras Escolhidas

Para identificar um espaço de $K=2$ dimensões, o modelo exige $K+1=3$ âncoras que formem um triângulo não degenerado (posição geral). As âncoras utilizadas foram:

1.  **Paul Wellstone (D-MN)**: Posicionado em $(-2.0, -1.32)$. *Arquetípico Liberal.*
2.  **Tom Coburn (R-OK)**: Posicionado em $(+2.0, +0.19)$. *Arquetípico Conservador.*
3.  **Lincoln Chafee (R-RI)**: Posicionado em $(-0.28, -2.0)$. *Republicano Moderado/Liberal.*

### 1.1 Definição da Dimensão 1 (Eixo Ideológico)
A escolha de Wellstone e Coburn como extremos opostos no eixo X ($-2$ vs $+2$) forçou a primeira dimensão a capturar a variância associada à polarização partidária.
*   Como a votação no Congresso Americano moderno (1997-2012) é esmagadoramente unidimensional e partidária, essa escolha alinhou o eixo principal do modelo (`dynIRT`) quase perfeitamente com o eixo principal dos dados.
*   **Resultado:** Correlação de 0.98 com DW-NOMINATE Dim 1. O modelo recuperou com sucesso a estrutura dominante porque as âncoras estavam alinhadas com o vetor principal de variância (PC1).

### 1.2 Definição da Dimensão 2 (O Problema do "Eixo Chafee")
A segunda dimensão foi definida primariamente pela posição de **Lincoln Chafee**.
*   Ao fixar Chafee em $y = -2.0$ (enquanto Wellstone e Coburn estão mais acima no eixo Y), o modelo tentou definir a segunda dimensão como "aquilo que distingue Lincoln Chafee da média partidária".
*   Substantivamente, Chafee era conhecido por ser fiscalmente conservador mas socialmente liberal/ambientalista. Portanto, a Dimensão 2 do `dynIRT` tentou capturar essa clivagem específica (Social/Ambiental vs Econômico).

## 2. Por que a Dimensão 2 falhou na correlação (r=0.35)?

A baixa correlação com a Dimensão 2 do DW-NOMINATE não indica necessariamente que o modelo errou, mas que ele **descobriu uma dimensão diferente** daquela do benchmark, devido à escolha da âncora.

### 2.1 A Natureza da Dimensão 2 no DW-NOMINATE
No DW-NOMINATE moderno, a segunda dimensão é frequentemente descrita como "residual" ou "caótica", capturando uma mistura de:
*   Interesses regionais (Sul vs Norte).
*   Questões raciais (antigamente).
*   Votos estratégicos/procedimentais.
*   Questões sociais transversais.

No período analisado (105º-112º), a variância explicada pela segunda dimensão é historicamente baixa. O "espaço" é quase uma linha reta.

### 2.2 O Conflito de Rotação
Mesmo com o alinhamento de Procrustes (que tenta rotacionar nossa solução para bater com o DW-NOMINATE), a correlação permaneceu baixa. Isso sugere que a **forma** da nuvem de pontos é diferente.
*   O `dynIRT` (guiado por Chafee) esticou os legisladores ao longo de um eixo "Moderado Republicano".
*   O DW-NOMINATE pode ter definido seu segundo eixo com base em outra clivagem (ex: Democratas Conservadores do Sul vs Democratas Liberais da Costa Oeste).

Se a "direção Chafee" não for ortogonal à "direção Wellstone-Coburn" nos dados reais, o modelo probit (que força ortogonalidade na prior $x \sim N(0, I)$) pode ter dificuldade em separar esses sinais, resultando em uma segunda dimensão ruidosa.

## 3. Conclusão Metodológica

A escolha das âncoras não apenas fixa a rotação, mas **determina o significado semântico** das dimensões latentes.

*   **Sucesso:** Usamos âncoras extremas (Wellstone/Coburn) para a dimensão principal, e o modelo capturou o sinal mais forte (partidarismo).
*   **Lição:** Para a Dimensão 2, confiar em um único legislador "cross-pressured" (Chafee) é arriscado se a dimensão não for saliente nos dados. O modelo tenta explicar o comportamento de Chafee como uma dimensão sistêmica, o que pode não se generalizar para o resto do Congresso (overfitting no comportamento da âncora).

**Recomendação para Aplicação em Tratados (Fase V5):**
Ao aplicar em dados de tratados (onde não temos um DW-NOMINATE para guiar), devemos selecionar âncoras para a Dimensão 2 que representem uma **clivagem teórica clara** (ex: Norte-Sul Global ou Democracia-Autocracia) e não apenas um "país moderado". Se a segunda dimensão for fraca nos dados, a escolha da âncora dominará a interpretação, criando potencialmente um artefato.
