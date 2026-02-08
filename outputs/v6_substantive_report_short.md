# Relatório curto — V6 (2D)

Gerado em 2026-02-08 10:25:56 (UTC).

## Escopo

O V6 estima modelos 2D com novas âncoras apenas para investment e security. Existem 7 issue-areas no banco atual, mas apenas essas duas têm estimação 2D finalizada na fase V6.

## Diagnóstico substantivo: suporte à Ordem Liberal Internacional (ILO)

Hipótese operacional: a Dim1, ancorada em DNK (polo positivo) vs IRN (polo negativo), captura variação alinhada com suporte à ILO. Isso é um teste substantivo, não uma identificação automática.

Evidência: (i) tendência temporal das médias de Dim1; (ii) correlação de Dim1 com ideal points da UNGA por período.

**Tabela 1. Tendências agregadas por domínio e dimensão (médias por período).**

| dominio | dimensao | slope | delta_total |
| --- | --- | --- | --- |
| Investment | Dim1 | 0.092 | 0.488 |
| Investment | Dim2 | 0.16 | 0.81 |
| Security | Dim1 | 0.029 | 0.183 |
| Security | Dim2 | 0.084 | 0.444 |

**Tabela 2. Correlação de Dim1 com UNGA ideal points (por período).**

| dominio | cor_media_dim1 | cor_min_dim1 | cor_max_dim1 |
| --- | --- | --- | --- |
| Investment | 0.092 | 0.06 | 0.119 |
| Security | 0.438 | 0.378 | 0.487 |

## Interpretação

Investment: Dim1 cresce de forma monotônica (Figura 1), sugerindo aumento relativo de adesão ao polo DNK ao longo dos períodos. A correlação com UNGA em Dim1 é baixa (média ~0.09), o que sinaliza suporte fraco para interpretar Dim1 como alinhamento à ILO em investment.

Security: Dim1 também cresce, mas em magnitude menor (Figura 2). A correlação com UNGA em Dim1 é moderada e estável (média ~0.44), o que dá apoio empírico à leitura de alinhamento com a ILO neste domínio.

Dim2: apresentou maior sensibilidade a escolhas de âncoras (ver relatório substantivo completo). Evite ancorar a narrativa principal na Dim2 sem validação externa adicional.

## Nota metodológica

A baixa correlação entre Dim1 e UNGA no domínio de investment pode refletir diferenças substantivas entre arenas: UNGA capta alinhamentos de segurança/diplomacia, enquanto tratados de investimento refletem compromissos jurídico‑econômicos e padrões de proteção ao capital. Além disso, o uso de flow coding privilegia entradas recentes e pode atenuar o sinal de membros antigos, o que reduz a convergência com medidas mais estáveis de alinhamento multilateral.

## Implicações para o paper

Os resultados sugerem que o suporte à ILO é multidimensional e domínio‑específico: security fornece um sinal consistente de alinhamento agregado, enquanto investment parece capturar uma dimensão distinta e menos correlacionada com a UNGA. Para a narrativa sistêmica, recomenda‑se tratar a Dim1 de security como eixo central de suporte e usar investment como componente complementar, explicitando a diferença de mecanismos institucionais.

## Figuras

**Figura 1.** Investment: médias por período (Dim1 e Dim2). Arquivo: `outputs/fig_v6_investment_trends.png`.

**Figura 2.** Security: médias por período (Dim1 e Dim2). Arquivo: `outputs/fig_v6_security_trends.png`.

