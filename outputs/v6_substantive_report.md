# Relatório V6 — análise substantiva (2D)

Gerado em 2026-02-08 10:11:38 (UTC).

Este relatório resume os resultados substantivos dos modelos 2D estimados na fase V6, com foco em investment e security.

## 1. Escopo e dados

Investment: N=203, J=2629, T=6.
Security: N=164, J=862, T=6.

## 2. Identificação e âncoras

### 2.1 Investment
Âncoras por país: DNK, IRN, ESP.
Posições: (2,0), (-2,0), (0,-2).
Âncoras por item: unctad_/international-investment-agreements/treaties/treaties-with-investment-provisions/3777/ec---ukraine-cooperation-agreement-1994-__1995-1999; unctad_/international-investment-agreements/treaties/bilateral-investment-treaties/2766/philippines---switzerland-bit-1997-__1995-1999; unctad_/international-investment-agreements/treaties/treaties-with-investment-provisions/3185/cotonou-agreement-2000-__2000-2004.

### 2.2 Security
Âncoras por país: DNK, IRN, UKR.
Posições: (2,0), (-2,0), (0,-2).
Âncoras por item: atop_4675_consul__1995-1999; atop_4905_consul__1995-1999; atop_4953_nonagg__2000-2004.

## 3. Resultados substantivos — Investment

**Tabela 1. Extremos de Dimensão 1 (Investment; âncoras por país).**

| rank | top_country | top_iso | top_score | bottom_country | bottom_iso | bottom_score |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | Turkmenistan | TKM | 4.282 | Curacao | CUW | -4.263 |
| 2 | Tunisia | TUN | 3.039 | Barbados | BRB | -4.144 |
| 3 | Saudi Arabia | SAU | 2.853 | Qatar | QAT | -4.084 |
| 4 | Martinique | MTQ | 2.85 | Libya | LBY | -4.069 |
| 5 | Italy | ITA | 2.845 | Georgia | GEO | -3.76 |
| 6 | Burundi | BDI | 2.812 | Fiji | FJI | -3.692 |
| 7 | Sudan | SDN | 2.802 | Slovakia | SVK | -3.639 |
| 8 | Madagascar | MDG | 2.776 | Senegal | SEN | -3.562 |
| 9 | Cyprus | CYP | 2.751 | Liberia | LBR | -3.417 |
| 10 | South Africa | ZAF | 2.748 | Isle of Man | IMN | -3.371 |

**Tabela 2. Extremos de Dimensão 2 (Investment; âncoras por país).**

| rank | top_country | top_iso | top_score | bottom_country | bottom_iso | bottom_score |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | Isle of Man | IMN | 6.019 | Chad | TCD | -5.686 |
| 2 | Bahrain | BHR | 5.336 | Kenya | KEN | -4.007 |
| 3 | Taiwan | TWN | 5.297 | United Kingdom | GBR | -3.737 |
| 4 | Israel | ISR | 5.288 | Spain | ESP | -3.525 |
| 5 | Belarus | BLR | 5.285 | Saudi Arabia | SAU | -3.044 |
| 6 | Angola | AGO | 5.148 | Madagascar | MDG | -2.929 |
| 7 | St. Kitts & Nevis | KNA | 5.138 | Hungary | HUN | -2.778 |
| 8 | Somalia | SOM | 5.137 | Iceland | ISL | -2.73 |
| 9 | Austria | AUT | 5.09 | Thailand | THA | -2.524 |
| 10 | Djibouti | DJI | 5.09 | Papua New Guinea | PNG | -1.578 |

**Tabela 3. Médias e dispersão por período (Investment; âncoras por país).**

| period | n_active | mean_dim1 | sd_dim1 | mean_dim2 | sd_dim2 |
| --- | --- | --- | --- | --- | --- |
| 1990-1994 | 203 | 0.992 | 1.722 | 1.635 | 1.93 |
| 1995-1999 | 203 | 1.222 | 1.873 | 1.945 | 1.998 |
| 2000-2004 | 203 | 1.45 | 1.927 | 2.303 | 1.98 |
| 2005-2009 | 203 | 1.471 | 1.985 | 2.389 | 2.044 |
| 2010-2014 | 203 | 1.478 | 2.024 | 2.431 | 2.077 |
| 2015-2018 | 203 | 1.48 | 2.039 | 2.445 | 2.088 |

**Tabela 4. Robustez entre estratégias (Investment).**

| indicador | valor |
| --- | --- |
| Correlação Dim1 (média por país) | 0.599 |
| Correlação Dim2 (média por país) | 0.464 |
| Correlação Dim1 por período (média) | 0.905 |
| Correlação Dim2 por período (média) | 0.809 |
| Jaccard Top10 Dim1 | 0.176 |
| Jaccard Bottom10 Dim1 | 0.429 |
| Jaccard Top10 Dim2 | 0.111 |
| Jaccard Bottom10 Dim2 | 0.053 |

Interpretação operacional (Dim1): valores altos aproximam-se do polo definido pela âncora positiva (ex.: DNK) e valores baixos do polo negativo (ex.: IRN).
Tendência agregada Dim1: slope=0.092 por período; variação total=0.488.
Tendência agregada Dim2: slope=0.160 por período; variação total=0.810.

## 3. Resultados substantivos — Security

**Tabela 5. Extremos de Dimensão 1 (Security; âncoras por país).**

| rank | top_country | top_iso | top_score | bottom_country | bottom_iso | bottom_score |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | Hungary | HUN | 6.019 | Russia | RUS | -9.027 |
| 2 | Spain | ESP | 5.605 | Kazakhstan | KAZ | -3.84 |
| 3 | Romania | ROU | 5.084 | Uzbekistan | UZB | -3.385 |
| 4 | Czechia | CZE | 4.798 | Kyrgyzstan | KGZ | -3.263 |
| 5 | Slovakia | SVK | 4.757 | Tajikistan | TJK | -2.815 |
| 6 | Poland | POL | 4.172 | Belarus | BLR | -2.439 |
| 7 | Croatia | HRV | 3.941 | China | CHN | -2.366 |
| 8 | Germany | DEU | 3.777 | Armenia | ARM | -2.254 |
| 9 | Estonia | EST | 3.711 | Azerbaijan | AZE | -1.981 |
| 10 | Bulgaria | BGR | 3.312 | Iran | IRN | -1.892 |

**Tabela 6. Extremos de Dimensão 2 (Security; âncoras por país).**

| rank | top_country | top_iso | top_score | bottom_country | bottom_iso | bottom_score |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | Congo - Kinshasa | COD | 5.29 | Uzbekistan | UZB | -4.321 |
| 2 | Angola | AGO | 4.51 | Ukraine | UKR | -3.133 |
| 3 | Rwanda | RWA | 3.894 | Bulgaria | BGR | -2.814 |
| 4 | Congo - Brazzaville | COG | 3.804 | Poland | POL | -2.531 |
| 5 | Central African Republic | CAF | 3.735 | Kazakhstan | KAZ | -2.425 |
| 6 | Tanzania | TZA | 3.509 | Belarus | BLR | -2.203 |
| 7 | Zambia | ZMB | 3.509 | Romania | ROU | -1.797 |
| 8 | Burundi | BDI | 3.212 | China | CHN | -1.739 |
| 9 | Uganda | UGA | 3.169 | Kyrgyzstan | KGZ | -1.608 |
| 10 | Namibia | NAM | 3.106 | Lithuania | LTU | -1.582 |

**Tabela 7. Médias e dispersão por período (Security; âncoras por país).**

| period | n_active | mean_dim1 | sd_dim1 | mean_dim2 | sd_dim2 |
| --- | --- | --- | --- | --- | --- |
| 1990-1994 | 164 | 0.71 | 1.612 | 0.595 | 1.131 |
| 1995-1999 | 164 | 0.861 | 1.675 | 0.799 | 1.269 |
| 2000-2004 | 164 | 0.857 | 1.774 | 1.019 | 1.431 |
| 2005-2009 | 164 | 0.885 | 1.803 | 1.035 | 1.463 |
| 2010-2014 | 164 | 0.89 | 1.817 | 1.039 | 1.476 |
| 2015-2018 | 164 | 0.892 | 1.824 | 1.039 | 1.481 |

**Tabela 8. Robustez entre estratégias (Security).**

| indicador | valor |
| --- | --- |
| Correlação Dim1 (média por país) | 0.536 |
| Correlação Dim2 (média por país) | 0.588 |
| Correlação Dim1 por período (média) | 0.823 |
| Correlação Dim2 por período (média) | 0.841 |
| Jaccard Top10 Dim1 | 0.053 |
| Jaccard Bottom10 Dim1 | 0.818 |
| Jaccard Top10 Dim2 | 0.25 |
| Jaccard Bottom10 Dim2 | 0.333 |

Interpretação operacional (Dim1): valores altos aproximam-se do polo definido pela âncora positiva (ex.: DNK) e valores baixos do polo negativo (ex.: IRN).
Tendência agregada Dim1: slope=0.029 por período; variação total=0.183.
Tendência agregada Dim2: slope=0.084 por período; variação total=0.444.

## 4. Síntese para o paper (nível sistêmico)

As séries de médias por período (Dim1) servem como indicador de suporte agregado dentro de cada issue-area. Um declínio persistente indica erosão relativa do polo associado à âncora positiva; um aumento sugere reforço. A interpretação substantiva deve ser feita junto ao desenho de períodos e ao viés de flow coding descrito em docs/research_design_notes.md.

A robustez entre estratégias (âncoras por país vs por item) é moderada: correlações por dimensão e sobreposição de extremos fornecem um diagnóstico direto de estabilidade identificacional. Em especial, Dim2 tende a ser mais sensível a escolhas de âncora, o que recomenda cautela na narrativa substantiva dessa dimensão.

