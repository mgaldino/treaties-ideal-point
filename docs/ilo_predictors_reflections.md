# Reflexões sobre preditores e interpretação da Dimensão 1 como suporte à ILO

Data: 2026-02-08

## Objetivo deste documento
Este documento consolida reflexões metodológicas e estratégicas sobre o uso de preditores externos para reforçar a interpretação da Dimensão 1 (Dim1) como “suporte à Ordem Liberal Internacional (ILO)” nos modelos 2D estimados a partir de participação em tratados e IOs. O foco é orientar escolhas futuras sem alterar imediatamente o algoritmo.

## Situação atual
A fase V6 concluiu estimações 2D com novas âncoras para **investment** e **security**. Outras issue-areas existem no banco (trade, environment, human_rights, arms_control, intellectual_property), mas ainda não têm estimações 2D equivalentes nesta fase. Os diagnósticos iniciais sugerem que Dim1 cresce no tempo em ambos os domínios, porém o alinhamento com UNGA é **moderado em security** e **fraco em investment**. Isso indica que a leitura “Dim1 = suporte à ILO” é plausível para security, mas ainda frágil para investment.

## Por que precisamos de preditores externos
Interpretar Dim1 como suporte à ILO é uma **hipótese substantiva**, não uma identificação automática. Para sustentá-la, precisamos de evidências externas que distingam três coisas:
1. Alinhamento internacional (ILO) versus liberalismo doméstico.
2. Eixos específicos de domínio (ex.: segurança) versus eixos gerais de participação em tratados.
3. Variação temporal agregada versus mudanças idiossincráticas de poucos países.

Preditores externos ajudam a ancorar a interpretação sem impor o resultado. Eles também permitem testar se Dim1 está “capturando o que deveria”, especialmente quando a evidência interna é ambígua.

## Caminhos possíveis
Abaixo estão caminhos graduais, do menos invasivo ao mais estrutural.

### 1. Validação pós-estimação (recomendado no curto prazo)
Usa os pontos ideais já estimados e testa sua associação com preditores externos por período.

Vantagens:
- Não exige alterações no algoritmo.
- Produz evidência direta para a narrativa do paper.
- É replicável e transparente.

Limitações:
- Não resolve identificação; apenas avalia plausibilidade.
- Sensível à escolha de preditores e à agregação temporal.

### 2. Supervisão leve via prior (sensibilidade, não recomendada como solução principal)
Define o prior inicial de Dim1 com base em um preditor (ex.: V-Dem) no primeiro período. Isso **orienta** o eixo, mas não estima o efeito do preditor.

Vantagens:
- Fácil de implementar.
- Útil como teste de robustez e alinhamento inicial.

Riscos:
- Pode colar Dim1 em liberalismo doméstico, especialmente se o preditor for V-Dem.
- Pode reduzir a interpretabilidade internacional do eixo.

### 3. Extensão estrutural do modelo com covariáveis
Inclui preditores na equação de estado do modelo (ex.: drift em função de Z_it). Isso permite estimar o impacto dos preditores no movimento de Dim1.

Vantagens:
- Fornece interpretação mais estruturada e com efeito estimado.
- Permite separar dinamicamente fatores domésticos e internacionais.

Desafios:
- Exige mudanças no filtro de Kalman e no M-step.
- Requer atualização da versão Rcpp e novos testes de validação.
- Aumenta complexidade computacional e risco de erros silenciosos.

### 4. Ancoragem substantiva por itens
Escolher itens-âncora explicitamente ligados à ILO pode alinhar Dim1 ao conteúdo internacional, sem introduzir covariáveis. Isso depende de uma curadoria substantiva dos tratados.

Vantagens:
- Mantém o modelo original.
- Aumenta interpretabilidade substantiva.

Riscos:
- Requer escolhas normativas explícitas.
- Pode reduzir comparabilidade entre domínios.

### 5. Rotação ou alinhamento pós-hoc
Após estimar o espaço 2D, pode-se rotacionar para maximizar correlação com um critério externo (ex.: UNGA). Isso facilita interpretação, mas é uma intervenção estatística fora do modelo.

Vantagens:
- Útil para diagnóstico e visualização.
- Não muda o ajuste do modelo.

Riscos:
- Pode ser visto como “forçar” interpretação ex post.
- Menos elegante para publicação metodológica.

## Preditores candidatos e avaliação
### V-Dem (liberal democracy)
- Vantagem: excelente cobertura temporal e comparabilidade.
- Risco: mede liberalismo doméstico, não necessariamente alinhamento internacional.
- Uso recomendado: **controle** ou validação, não prior.

### Conflitos autorizados vs não autorizados pela ONU
- Forte conteúdo substantivo para ILO.
- Requer definição clara de “autorizado” (resolução do CSNU) e codificação por período.
- Pode gerar um teste direto: participação em conflito não autorizado deveria reduzir Dim1.

### Peacekeeping e missões autorizadas
- Próximo do núcleo normativo da ILO.
- Necessita base consolidada e critérios de participação (tropas, financiamento, comando).

### Refugiados e políticas migratórias
- Relevante para liberalismo internacional, mas potencialmente circular se tratados de direitos humanos estiverem no domínio analisado.
- Melhor se a fonte for **independente** do conjunto de tratados já usado.

### Trade openness (posterior)
- Faz sentido quando a dimensão trade estiver pronta via tarifas.
- Pode ser usado como preditor externo para testar se Dim1 está “contaminada” pelo eixo econômico.

## Riscos e desafios
1. **Endogeneidade**: países podem aderir a tratados porque já estão alinhados à ILO; o preditor e Dim1 podem ser co-determinados.
2. **Circularidade**: usar preditores derivados de tratados da própria base pode inflar correlações.
3. **Agregação temporal**: janelas longas diluem choques importantes (ex.: 2003, 2011). Isso pode reduzir o sinal dos preditores.
4. **Comparabilidade entre domínios**: o mesmo preditor pode significar coisas diferentes em investment e security.
5. **Viés de flow coding**: países com adesões antigas podem parecer menos “ativos”, o que afeta a leitura de suporte agregado.
6. **Interpretação normativa**: “suporte à ILO” é um conceito contestável; precisa de definição operacional explícita.

## Potenciais ganhos
1. **Clareza substantiva**: fortalece a interpretação do eixo principal no paper.
2. **Validade externa**: ancorar Dim1 em preditores reconhecidos melhora credibilidade.
3. **Diferenciação de mecanismos**: permite separar liberalismo doméstico de alinhamento internacional.
4. **Narrativa sistêmica**: facilita discutir erosão ou reforço da ILO no tempo com indicadores independentes.
5. **Transparência**: diagnóstico explícito evita “overclaiming” sobre o que Dim1 mede.

## Estratégia recomendada (sequência pragmática)
1. **Validação pós-estimação imediata** com V-Dem (como controle), UNGA, e um indicador simples de conflitos autorizados/não autorizados por período.
2. **Teste de sensibilidade** com supervisão leve via prior apenas como robustness check, não como solução principal.
3. **Curadoria de âncoras substantivas** em security para testar se Dim1 se alinha mais com ILO quando os itens-âncora são normativamente centrais.
4. **Avaliar necessidade de extensão estrutural** só após consolidar os resultados de validação.

## Ganhos e custos esperados por caminho
1. Validação pós-estimação: ganho alto, custo baixo, risco baixo.
2. Supervisão via prior: ganho moderado, custo baixo, risco médio de viés interpretativo.
3. Extensão estrutural com covariáveis: ganho alto, custo alto, risco médio-alto de complexidade e regressões.
4. Ancoragem substantiva: ganho moderado, custo médio, risco médio de arbitrariedade.
5. Rotação pós-hoc: ganho baixo a moderado, custo baixo, risco médio de percepção “ad hoc”.

## Considerações estratégicas para o paper
1. **Não sobre-interpretar Dim1** em investment; a evidência atual sugere um eixo distinto de alinhamento UNGA.
2. **Ancorar a narrativa sistêmica** em security, onde o alinhamento com UNGA é mais consistente.
3. **Explicitar o que Dim1 não mede**: liberalismo doméstico não é o mesmo que suporte à ILO.
4. **Apresentar a validação como contribuição**: um teste explícito de alinhamento internacional pode ser um diferencial do paper.
5. **Planejar extensões futuras**: a inclusão de trade e covariáveis abre caminho para uma explicação mais completa do suporte à ILO.

## Próximos passos sugeridos
1. Definir fontes para conflitos autorizados e peacekeeping, com codificação por período.
2. Construir um quadro de preditores externos com documentação de fonte e data de acesso.
3. Estimar correlações e regressões por domínio e período.
4. Decidir se a extensão estrutural do modelo é necessária para o paper principal ou para um apêndice metodológico.

