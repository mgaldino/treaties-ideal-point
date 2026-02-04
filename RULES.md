RULES.md

Here I present rules that you must follow to produce great outputs to me, things I care or how I think or like to do stuff.

1. I prefer R for data analys, since that is the programming language I know most.
2. I prefer python for machine learning, web scrapping and text processing because it is better suited to such tasks.
3. I prefer python for web apps or CLI models or apps.
4. I trust what has been tested. I am skeptical of what hasn't been tested.
5. For papers and reports, the desired default output is pdf.
6. Always start with Q&A. 
7. Follow up with more questions after the first iteration of Q&A
8. More often than not I have an ultimate or long term goal than what I am asking for. If helpful to complte the present task, try to confirm what it is.
9. Keep computation in separate R scripts.
10. Make reports, analysis and academic papers fully reproducible.
11. Preserve raw/extracted files in the repo.
12. Ao realizar validações de dados, cheque Regras lógicas de datas e valores incompatíveis com o que se espera de variáveis, quando aplicável.
13. Dados públicos podem ser usados sem autorização; documente fonte e data de acesso.
14. Identificação causal é muito importante para mim. Sempre que o escopo envolver causalidade, assuma chapéu dos melhores economistas.
15. Sou Bayesiano. Leve isso em consideração se o escopo permitir.
16. Explique os passos para resolver o problema ou cumprir a tarefa que é pedida, antes de executá-la e peça feedback sobre o plano.
16. Sempre numere suas perguntas, para facilitar respostas.
17. Se não existir um READ.me, crie um assim que todo o planejamento estiver pronto.
18. Sempre numere figuras e tabelas.
19. Sempre que houver um plano com várias fases, só passe para a fase seguinte após ter testado extensivamente que cada fase foi implementada corretamente.
20. Após cada fase ser encerrada, atualize HANDOFF.md se existir, ou READ.md caso não exista.
21. Se qualquer tarefa exigir acesso à internet e falhar por DNS/conectividade, solicite imediatamente escalonamento de permissões para o mesmo comando, sem diagnóstico adicional.
22. Se alguma extração via rede falhar (DNS, timeout, HTTP 403) mesmo com permissionamento escalado : opção "a" é tentar novamente após 30 segundos (pode ser falha temporária); opção "b", se falha persistir, tente uma fonte alternativa. Grave em log as falhas.
23. When any step fails (script error, download failure, package installation error, etc.), do NOT simply retry the same command or give up. Run diagnostic commands **before** retrying and **act** on the diagnosis.  If all else fails, log the exact error message and diagnostic results to `logs/` and report to user.
24. Sempre que um plano tiver várias fases, escreva o plano completo em um arquivo (ex: `docs/`) antes de começar a execução.