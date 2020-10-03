# Painel Macro

Painel de indicadores macroeconômicos do Grupo de Estudos de Conjuntura Econômica da Universidade Federal do Rio Grande (GECE/FURG).

## Instruções

O procedimento de criação do painel segue, atualmente, o seguinte fluxo:

1. Execução individual de scripts de cada seção do painel (mercado de trabalho, inflação, etc...) na pasta `R`, conforme divulgação de novos dados;
2. Ao executar o script de qualquer seção, os dados são baixados, tratados e salvos na pasta `data`;
3. Rodar o arquivo `dashboard.Rmd`, que importa os dados salvos, gera gráficos/tabelas e cria o painel (um arquivo `html`).

## Próximos passos

- Inserir opção no arquivo `.Rmd` para rodar os scripts das seções sem precisar abri-los individualmente.
- Harmonização de nomes das variáveis.
- Criar funções para geração de gráficos.
- Atualização de dados dinâmica, conforme algum calendário econômico.
- Transformar o painel em um pacote.
