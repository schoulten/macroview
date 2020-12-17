### Política Fiscal


## Rotina de extração e tratamento de dados



# Pacotes necessários -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load("readxl","tidyverse","rbcb","sidrar","zoo", "janitor", "rio")



# Importação de dados -----------------------------------------------------

options(scipen = 999)


download.file("https://sisweb.tesouro.gov.br/apex/cosis/thot/transparencia/anexo/9906:429799:inline",
              destfile = "../data/resultado.xlsx",
              mode = "wb")
dados_resultado_tn <- as_tibble(t(read_xlsx("../data/resultado.xlsx",
                                      sheet = "1.1-A",
                                      col_names = FALSE,
                                      skip = 5,
                                      n_max = 74)))


dados_pib_fiscal <- get_series(c("valor" = 4382), start_date = "1997-01-01") %>%
  mutate(periodo = paste0(format(date, format = "%Y/%m"), "/27")) %>%
  select(periodo, valor)


download.file("https://www.bcb.gov.br/content/estatisticas/Documents/Tabelas_especiais/Divggnp.xls",
              destfile = "../data/divida.xls",
              mode = "wb")
dados_divida_reais <- read_excel("../data/divida.xls",
                           sheet = "R$ milhões",
                           skip = 8,
                           col_names = FALSE,
                           n_max = 48)


dados_ipca <- ts(get_sidra(api = "/t/1737/n1/all/v/2266/p/all/d/v2266%2013")$Valor, start = c(1979, 12), freq = 12) %>%
  window(., start = c(2006, 12))



download.file("https://www.tesourotransparente.gov.br/ckan/dataset/0998f610-bc25-4ce3-b32c-a873447500c2/resource/0402cb77-5e4c-4414-966f-0e87d802a29a/download/2.1.xlsx",
              destfile = "../data/dpf.xlsx",
              mode = "wb")
dados_dpf <- read_excel("../data/dpf.xlsx",
                         skip = 4,
                         col_names = FALSE)


download.file("https://sisweb.tesouro.gov.br/apex/f?p=2810:2::CSV:NO:RP::",
              destfile = "../data/risco.csv",
              mode = "wb")
dados_risco <- rio::import("../data/risco.csv")



# Tratamento de dados -----------------------------------------------------

tesouro <- dados_resultado_tn %>%
  janitor::row_to_names(1, remove_row = TRUE) %>%
  mutate(periodo = format(seq(as.Date("1997/01/27"), length = nrow(dados_resultado_tn)-1, by = "months"), format = "%Y/%m/%d"))


tesouro_acum12m <- tesouro %>%
  mutate_at(-75, ~as.numeric(gsub(",", ".", .))) %>%
  mutate_at(-75, funs(rollapply(., width = 12, FUN = sum, fill = NA, align = "right"))) %>%
  left_join(dados_pib_fiscal, by = "periodo") %>%
  drop_na()


tesouro_acum12m_pib <- tesouro_acum12m %>%
  mutate_at(-(75:76), funs(. / valor*100))


# Box Resultado Primário
res_primario <- tesouro_acum12m_pib %>%
  select(periodo, primario = 68) %>%
  mutate(id = "Resultado Primário")


# Box Receitas e Despesas
receita_desp <- tesouro_acum12m_pib %>%
  select(periodo, `Receita Líquida` = 34, `Despesa Total` = 35) %>%
  pivot_longer(-periodo, names_to = "fluxo", values_to = "value")


# Box Composição das Receitas e Despesas
categorias <- tibble(categoria_1 = c(rep("Receita Total", 21), rep("Transf. por Repartição de Receita", 7), rep("Despesa Total", 29)),
                     categoria_2 = c(rep("Receita Administrada pela RFB, exceto RGPS", 10), "Incentivos Fiscais",
                                      "Arrecadação Líquida para o RGPS", rep("Receitas Não Administradas pela RFB", 9),
                                      "FPM / FPE / IPI-EE", rep("Fundos Constitucionais", 2), "Contribuição do Salário Educação",
                                      "Exploração de Recursos Naturais", "CIDE - Combustíveis", "Demais", "Benefícios Previdenciários",
                                      "Pessoal e Encargos Sociais", rep("Outras Despesas Obrigatórias", 25),
                                      rep("Despesas do Poder Executivo Sujeitas à Programação Financeira", 2)),
                     categoria_3 = c("Imposto de Importação", "IPI", "Imposto sobre a Renda", "IOF", "COFINS", "PIS/PASEP", "CSLL", "CPMF",
                                      "CIDE Combustíveis", "Outras Administradas pela RFB", "Incentivos Fiscais",
                                      "Arrecadação Líquida para o RGPS", "Concessões e Permissões", "Dividendos e Participações",
                                      "Contr. Plano de Seguridade Social do Servidor", "Exploração de Recursos Naturais",
                                      "Receitas Próprias e de Convênios", "Contribuição do Salário Educação",
                                      "Complemento para o FGTS (LC nº 110/01)", "Operações com Ativos", "Demais Receitas",
                                      "FPM / FPE / IPI-EE", "Repasse Total", "Superávit dos Fundos", "Contribuição do Salário Educação",
                                      "Exploração de Recursos Naturais", "CIDE - Combustíveis", "Demais", "Benefícios Previdenciários",
                                      "Pessoal e Encargos Sociais", "Abono e Seguro Desemprego", "Anistiados", "Apoio Fin. EE/MM",
                                      "Auxílio CDE", "Benefícios de Legislação Especial e Indenizações",
                                      "Benefícios de Prestação Continuada da LOAS/RMV", "Complemento para o FGTS (LC nº 110/01)",
                                      "Créditos Extraordinários (exceto PAC)", "Compensação ao RGPS pelas Desonerações da Folha",
                                      "Convênios", "Doações", "Fabricação de Cédulas e Moedas", "Fundef/Fundeb - Complementação da União",
                                      "Fundo Constitucional DF (Custeio e Capital)", "FDA/FDNE",
                                      "Legislativo/Judiciário/MPU/DPU (Custeio e Capital)", "Lei Kandir (LC nº 87/96 e 102/00) e FEX",
                                      "Reserva de Contingência", "Ressarc. Est/Mun. Comb. Fósseis",
                                      "Sentenças Judiciais e Precatórios (Custeio e Capital)", "Subsídios, Subvenções e Proagro",
                                      "Transferências ANA", "Transferências Multas ANEEL", "Impacto Primário do FIES",
                                      "Financiamento de Campanha Eleitoral", "Obrigatórias com Controle de Fluxo", "Discricionárias"))

comp_rec_desp <- tesouro_acum12m %>%
  slice_tail(n = 1) %>%
  select(-c(1,2,15,25,27,34,35,38,64,67:74,76)) %>%
  pivot_longer(-periodo, names_to = "resultado", values_to = "valor") %>%
  select(-resultado) %>%
  bind_cols(categorias) %>%
  mutate(periodo = format(as.Date(periodo, format = "%Y/%m/%d"), format = "%b %Y"))


# Box Saldo Conta Única
divida_reais <- dados_divida_reais %>%
  mutate_at(-1, as.numeric)

divida_reais <- ts(t(divida_reais[,-1]), start = c(2006, 12), freq = 12)
divida_reais <- dados_ipca[length(dados_ipca)-1] * (divida_reais / dados_ipca)

conta_unica <- divida_reais[,31] %>%
  as_tibble() %>%
  rename(disponibilidade = x) %>%
  mutate(periodo = seq(as.Date("2006/12/27"), length = nrow(divida_reais), by = "months"),
         periodo = format(periodo, format = "%Y/%m/%d"),
         disponibilidade = as.numeric(disponibilidade)*-1,
         id = "Saldo da Conta Única")


# Box Estoque da Dívida
estoque_div <- t(dados_dpf[c(1,3),-1]) %>%
  as_tibble() %>%
  mutate(valor = as.numeric(V2),
         periodo = format(seq(as.Date("2006/01/27"), length = ncol(dados_dpf)-1, by = "months"), format = "%Y/%m/%d"),
         id = "Estoque da Dívida") %>%
  select(periodo, valor, id) %>%
  filter(periodo >= "2006/12/01") %>%
  mutate(valor_real = (dados_ipca[length(dados_ipca)-1] * (valor / dados_ipca[1:length(dados_ipca)-1])))


# Box Risco da Dívida
risco  <- dados_risco %>%
  mutate(`Moeda Estrangeira` = gsub("\\-", "--", `Moeda Estrangeira`),
         `Moeda Local` = gsub("\\-", "--", `Moeda Local`))


# Box Carteira de Títulos Públicos
titulos_dpmfi <- t(dados_dpf) %>%
  as_tibble() %>%
  select(7:10, 12:15) %>%
  row_to_names(1) %>%
  mutate(periodo = as.character(seq(as.Date("2006-01-01"), length = ncol(dados_dpf)-1, by = "months"))) %>%
  pivot_longer(-periodo, names_to = "id", values_to = "value") %>%
  mutate(value = as.numeric(format(value, decimal.mark = "."))*1000,
         mes = lubridate::month(periodo, label = TRUE),
         ano = lubridate::year(periodo),
         mes_ano = paste(mes, ano, sep = " "))



# Exportar dados ----------------------------------------------------------

diretorio <- file.path("../data")
save.image(file = file.path(diretorio, "fiscal.Rdata"))
