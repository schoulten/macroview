### Inflação


## Rotina de extração e tratamento de dados



# Pacotes necessários --------------------------------------------------

library(sidrar)
library(tidyverse)
library(rbcb)



# Importação de dados -----------------------------------------------------

dados_ipca <- get_sidra(api = "/t/1737/n1/all/v/63,69,2263,2264,2265/p/all/d/v63%202,v69%202,v2263%202,v2264%202,v2265%202") %>%
  select(periodo = "Mês (Código)", taxa = "Variável", valor = "Valor") %>%
  mutate(taxa = recode(taxa, "IPCA - Variação mensal" = "Mensal",
                             "IPCA - Variação acumulada em 12 meses" = "Acumulada em 12 meses",
                             "IPCA - Variação acumulada no ano" = "Acumulada no ano",
                             "IPCA - Variação acumulada em 3 meses" = "Acumulada em 3 meses",
                             "IPCA - Variação acumulada em 6 meses" = "Acumulada em 6 meses"),
         periodo = as.Date(paste0(as.character(periodo), "27"), format = "%Y%m%d"),
         periodo = format(periodo, format = "%Y/%m/%d"),
         valor = as.numeric(format(valor, decimal.mark = "."))) %>%
  drop_na()


dados_meta_inf <- get_series(c(valor = 13521), start_date = "1999-01-01")


dados_ipca_grupos <- get_sidra(api = "/t/7060/n1/all/v/63,69/p/all/c315/7169,7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202,v69%202") %>%
  select(periodo = "Mês (Código)",
         taxa = "Variável",
         Indicador = "Geral, grupo, subgrupo, item e subitem",
         valor = "Valor") %>%
  mutate(Indicador = recode(Indicador,
                        "Índice geral" = "IPCA",
                        "1.Alimentação e bebidas" = "Alimentação e bebidas",
                        "2.Habitação" = "Habitação",
                        "3.Artigos de residência" = "Artigos de residência",
                        "4.Vestuário" = "Vestuário",
                        "5.Transportes" = "Transportes",
                        "6.Saúde e cuidados pessoais" = "Saúde e cuidados pessoais",
                        "7.Despesas pessoais" = "Despesas pessoais",
                        "8.Educação" = "Educação",
                        "9.Comunicação" = "Comunicação"),
         taxa = recode(taxa, "IPCA - Variação mensal" = "Mensal",
                             "IPCA - Variação acumulada no ano" = "Acumulada no Ano"),
         periodo = as.Date(paste0(as.character(periodo), "27"), format = "%Y%m%d"),
         periodo = format(periodo, format = "%Y/%m/%d"),
         valor = format(valor, decimal.mark = "."))


dados_ipca_regiao <- get_sidra(api = "/t/7060/n7/all/v/69/p/all/c315/7169/d/v69%202") %>%
  select(periodo = "Mês (Código)",
         regiao = "Região Metropolitana",
         valor = "Valor") %>%
  mutate(periodo = as.Date(paste0(as.character(periodo), "27"), format = "%Y%m%d"),
         periodo = format(periodo, format = "%Y/%m/%d")) %>%
  as_tibble()


dados_ipca_difusao <- get_series(c(value = 21379), start_date = "2001-01-01")


variaveis_ipca_nucleos <- list("IPCA-EX0" = 11427, "IPCA-EX1" = 16121, "IPCA-DP" = 16122, "IPCA-MA" = 11426, "IPCA-MS" = 4466)
dados_ipca_nucleos <- get_series(variaveis_ipca_nucleos, start_date = "2001-01-01", as = "tibble")


variaveis_igp <- list("IGP-M" = 189, "IGP-DI" = 190, "IGP-10" = 7447)
dados_igp <- get_series(variaveis_igp, start_date = "1994-09-01", as = "tibble")



# Tratamento de dados -----------------------------------------------------

# Box Inflação em 12 meses
ipca_12m <- dados_ipca %>%
  filter(taxa == "Acumulada em 12 meses") %>%
  slice_tail(n = 1)


# Box Meta de Inflação ano corrente
meta_inf <- dados_meta_inf %>%
  mutate(date = format(date, format = "%Y")) %>%
  filter(date == format(Sys.time(), format = "%Y")) %>%
  slice_tail(n = 1)


# Box tabela de variações do IPCA
ipca_grupos_tabela <- dados_ipca_grupos %>%
  filter(periodo == max(periodo)) %>%
  select(-periodo) %>%
  pivot_wider(names_from = "taxa", values_from = "valor", id_cols = "Indicador") %>%
  mutate(Mensal = as.numeric(Mensal),
         `Acumulada no Ano` = as.numeric(`Acumulada no Ano`))


ipca.grupos <- dados_ipca_grupos %>%
  filter(periodo == max(periodo),
         Indicador == "IPCA",
         taxa == "Mensal") %>%
  select(periodo) %>%
  mutate(periodo = paste0("Nota: dados de ", format(as.Date(periodo), format = "%b/%Y"), ".")) %>%
  pull(periodo)


# Box gráfico variações IPCA
ipca_grafico <- dados_ipca %>%
  filter(taxa %in% c("Mensal", "Acumulada em 12 meses"))
  

# Box gráfico Índice de difusão do IPCA
ipca_difusao <- dados_ipca_difusao %>%
  mutate(id = "Índice de Difusão",
         date = format(date, format = "%Y%m"),
         date = as.Date(paste0(as.character(date), "27"), format = "%Y%m%d"),
         date = format(date, format = "%Y/%m/%d"))


# Box gráfico Núcleos do IPCA
ipca_nucleos <- dados_ipca_nucleos %>%
  reduce(inner_join, by = "date") %>%
  pivot_longer(!date, names_to = "nucleo", values_to = "valor") %>%
  mutate(date = format(date, format = "%Y%m"),
         date = as.Date(paste0(as.character(date), "27"), format = "%Y%m%d"),
         date = format(date, format = "%Y/%m/%d"))


# Box gráfico Índices Gerais de Preço - IGP
igp_grafico <- dados_igp %>%
  reduce(inner_join, by = "date") %>%
  drop_na() %>%
  pivot_longer(!date, names_to = "igp", values_to = "valor") %>%
  mutate(date = format(date, format = "%Y%m"),
         date = as.Date(paste0(as.character(date), "27"), format='%Y%m%d'),
         date = format(date, format = "%Y/%m/%d"))



# Exportar dados ----------------------------------------------------------

diretorio <- file.path("../data")
save.image(file = file.path(diretorio, "inflacao.Rdata"))
