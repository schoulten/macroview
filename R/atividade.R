### Nível de atividade


## Rotina de extração e tratamento de dados



# Pacotes necessários -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load("tidyverse","sidrar","readxl","readxl","rbcb", "zoo")



# Importação de dados -----------------------------------------------------

dados_pib <- get_sidra(
  api = "/t/5932/n1/all/v/all/p/all/c11255/90687,90691,90696,90707,93404,93405,93406,93407,93408/d/v6561%201,v6562%201,v6563%201,v6564%201") %>%
  select(taxa = "Variável", setores = "Setores e subsetores", trimestre = "Trimestre (Código)", valor = "Valor")


dados_pib_valores <- get_sidra(api = "/t/1846/n1/all/v/all/p/all/c11255/90687,90691,90696,90707,93404,93405,93406,93407,93408/d/v585%200") %>%
  select(setores = "Setores e subsetores", trimestre = "Trimestre (Código)", valor = "Valor", medida = "Unidade de Medida")


url_icva <- "https://apicatalog.mziq.com/filemanager/v2/d/4d1ebe73-b068-4443-992a-3d72d573238c/3e864198-0b72-c970-1771-80cd8c338a30?origin=2"
download.file(url = url_icva, destfile = "icva.xlsx", mode = "wb") %>%
  file.copy(from = "./icva.xlsx", to   = "../data")
file.remove("./icva.xlsx")
dados_icva <- read_excel("../data/icva.xlsx")


url_anfavea <- "http://www.anfavea.com.br/docs/SeriesTemporais_Autoveiculos.xlsm"
download.file(url = url_anfavea, destfile = file.path("../data", basename(url_anfavea)), mode = "wb")
dados_veiculos <- read_excel(file.path("../data", basename(url_anfavea)), skip = 4)


dados_uci_sa <- get_series(28561, start_date = "2001-01-01")


variaveis_ibc <- list(Brasil = 24364, Norte = 25407, Nordeste = 25389, "Centro-Oeste" = 25382, Sudeste = 25395, Sul = 25403)
dados_ibc <- get_series(variaveis_ibc, start_date = "2003-01-27", as = "tibble")


pmc_restrito <- get_sidra(api = "/t/3416/n1/all/v/all/p/all/c11046/90668/d/v564%201,v565%201") %>%
  select(indice = "Variável", periodo = "Mês (Código)", taxa = "Valor") %>%
  mutate(indice = recode(indice, "Índice de volume de vendas no comércio varejista" = "Volume Restrito",
                         "Índice de receita nominal de vendas no comércio varejista" = "Receita Restrito"))


pmc_ampliado <- get_sidra(api = "/t/3417/n1/all/v/all/p/all/c11046/90668/d/v1186%201,v1190%201") %>%
  select(indice = "Variável", periodo = "Mês (Código)", taxa = "Valor") %>%
  mutate(indice = recode(indice, "Índice de volume de vendas no comércio varejista ampliado" = "Volume Ampliado",
                         "Índice de receita nominal de vendas no comércio varejista ampliado" = "Receita Ampliado"))


dados_pms <- get_sidra(api = "/t/6442/n1/all/v/all/p/all/c11046/90668/d/v8676%201,v8677%201") %>%
  select(indice = "Variável", periodo = "Mês (Código)", taxa = "Valor") %>%
  mutate(indice = recode(indice, "Índice de receita nominal de serviços" = "Receita",
                         "Índice de volume de serviços" = "Volume"),
         periodo = as.Date(paste0(as.character(periodo), "27"), format = "%Y%m%d"))


dados_pim <- get_sidra(api = "/t/3653/n1/all/v/3139/p/all/c544/129314,129315,129316,129338/d/v3139%201") %>%
  select(periodo = "Mês (Código)", atividade = "Seções e atividades industriais (CNAE 2.0)", taxa = "Valor") %>%
  mutate(atividade = recode(atividade, "1 Indústria geral" = "Indústria Geral",
                         "2 Indústrias extrativas" = "Indústrias Extrativas",
                         "3 Indústrias de transformação" = "Indústrias de Transformação",
                         "3.29 Fabricação de veículos automotores, reboques e carrocerias" = "Fabricação de Veículos"),
         periodo = as.Date(paste0(as.character(periodo), "27"), format = "%Y%m%d"))



# Tratamento de dados -----------------------------------------------------


# Box do Crescimento do PIB
pib_crescimento <- dados_pib %>%
  filter(setores == "PIB a preços de mercado",
         taxa == "Taxa acumulada em quatro trimestres (em relação ao mesmo período do ano anterior)") %>%
  slice_tail(n = 1)


# Box do PIB nominal
pib_nominal <- dados_pib_valores %>%
  filter(setores == "PIB a preços de mercado") %>%
  summarise(n = round(sum(tail(.$valor, 4)) / 1000000, digits = 2))


# Box do Indicador Cielo
icva <- dados_icva %>%
  rename_all(~c("date", "nominal", "nominal_sa", "real", "real_sa")) %>%
  mutate(real_sa = real_sa*100) %>%
  select(date, real_sa) %>%
  slice_tail(n = 1)


# Box da Produção de Veículos
veiculos <- dados_veiculos %>%
  select(...1, `Produção...5`) %>%
  rename(dates = ...1, producao = `Produção...5`) %>%
  na_if(0) %>%
  drop_na()

  
# Box da UCI
uci <- slice_tail(dados_uci_sa[2], n = 1)


# Tabela das óticas do PIB
ordem <- c("PIB", "Agropecuária", "Indústria", "Serviços", "Consumo das Famílias", "Consumo do Governo", "FBCF", "Exportação", "Importação (-)")


pib_tabela <- dados_pib %>%
  filter(trimestre == max(trimestre)) %>%
  pivot_wider(id_cols = setores, names_from = taxa, values_from = valor) %>%
  select(1, 5, 2, 4, 3) %>%
  mutate(setores = mgsub::mgsub(setores,
                                c(" - total", " a preços de mercado", "Despesa de consumo das famílias",
                                  "Despesa de consumo da administração pública", "Formação bruta de capital fixo",
                                  " de bens e serviços", " (-)"),
                                c("", "", "Consumo das Famílias", "Consumo do Governo", "FBCF", "", "")),
         setores = factor(setores, levels = ordem)) %>%
  rename_all(~c("Indicador", "Margem", "Interanual", "Acumulado no ano", "Anual")) %>%
  arrange(Indicador)


trim.pib <- slice_tail(dados_pib, n = 1) %>%
  mutate(trimestre = gsub("(\\d{4})(\\d{2})$","\\1-\\2", trimestre),
         trimestre = gsub("-0", "-T", trimestre),
         nota = paste("Nota: dados de ", trimestre, ".", sep = "")) %>%
  pull(nota)


# Box gráfico de linhas do PIB
pib_grafico <- dados_pib %>%
  filter(taxa == "Taxa acumulada em quatro trimestres (em relação ao mesmo período do ano anterior)" &
           setores %in% c("Agropecuária - total", "Indústria - total", "Serviços - total", "PIB a preços de mercado")) %>%
  select(trimestre, setores, valor) %>%
  mutate(trimestre = gsub("(\\d{4})(\\d{2})$","\\1-\\2", trimestre),
         trimestre = gsub("-0", "-T", trimestre),
         setores = mgsub::mgsub(setores, c(" - total", " a preços de mercado"), c("", "")))


# Box gráfico barra IBCR
ibc_grafico <- dados_ibc %>%
  reduce(inner_join, by = "date") %>%
  pivot_longer(!date, names_to = "ibcr", values_to = "indice") %>%
  group_by(ibcr) %>%
  mutate(previous_year = lag(indice, 12),
         yoy = (rollsum(indice, 12, fill = "NA", align = "right") / 
                  rollsum(previous_year, 12, fill = "NA", align = "right") -1) * 100,
         yoy = round(yoy, digits = 2),
         date = as.character(format(date, format = "%Y/%m/%d"))) %>%
  drop_na()


# Box PMC
pmc <- bind_rows(pmc_ampliado, pmc_restrito) %>%
  drop_na() %>%
  mutate(periodo = format(as.Date(paste0(periodo, "27"), format = "%Y%m%d"), format = "%Y/%m/%d"))


# Box PMS
pms <- dados_pms %>%
  drop_na() %>%
  mutate(periodo = format(as.Date(periodo), format = "%Y/%m/%d"))


# Box PIM
pim <- dados_pim %>%
  drop_na() %>%
  mutate(periodo = format(as.Date(periodo), format = "%Y/%m/%d"))



# Exportar dados ----------------------------------------------------------

diretorio <- file.path("../data")
save.image(file = file.path(diretorio, "atividade.Rdata"))
