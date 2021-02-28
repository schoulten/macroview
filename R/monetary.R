### Monetary policy ###


# This R code provides the extraction and data wrangling of macroeconomic
# variables referring to the Brazilian Monetary policy


# Packages ----------------------------------------------------------------


# Install/load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "tidyverse",
  "GetBCBData",
  "rbcb",
  "ecoseries",
  "lubridate",
  "zoo",
  "sidrar",
  "GetTDData",
  "quantmod",
  "tibbletime"
  )



# Importação de dados -----------------------------------------------------

dados_juros <- get_series(c("SELIC Meta" = 432, "SELIC Efetiva" = 1178, "CDI" = 4389), start_date = "2001/11/07", as = "tibble")


dados_inf_expec <- get_twelve_months_inflation_expectations("IPCA", start_date = "07/11/2001")


dados_swap <- series_ipeadata("1900214364", periodicity = "M")$serie_1900214364
ipeadata()


dados_inf_expec_m <- series_ipeadata("1693254712", periodicity = "M")$serie_1693254712


dados_selic_aa <- get_series(c("selic_aa" = 4189), start_date = "2001/07/01") %>%
  rename(data = date)


dados_moedas <- tibble(currency = c("USD", "EUR", "ARS", "MXN", "CNY", "TRY", "RUB", "INR", "SAR", "ZAR")) %>%
  mutate(dados = map(currency, ~ get_currency(.x, as = "tibble", start_date = "2006-09-01", end_date = Sys.Date())))


dados_selic_expec <- get_annual_market_expectations("Meta para taxa over-selic", end_date = Sys.Date())


dados_ettj <- GetTDData::get.yield.curve()


quantmod::getSymbols("^BVSP", src = "yahoo")


dados_embi <- series_ipeadata("40940", periodicity = "D")$serie_40940


dados_ipca <- get_sidra(api = "/t/1737/n1/all/v/63,69,2263,2264,2265/p/all/d/v63%202,v69%202,v2263%202,v2264%202,v2265%202") %>%
  select(data = "Mês (Código)", taxa = "Variável", valor = "Valor") %>%
  mutate(data = paste0(data, "01") %>% as.Date(format = "%Y%m%d")) %>%
  drop_na()



# Tratamento de dados -----------------------------------------------------

# Box Meta da Taxa de Juros SELIC (% a.a.)
selic <- dados_juros$`SELIC Meta` %>%
  filter(date == max(date)) %>%
  mutate(`SELIC Meta` = paste0(`SELIC Meta`, "%")) %>% 
  pull(`SELIC Meta`)


# Box Expectativas de Inflação
inf_expec <- dados_inf_expec %>%
  filter(smoothed == "S" & base == "0") %>%
  select(date, indic, mean) %>%
  group_by(month = month(date), year = year(date)) %>%
  slice(which.max(day(date))) %>%
  ungroup() %>%
  mutate(mes_ano = paste(month(date, label = TRUE), year(date), sep = " "),
         date = paste0(format(as.Date(date), format = "%Y/%m"), "/27", sep = "")) %>%
  select(-month, -year)


# Box Evolução das Taxas de Juros
juros <- dados_juros %>%
  reduce(inner_join, by = "date") %>%
  pivot_longer(!date, names_to = "variavel", values_to = "value") %>%
  group_by(variavel, month = month(date), year = year(date)) %>%
  slice(which.max(day(date))) %>%
  ungroup() %>%
  mutate(mes_ano = paste(month(date, label = TRUE), year(date), sep = " "),
         date = paste0(format(as.Date(date), format = "%Y/%m"), "/27", sep = "")) %>%
  select(-month, -year)


# Box Juros Real ex-ante e ex-post
ex_ante <- inner_join(dados_swap, dados_inf_expec_m, by = "data") %>%
  rename(swap = valor.x, inf_expec = valor.y) %>%
  mutate(valor = (((1+(swap/100))/(1+(inf_expec/100)))-1)*100,
         data = paste0(format(data, format = "%Y/%m"), "/27"),
         id = "Ex-ante")

ex_post <- dados_ipca %>%
  filter(taxa == "IPCA - Variação acumulada em 12 meses") %>%
  select(data, ipca_12m = valor) %>%
  inner_join(dados_selic_aa, by = "data") %>%
  mutate(valor = (((1+(selic_aa/100))/(1+(ipca_12m/100)))-1)*100,
         data = paste0(format(data, format = "%Y/%m"), "/27"),
         id = "Ex-post")

juros_real <- bind_rows(ex_ante[c(1,4:5)], ex_post[c(1,4:5)])


# Tabela Mercado Cambial
moedas_nomes <- tibble(currency = c("USD", "EUR", "ARS", "MXN", "CNY", "TRY", "RUB", "INR", "SAR", "ZAR"),
                       names = c('Dólar Americano', 'Euro', 'Peso Argentino', 'Peso Mexicano', 'Renminbi Chinês', 'Lira Turca', 'Rublo Russo', 'Rupia Indiana', 'Rial Saudita', 'Rand Sul-africano'))

moedas_nomes_tabela <- c("Moeda", paste0("Cotação (", format(tail(dados_moedas[[2]][[1]]$date, 1), format = "%b/%y"), ")"), 'Mensal (%)', 'Trimestral (%)', 'Interanual (%)', '12 meses (%)')

moedas <- dados_moedas %>%
  unnest(dados) %>%
  group_by(currency) %>%
  tibbletime::as_tbl_time(index = date) %>%
  tibbletime::collapse_by(period = "monthly") %>%
  as_tibble() %>%
  group_by(currency, date) %>%
  summarise(ask_monthly = mean(ask)) %>%
  mutate(previous = lag(ask_monthly, 12), 
         mom = (ask_monthly/lag(ask_monthly)-1)*100,
         qyoy = (rollsum(ask_monthly, 3, fill = "NA", align = "right") / 
                  rollsum(previous, 3, fill = "NA", align = "right")-1)*100,
         myoy = (ask_monthly/lag(ask_monthly, 12)-1)*100,
         yoy = (rollsum(ask_monthly, 12, fill = "NA", align = "right") / 
                  rollsum(previous, 12, fill = "NA", align = "right")-1)*100) %>%
  group_by(currency) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate_if(is.numeric, round, 2) %>%
  left_join(moedas_nomes, by = "currency") %>%
  arrange(order(moedas_nomes$currency)) %>%
  select(9, 3, 5:8) %>%
  rename_all(~moedas_nomes_tabela)

moedas_footnote <- paste0("Nota: valores a partir de dados diários mensalizados, atualizado até ",
                         paste0(format(tail(dados_moedas[[2]][[1]]$date, 1), format = "%d/%b/%Y")),
                         ".")


# Box Expectativas de Juros (Focus)
selic_expec <- dados_selic_expec %>%
  filter(indic_detail == "Fim do ano" & reference_year == format(Sys.Date(), "%Y")) %>%
  select(date, indic, indic_detail, reference_year, mean) %>%
  mutate(date = format(date, format = "%Y/%m/%d")) %>%
  group_by(month = month(date), year = year(date)) %>%
  slice(which.max(day(date))) %>%
  ungroup() %>%
  mutate(mes_ano = paste(month(date, label = TRUE), year(date), sep = " "),
         date = paste0(format(as.Date(date), format = "%Y/%m"), "/27")) %>%
  select(-month, -year)


# Box ETTJ
ettj <- dados_ettj %>%
  mutate(data_consulta = paste(day(as.Date(current.date)),
                               month(as.Date(current.date), label = TRUE),
                               year(as.Date(current.date)), sep = " "),
         data_ref = format(as.Date(ref.date), format = "%Y/%m/%d"),
         data_dmy = paste(day(as.Date(data_ref)),
                          month(as.Date(data_ref), label = TRUE),
                          year(as.Date(data_ref)), sep = " "),
         n.biz.days = as.character(n.biz.days),
         value = round(value, 2),
         id = "ETTJ IPCA") %>%
  filter(type == "real_return") %>%
  select(data_consulta, data_ref, valor = value, dias_uteis = n.biz.days, id, data_dmy)
  

# Box Ibovespa
ibov <- tibble(periodo = as.Date(time(BVSP)),
               pontos = as.numeric(BVSP$BVSP.Close),
               id = "IBOVESPA") %>%
  mutate(periodo = format(periodo, format = "%Y/%m/%d")) %>%
  group_by(month = month(periodo),
           year = year(periodo)) %>%
  slice(which.max(day(periodo))) %>%
  ungroup() %>%
  mutate(mes_ano = paste(month(periodo, label = TRUE), year(periodo), sep = " "),
         periodo = paste0(format(as.Date(periodo), format = "%Y/%m"), "/27", sep = "")) %>%
  select(-month, -year)


# Box Risco-País (EMBI+)
embi <- dados_embi %>%
  mutate(data = format(data, format = "%Y/%m/%d"),
         id = "EMBI+ Risco-Brasil") %>%
  group_by(month = month(data), year = year(data)) %>%
  slice(which.max(day(data))) %>%
  ungroup() %>%
  mutate(mes_ano = paste(month(data, label = TRUE), year(data), sep = " "),
         data = paste0(format(as.Date(data), format = "%Y/%m"), "/27", sep = "")) %>%
  select(-month, -year)



# Exportar dados ----------------------------------------------------------

diretorio <- file.path("../data")
save.image(file = file.path(diretorio, "monetaria.Rdata"))
