### Economia Internacional


## Rotina de extração e tratamento de dados



# Pacotes necessários --------------------------------------------------

library(OECD)
library(tidyverse)



# Importação de dados -----------------------------------------------------

# PIB OCDE
pib_estr <- get_data_structure("QNA")
dados_pib_ocde <- get_dataset(
                        dataset = "QNA",
                        filter = list("AUS+CAN+CHL+FRA+DEU+ITA+JPN+MEX+ESP+TUR+USA+ARG+BRA+CHN+CRI+IND+RUS.B1_GE.GYSA.A+Q"),
                        pre_formatted = TRUE)


# Taxa de Desemprego OCDE
desemprego_estr <- get_data_structure("STLABOUR")
dados_desemprego_ocde <- get_dataset(
                               dataset = "STLABOUR",
                               filter = list("AUS+CAN+CHL+FRA+DEU+ITA+JPN+MEX+ESP+TUR+USA.LRHUTTTT.STSA.M"),
                               pre_formatted = TRUE)


# Inflação OCDE
inflacao_estr <- get_data_structure("MEI")
dados_inflacao_ocde <- get_dataset(
                             dataset = "MEI",
                             filter = list("AUS+CAN+CHL+FRA+DEU+ITA+JPN+MEX+ESP+TUR+USA+ARG+BRA+CHN+CRI+IND+RUS+BRIICS.CPALTT01.GY.M"),
                             pre_formatted = TRUE)


# Taxa de Juros OCDE
dados_juros_ocde <- get_dataset(
                          dataset = "MEI",
                          filter = list("AUS+CAN+CHL+FRA+DEU+ITA+JPN+MEX+ESP+TUR+USA+ARG+BRA+CHN+CRI+IND+RUS+BRIICS.IRSTCB01+IRSTCI01.ST+STSA.M"),
                          pre_formatted = TRUE)



# Tratamento de dados -----------------------------------------------------

# Box PIB OCDE
pib_ocde_cod <- pib_estr$LOCATION %>%
  rename(codigo = id, pais = label)

pib_ocde <- dados_pib_ocde %>%
  filter(FREQUENCY == "Q") %>%
  select(codigo = LOCATION, periodo = obsTime, valor = obsValue, observacao = contains("OBS_STATUS")) %>%
  left_join(pib_ocde_cod, by = "codigo") %>%
  mutate(periodo = gsub("Q", "T", periodo),
         across(any_of("observacao"), ~mgsub::mgsub(observacao, c("E", "P"), c("Valor estimado", "Valor provisório")))) %>%
  as_tibble()


# Box Inflação OCDE
inflacao_ocde_cod <- inflacao_estr$LOCATION %>%
  rename(codigo = id, pais = label)

inflacao_ocde <- dados_inflacao_ocde %>%
  select(codigo = LOCATION, periodo = obsTime, valor = obsValue, observacao = contains("OBS_STATUS")) %>%
  left_join(inflacao_ocde_cod, by = "codigo") %>%
  mutate(periodo = paste0(gsub("-", "/", periodo), "/27"),
         across(any_of("observacao"), ~gsub("P", "Valor provisório", observacao))) %>%
  as_tibble()


# Box Taxa de Desemprego OCDE
desemprego_ocde_cod <- desemprego_estr$LOCATION %>%
  rename(codigo = id, pais = label)

desemprego_ocde <- dados_desemprego_ocde %>%
  select(codigo = LOCATION, periodo = obsTime, valor = obsValue, observacao = contains("OBS_STATUS")) %>%
  left_join(desemprego_ocde_cod, by = "codigo") %>%
  mutate(periodo = paste0(gsub("-", "/", periodo), "/27"),
         across(any_of("observacao"), ~mgsub::mgsub(observacao, c("E", "B"), c("Valor estimado", "Break")))) %>%
  as_tibble()


# Box Taxa de Juros OCDE
juros_ocde <- dados_juros_ocde %>%
  filter(SUBJECT == "IRSTCI01") %>%
  select(codigo = LOCATION, periodo = obsTime, valor = obsValue, observacao = contains("OBS_STATUS")) %>%
  left_join(inflacao_ocde_cod, by = "codigo") %>%
  mutate(periodo = paste0(gsub("-", "/", periodo), "/27"),
         across(any_of("observacao"), ~gsub("B", "Break", observacao))) %>%
  as_tibble()



# Exportar dados ----------------------------------------------------------

diretorio <- file.path("../data")
save.image(file = file.path(diretorio, "internacional.Rdata"))
