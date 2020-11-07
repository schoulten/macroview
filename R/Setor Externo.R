### Rotina de Importação e tratamento do Setor Externo




# Pacotes Necessários -----------------------------------------------------

library(tidyverse)
library(rbcb)
library(D3plusR)


# Importação de dados -----------------------------------------------------

dados_externo <- tibble(codigos = c("13078" , "13079")) %>%
  mutate(dados = map(codigos, ~ get_series(.x, as = "tibble")))


# Tratamento de dados -----------------------------------------------------

externo <- dados_externo %>%
  unnest(dados) %>%
  group_by(date) %>%
  pivot_longer(cols = c("13078" , "13079"),
               names_to = "cod",
               values_to = "valor",
               values_drop_na = TRUE) %>%
  select(!cod) %>%
  mutate(id = recode(codigos,
                     "13078" = "Exportação de bens",
                     "13079" = "Importação de bens"))

# Gráficos ----------------------------------------------------------------

#Gráfico referente as Exportações e Importações do RS

dic_ipca_difusao <- list(date = "Periodo",
                         value = "Índice",
                         isolar = "Selecionar",
                         ocultar = "Esconder")

filtro_ipca_difusao <- externo %>%
  filter(row_number() >= (n() - 72)) %>%
  pull(date)


d3plus(data = externo,
       type = "line",
       id = "id",
       width = "100%",
       height = 500, 
       number_text = "US$",
       locale = "pt_BR",
       noformat_number_var = "valor") %>%
  d3plusX(value = "date", grid = FALSE) %>%
  d3plusY(value = "valor", grid = TRUE) %>% 
  d3plusOrder("date", sort = "asc") %>% 
  d3plusAxes(ticks = FALSE, background = list(color = "#FFFFFF", stroke = list(width = 0))) %>%
  d3plusTooltip(c("date", "valor")) %>% 
  d3plusColor("id") %>%
  d3plusLegend(value = TRUE, data = FALSE,
               order = list(value = "id"), size = 75) %>%
  d3plusTitle(value = "Movimentação de Exportações e Importações do RS (BCB)",
              font = list(size = 13, weight = 800))
