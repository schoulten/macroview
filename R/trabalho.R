### Mercado de Trabalho


## Rotina de extração e tratamento de dados



# Pacotes necessários --------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load("PNADcIBGE","tidyverse","lubridate","sidrar","janitor","survey","convey")



# Importação de dados -----------------------------------------------------

variaveis_pnadc <- c("Ano", "Trimestre", "UF", "V1028", "V2007", "V2009", "V2010", "V3007",
                            "VD4001", "VD4002", "VD4009", "VD4020", "VD4035")

dados_pnadc <- get_pnadc(year = format(Sys.Date(), format = "%Y"),
                         quarter = 2,
                         vars = variaveis_pnadc,
                         design = FALSE) ### atualizável


dados_desemprego <- get_sidra(api = "/t/4093/n1/all/v/4090,4092,4099/p/all/c2/6794/d/v4099%201")


dados_ocupacao <- get_sidra(api = "/t/4097/n1/all/v/4090/p/all/c11913/31721,31724,31727,31731,96170,96171")


dados_participacao <- get_sidra(api = "/t/4093/n1/all/v/4096,4097/p/all/c2/all/d/v4096%201,v4097%201")


dados_rendimento <- get_sidra(api = "/t/5437/n1/all/v/5935/p/all/c58/all")



# Tratamento de dados -----------------------------------------------------

# Box Pessoas Ocupadas
totais <- dados_pnadc %>% 
  group_by(VD4002) %>%
  summarise(n = round(sum(V1028)/1e6, 1)) %>% 
  drop_na() %>% 
  pivot_wider(names_from = VD4002, values_from = n) %>% 
  clean_names()


# Box Perfil Ocupação
perfil_ocupacao <- dados_ocupacao %>%
  select(trimestre = "Trimestre (Código)",
         ocupacao = "Posição na ocupação e categoria do emprego no trabalho principal",
         valor = "Valor") %>%
  mutate(trimestre = gsub("(\\d{4})(\\d{2})$","\\1-\\2", trimestre),
         trimestre = gsub("-0", "-T", trimestre),
         ocupacao = gsub(", exclusive trabalhador doméstico", "", ocupacao),
         valor = valor*1000) %>%
  filter(trimestre == max(trimestre))


num_txt_ocupacao <- c("Mil", "Milhão", "Milhões", "Bilhão", "Bilhões")


# Gráfico Taxa de Desemprego
desemprego <- dados_desemprego %>%
  select(trimestre = "Trimestre (Código)",
         variavel = "Variável",
         valor = "Valor") %>%
  mutate(trimestre = gsub("(\\d{4})(\\d{2})$","\\1-\\2", trimestre),
         trimestre = gsub("-0", "-T", trimestre),
         variavel = recode(variavel,
                           "Pessoas de 14 anos ou mais de idade, ocupadas na semana de referência" = "pessoas_ocupadas",
                           "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência" = "pessoas_desocupadas",
                           "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade" = "tx_desemprego"),
         id = "Brasil") %>%
  pivot_wider(id_cols = c(trimestre, id), names_from = variavel, values_from = valor) %>%
  mutate(pessoas_ocupadas = pessoas_ocupadas*1000,
         pessoas_desocupadas = pessoas_desocupadas*1000)
  

# Gráfico Taxa de Desemprego por Gênero e Cor/Raça
desemprego_gencor <- dados_pnadc %>%
  group_by(V2007, V2010, VD4002) %>%
  summarise(n = sum(V1028)) %>%
  drop_na() %>% 
  ungroup() %>% 
  pivot_wider(names_from = VD4002, values_from = n) %>% 
  clean_names() %>%
  replace_na(list(pessoas_desocupadas = 0)) %>%
  group_by(v2007, v2010) %>%
  summarise(pessoas_ocupadas = sum(pessoas_ocupadas),
            pessoas_desocupadas = sum(pessoas_desocupadas),
            tx_desemprego = sum(pessoas_desocupadas)/sum(pessoas_desocupadas + pessoas_ocupadas) * 100) %>% 
  filter(v2010 != "Ignorado") %>% 
  mutate(trimestre = paste(tail(dados_pnadc$Ano, length(tx_desemprego)), "-T",
                               tail(dados_pnadc$Trimestre, length(tx_desemprego)), sep = ""))


genero_attr <- data.frame(
  v2007 = c("Homem", "Mulher"),
  cor = c("#2980b9", "#9b59b6"),
  icon = c("https://user-images.githubusercontent.com/64612880/94350836-cfcde080-0028-11eb-989b-5129c80a9902.png",
           "https://user-images.githubusercontent.com/64612880/94350835-cf354a00-0028-11eb-949b-00960f64d91a.png"))


# Mapa do Desemprego
uf_sigla <- tibble(
  uf = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", 
         "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", 
         "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais", 
         "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", 
         "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", 
         "Mato Grosso", "Goiás", "Distrito Federal"),
  uf_id = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", 
             "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", 
             "SC", "RS", "MS", "MT", "GO", "DF"))


dicionario_desemprego_map <- list(
  tx_desemprego = "Taxa de Desemprego",
  pessoas_ocupadas = "Pessoas Ocupadas",
  pessoas_desocupadas = "Pessoas Desocupadas",
  trimestre = "Trimestre",
  isolar = "Isolar",
  ocultar = "Ocultar")


num_txt_desemprego_map <- c("Mil", "Milhão", "Milhões", "Bilhão", "Bilhões")


desemprego_map <- dados_pnadc %>% 
  group_by(UF, VD4002) %>%
  summarise(n = sum(V1028)) %>%
  drop_na() %>% 
  ungroup() %>% 
  pivot_wider(names_from = VD4002, values_from = n) %>% 
  clean_names() %>% 
  replace_na(list(pessoas_desocupadas = 0, pessoas_ocupadas = 0)) %>% 
  group_by(uf) %>% 
  summarise(pessoas_ocupadas = sum(pessoas_ocupadas),
            pessoas_desocupadas = sum(pessoas_desocupadas),
            tx_desemprego = sum(pessoas_desocupadas)/sum(pessoas_desocupadas + pessoas_ocupadas) * 100) %>% 
  arrange(-tx_desemprego) %>% 
  left_join(uf_sigla, by = "uf") %>%
  mutate(trimestre = paste(tail(dados_pnadc$Ano, length(uf)), "-T",
                               tail(dados_pnadc$Trimestre, length(uf)), sep = ""))


# Gráfico Nível da Ocupação
nivel_ocupacao <- dados_participacao %>%
  select(trimestre = "Trimestre (Código)",
         variavel = "Variável",
         sexo = "Sexo",
         valor = "Valor") %>%
  mutate(trimestre = gsub("(\\d{4})(\\d{2})$","\\1-\\2", trimestre),
         trimestre = gsub("-0", "-T", trimestre)) %>%
  filter(variavel == "Nível de ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade") %>%
  select(-variavel)


# Box Taxa de Participação
taxa_participacao <- dados_participacao %>%
  select(trimestre = "Trimestre (Código)",
         variavel = "Variável",
         sexo = "Sexo",
         valor = "Valor") %>%
  mutate(trimestre = gsub("(\\d{4})(\\d{2})$","\\1-\\2", trimestre),
         trimestre = gsub("-0", "-T", trimestre)) %>%
  filter(variavel == "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade") %>%
  select(-variavel)


# Box Rendimento
rendimento <- dados_rendimento %>%
  select(trimestre = "Trimestre (Código)",
         idade = "Grupo de idade",
         valor = "Valor") %>%
  mutate(trimestre = gsub("(\\d{4})(\\d{2})$","\\1-\\2", trimestre),
         trimestre = gsub("-0", "-T", trimestre),
         idade = gsub(" anos", "", idade),
         id = rep(c("1","2","3","4","5","6"), length(trimestre)/6)) %>%
  drop_na()


# Box Índice de Gini Estadual
dados_pnadc_gini <- dados_pnadc %>%
  pnadc_design() %>%
  convey_prep()

gini <- svyby(~VD4020, by = ~UF, dados_pnadc_gini, svygini, na.rm = TRUE) %>%
  select(uf = UF, VD4020) %>%
  mutate(id = "gini") %>%
  left_join(uf_sigla, by = "uf") %>%
  as_tibble() %>%
  mutate(trimestre = paste0(dados_pnadc$Ano[1], "-T", dados_pnadc$Trimestre[1]))



# Exportar dados ----------------------------------------------------------

diretorio <- file.path("../data")
save.image(file = file.path(diretorio, "trabalho.Rdata"))
