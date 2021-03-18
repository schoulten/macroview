### Labor market ###


# This R code provides the extraction and data wrangling of macroeconomic
# variables referring to the Brazilian labour market


# Packages ----------------------------------------------------------------


# Install/load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "PNADcIBGE",
  "tidyverse",
  "lubridate",
  "sidrar",
  "janitor",
  "survey",
  "convey",
  "rvest"
  )



# Parameters --------------------------------------------------------------


# Parameters used in the code to import, download or cleaning data


# Load useful functions
source("./R/utils.R")


# List of parameters to get data from IBGE website
api_ibge <- dplyr::lst(
  
  # Continuous National Household Sample Survey - Quarterly Disclosure (PNADC-T)
  
  # Query variables
  api_pnadc_variables = c(
    "Ano", "Trimestre", "UF", "V1028", "V2007", "V2009", "V2010",
    "V3007", "VD4001", "VD4002", "VD4009", "VD4020", "VD4035"
    ),
  
  # Last available quarter and year
  api_pnadc_dates = xml2::read_html("https://sidra.ibge.gov.br/home/pnadct/brasil") %>%
    rvest::html_nodes(".titulo-aba") %>%
    rvest::html_text() %>%
    stringr::str_extract("(Divulgação Trimestral) - \\d{1}. trimestre \\d{4}") %>% 
    dplyr::lst(
      quarter = str_extract(., "\\d{1}") %>% as.numeric(),
      year = str_extract(., "\\d{4}") %>% as.numeric()
      ),
  
  
  # Unemployment rate (PNADC-T)
  api_unemployment = "/t/4093/n1/all/v/4090,4092,4099/p/all/c2/6794/d/v4099%201",
  
  # Employment by occupation / category in the main job (PNADC-T)
  api_employment = "/t/4097/n1/all/v/4090/p/all/c11913/31721,31724,31727,31731,96170,96171",
  
  # Labor force participation rate and occupation level (PNADC-T)
  api_labour_force = "/t/4093/n1/all/v/4096,4097/p/all/c2/all/d/v4096%201,v4097%201",
  
  # Average real income from work, by age group (PNADC-T)
  api_income = "/t/5437/n1/all/v/5935/p/all/c58/all"
  
  )



# Import data -------------------------------------------------------------


# This section performs the import of data from different sources


# PNADC-T microdata from IBGE
raw_pnadct <- PNADcIBGE::get_pnadc(
  year    = api_ibge$api_pnadc_dates$year,
  quarter = api_ibge$api_pnadc_dates$quarter,
  vars    = api_ibge$api_pnadc_variables,
  design  = FALSE
  )

# Unemployment rate (PNADC-T)
raw_unemployment <- get_sidra(api = api_ibge$api_unemployment)

# Employment by occupation / category in the main job (PNADC-T)
raw_employment <- get_sidra(api = api_ibge$api_employment)

# Labor force participation rate and occupation level (PNADC-T)
raw_labor_force <- get_sidra(api = api_ibge$api_labour_force)

# Average real income from work, by age group (PNADC-T)
raw_income <- get_sidra(api = api_ibge$api_income)




# Data wrangling ----------------------------------------------------------


# This section performs data wrangling


# Number of employed and unemployed people
big_numbers <- raw_pnadct %>% 
  group_by(VD4002) %>%
  summarise(n = round(sum(V1028) / 1e6, 1)) %>%
  drop_na() %>% 
  pivot_wider(names_from = VD4002, values_from = n) %>% 
  rename_with(~c("employed", "unemployed"))


# Employment by category in the main job
employment_category <- raw_employment %>%
  select(
    date     = "Trimestre (Código)",
    variable = "Posição na ocupação e categoria do emprego no trabalho principal",
    value    = "Valor"
    ) %>%
  filter(date == max(as.numeric(date))) %>%
  mutate(
    date     = stringr::str_replace(date, "(\\d{4})0(\\d{1}$)", "\\1 Q\\2"),
    variable = recode(
      variable,
      "Empregado no setor privado, exclusive trabalhador doméstico" = "Employee in the private sector",
      "Trabalhador doméstico"         = "Domestic worker",
      "Empregado no setor público"    = "Employee in the public sector",
      "Trabalhador familiar auxiliar" = "Auxiliary family worker",
      "Empregador"                    = "Employer",
      "Conta própria"                 = "Own account",
      ),
    value    = value * 1e3
    )
#num_txt_ocupacao <- c("Mil", "Milhão", "Milhões", "Bilhão", "Bilhões")


# Unemployment rate
unemployment <- raw_unemployment %>%
  select(
    date     = "Trimestre (Código)",
    variable = "Variável",
    value    = "Valor"
    ) %>%
  mutate(
    date     = stringr::str_replace(date, "(\\d{4})0(\\d{1}$)", "\\1 Q\\2"),
    region   = "Brazil",
    variable = recode(
      variable,
      "Pessoas de 14 anos ou mais de idade, ocupadas na semana de referência" = "employed",
      "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência" = "unemployed",
      "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade" = "unemployment_rate"
      )
    ) %>%
  pivot_wider(
    id_cols     = c(date, region),
    names_from  = variable,
    values_from = value
    ) %>%
  mutate(across(employed:unemployed, ~ . * 1e3))
  

# Unemployment rate by gender and color
unemployment_gender_color <- raw_pnadct %>%
  group_by(
    "gender" = V2007,
    "color" = V2010,
    VD4002
    ) %>%
  summarise(n = sum(V1028)) %>%
  drop_na() %>% 
  ungroup() %>% 
  pivot_wider(
    names_from  = VD4002, 
    values_from = n
    ) %>% 
  rename_with(~c("employed", "unemployed"), 3:4) %>% 
  replace_na(list(unemployed = 0)) %>%
  group_by(gender, color) %>%
  mutate(
    unemployment_rate = sum(unemployed) / sum(unemployed + employed) * 100,
    date              = paste0(
      api_ibge$api_pnadc_dates$year, " Q", api_ibge$api_pnadc_dates$quarter
      )
    ) %>% 
  filter(color != "Ignorado")


# Create attributes for the plot of the last object
gender_attr <- data.frame(
  gender = c("Man", "Woman"),
  color  = c("#2980b9", "#9b59b6"),
  icon   = c(
    here::here("inst/imgs/man.png"),
    here::here("inst/imgs/woman.png")
    )
  )


# Create list of Brazilian states
states <- tibble(
  state = c(
    "Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", 
    "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", 
    "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia",
    "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo",
    "Paraná", "Santa Catarina", "Rio Grande do Sul",
    "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"
    ),
  code = c(
    "RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI",
    "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES",
    "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF"
    )
  )


dicionario_desemprego_map <- list(
  tx_desemprego = "Taxa de Desemprego",
  pessoas_ocupadas = "Pessoas Ocupadas",
  pessoas_desocupadas = "Pessoas Desocupadas",
  trimestre = "Trimestre",
  isolar = "Isolar",
  ocultar = "Ocultar")


num_txt_desemprego_map <- c("Mil", "Milhão", "Milhões", "Bilhão", "Bilhões")

# Mapa do Desemprego
desemprego_map <- raw_pnadct %>% 
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
  mutate(trimestre = paste(tail(raw_pnadct$Ano, length(uf)), "-T",
                               tail(raw_pnadct$Trimestre, length(uf)), sep = ""))


# Gráfico Nível da Ocupação
nivel_ocupacao <- raw_labor_force %>%
  select(trimestre = "Trimestre (Código)",
         variavel = "Variável",
         sexo = "Sexo",
         valor = "Valor") %>%
  mutate(trimestre = gsub("(\\d{4})(\\d{2})$","\\1-\\2", trimestre),
         trimestre = gsub("-0", "-T", trimestre)) %>%
  filter(variavel == "Nível de ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade") %>%
  select(-variavel)


# Box Taxa de Participação
taxa_participacao <- raw_labor_force %>%
  select(trimestre = "Trimestre (Código)",
         variavel = "Variável",
         sexo = "Sexo",
         valor = "Valor") %>%
  mutate(trimestre = gsub("(\\d{4})(\\d{2})$","\\1-\\2", trimestre),
         trimestre = gsub("-0", "-T", trimestre)) %>%
  filter(variavel == "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade") %>%
  select(-variavel)


# Box Rendimento
rendimento <- raw_income %>%
  select(trimestre = "Trimestre (Código)",
         idade = "Grupo de idade",
         valor = "Valor") %>%
  mutate(trimestre = gsub("(\\d{4})(\\d{2})$","\\1-\\2", trimestre),
         trimestre = gsub("-0", "-T", trimestre),
         idade = gsub(" anos", "", idade),
         id = rep(c("1","2","3","4","5","6"), length(trimestre)/6)) %>%
  drop_na()


# Box Índice de Gini Estadual
raw_pnadct_gini <- raw_pnadct %>%
  pnadc_design() %>%
  convey_prep()

gini <- svyby(~VD4020, by = ~UF, raw_pnadct_gini, svygini, na.rm = TRUE) %>%
  select(uf = UF, VD4020) %>%
  mutate(id = "gini") %>%
  left_join(uf_sigla, by = "uf") %>%
  as_tibble() %>%
  mutate(trimestre = paste0(raw_pnadct$Ano[1], "-T", raw_pnadct$Trimestre[1]))



# Exportar dados ----------------------------------------------------------

diretorio <- file.path("../data")
save.image(file = file.path(diretorio, "trabalho.Rdata"))
