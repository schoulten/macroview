#' ETL Labor market
#'
#' @encoding UTF-8
#' @import dplyr PNADcIBGE
#' @importFrom PNADcIBGE pnadc_labeller
#' @importFrom utils lsf.str
#' @return RDATA
#' @export
#'
etl_labor <- function(){

### Labor market ###


# This R code provides the extraction and data wrangling of macroeconomic
# variables referring to the Brazilian labour market


# Packages ----------------------------------------------------------------


# Install/load packages
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   "PNADcIBGE",
#   "tidyverse",
#   "lubridate",
#   "sidrar",
#   "janitor",
#   "survey",
#   "convey",
#   "rvest",
#   "here"
#   )

if (!"package:PNADcIBGE" %in% search()) { attachNamespace("PNADcIBGE") }


# Parameters --------------------------------------------------------------


# Parameters used in the code to import, download or cleaning data


# Load useful functions
# source("./R/utils.R")


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
      quarter = stringr::str_extract(., "\\d{1}") %>% as.numeric(),
      year = stringr::str_extract(., "\\d{4}") %>% as.numeric()
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
raw_unemployment <- sidrar::get_sidra(api = api_ibge$api_unemployment)

# Employment by occupation / category in the main job (PNADC-T)
raw_employment <- sidrar::get_sidra(api = api_ibge$api_employment)

# Labor force participation rate and occupation level (PNADC-T)
raw_labor_force <- sidrar::get_sidra(api = api_ibge$api_labour_force)

# Average real income from work, by age group (PNADC-T)
raw_income <- sidrar::get_sidra(api = api_ibge$api_income)




# Data wrangling ----------------------------------------------------------


# This section performs data wrangling


# Number of employed and unemployed people
big_numbers <- raw_pnadct %>%
  group_by(VD4002) %>%
  summarise(n = round(sum(V1028) / 1e6, 1)) %>%
  tidyr::drop_na() %>%
  pivot_wider(names_from = VD4002, values_from = n) %>%
  rename_with(~c("employed", "unemployed")) %>%
  mutate(
    across(everything(), ~paste0(., " million")),
    date = paste0(api_ibge[[2]][[3]], " Q", api_ibge[[2]][[2]])
    )


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
    value    = (value / 1e3) %>% round(2)
    ) %>%
  arrange(desc(value))
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
  slice_tail(n = 24) %>%
  mutate(across(employed:unemployed, ~ . * 1e3))


# Unemployment rate by gender and color
unemployment_gender_color <- raw_pnadct %>%
  group_by(
    "gender" = V2007,
    "color" = V2010,
    VD4002
    ) %>%
  summarise(n = sum(V1028)) %>%
  tidyr::drop_na() %>%
  ungroup() %>%
  pivot_wider(
    names_from  = VD4002,
    values_from = n
    ) %>%
  rename_with(~c("employed", "unemployed"), 3:4) %>%
  tidyr::replace_na(list(unemployed = 0)) %>%
  group_by(gender, color) %>%
  mutate(
    unemployment_rate = (unemployed / (unemployed + employed) * 100) %>% round(2),
    date              = paste0(api_ibge[[2]][[3]], " Q", api_ibge[[2]][[2]]),
    gender = recode(gender, "Homem" = "Man", "Mulher" = "Woman"),
    color = recode(
      color,
      "Branca" = "White",
      "Preta" = "Black",
      "Amarela" = "Yellow",
      "Parda" = "Brown",
      "Indígena" = "Indian"
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


# Create list of Brazilian states for the map plot
states <- tibble(
  states = c(
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


# Unemployment rate by states
unemployment_states <- raw_pnadct %>%
  group_by(
    "states" = UF,
    VD4002
    ) %>%
  summarise(n = sum(V1028)) %>%
  tidyr::drop_na() %>%
  ungroup() %>%
  pivot_wider(
    names_from  = VD4002,
    values_from = n
    ) %>%
  rename_with(~c("employed", "unemployed"), 2:3) %>%
  tidyr::replace_na(list(employed = 0, unemployed = 0)) %>%
  mutate(
    unemployment_rate = (unemployed / (unemployed + employed) * 100) %>% round(2),
    date              = paste0(api_ibge[[2]][[3]], " Q", api_ibge[[2]][[2]])
    ) %>%
  arrange(-unemployment_rate) %>%
  left_join(states, by = "states") %>%
  rename("woe-name" = states)


# Labor force occupation level
occupation_level <- raw_labor_force %>%
  select(
    date     = "Trimestre (Código)",
    variable = "Variável",
    gender   = "Sexo",
    value    = "Valor"
    ) %>%
  filter(
    variable == "Nível de ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade"
    ) %>%
  slice_tail(n = 48) %>%
  mutate(
    date = stringr::str_replace(date, "(\\d{4})0(\\d{1}$)", "\\1 Q\\2"),
    variable = "Labor force occupation level",
    gender = recode(
      gender,
      "Homens" = "Man",
      "Mulheres" = "Woman"
      )
    )


# Labor force participation rate
participation_rate <- raw_labor_force %>%
  select(
    date     = "Trimestre (Código)",
    variable = "Variável",
    gender   = "Sexo",
    value    = "Valor"
    ) %>%
  filter(
    variable == "Taxa de participação na força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade"
    ) %>%
  slice_tail(n = 48) %>%
  mutate(
    date = stringr::str_replace(date, "(\\d{4})0(\\d{1}$)", "\\1 Q\\2"),
    variable = "Labor force participation rate",
    gender = recode(
      gender,
      "Homens" = "Man",
      "Mulheres" = "Woman"
      )
    )


# Average real income from work, by age group
income <- raw_income %>%
  select(
    date     = "Trimestre (Código)",
    variable = "Variável",
    age      = "Grupo de idade",
    value    = "Valor"
    ) %>%
  mutate(
    date = stringr::str_replace(date, "(\\d{4})0(\\d{1}$)", "\\1 Q\\2"),
    variable = "Average real income from work",
    id = rep(c("1","2","3","4","5","6"), length(date)/6),
    age = purrr::reduce2(
      .x    = c(" anos", " ou mais", " a "),
      .y    = c("", " or more", " to "),
      .init = age,
      stringr::str_replace
      )
    ) %>%
  tidyr::drop_na() %>%
  slice_tail(n = 80)


# Gini index by state
raw_pnadct_svy <- raw_pnadct %>%
  PNADcIBGE::pnadc_design() %>%
  convey::convey_prep()

gini <- survey::svyby(~VD4020, by = ~UF, raw_pnadct_svy, convey::svygini, na.rm = TRUE) %>%
  select("states" = UF, VD4020) %>% # Effective monthly income from all jobs
  left_join(states, by = "states") %>%
  as_tibble() %>%
  mutate(
    quarter  = paste0(raw_pnadct$Ano[1], " Q", raw_pnadct$Trimestre[1]),
    variable = "Gini index",
    value = round(value, 2)
    ) %>%
  select(
    quarter,
    code,
    states,
    variable,
    "value" = VD4020
    ) %>%
  arrange(states)




# Save data ---------------------------------------------------------------


# Aggregate data
rm(raw_pnadct_svy, envir = environment())
imported_data_labor <- mget(ls(pattern = "raw_|api_"))


# Remove unnecessary objects
rm(
  list  = c(lsf.str(), ls(pattern = "raw_|api_")),
  envir = environment()
  )


# Save RDATA file
save(
  list  = ls(),
  file  = file.path(file.path("./inst/extdata"), "labor.Rdata"),
  envir = environment()
  )

}
