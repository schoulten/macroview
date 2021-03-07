library(GetBCBData)
GetBCBData::gbcbd_get_default_cache_folder()
GetBCBData::gbcbd_get_JSON_fct()
GetBCBData::gbcbd_get_series()
GetBCBData::gbcbd_get_single_series()
GetBCBData::gbcbd_message()
GetBCBData::gbcbd_test_internet()

library(BETS)
BETS::bcbExpectA(indicator = "IPCA",  start = "2021-01-01", end = "2021-03-03")

library(rbcb)
rbcb::get_annual_market_expectations()
rbcb::get_monthly_market_expectations()
rbcb::get_top5s_annual_market_expectations()

# testing purrrring wrapper -----------------------------------------------

indicators <- list("Balança Comercial", 
                   "Balanço de Pagamentos",
                   "Fiscal", 
                   "IGP-DI", 
                   "IGP-M",
                   "INPC", 
                   "IPA-DI", 
                   "IPA-M", 
                   "IPCA", 
                   "IPCA-15", 
                   "IPC-FIPE",
                   "Preços administrados por contrato e monitorados", 
                   "Produção industrial",
                   "PIB Agropecuária", 
                   "PIB Industrial", 
                   "PIB Serviços", 
                   "PIB Total",
                   "Meta para taxa over-selic",
                   "Taxa de câmbio") %>% t() %>% data.frame() 
indicators_rbcb <- list("Balança Comercial", 
                        "Balanço de Pagamentos",
                        "Fiscal", 
                        "IGP-DI", 
                        "IGP-M",
                        "INPC", 
                        "IPA-DI", 
                        "IPA-M", 
                        "IPCA", 
                        "IPCA-15", 
                        "IPC-Fipe",
                        "Preços administrados por contrato e monitorados", 
                        "Produção industrial",
                        "PIB Agropecuária", 
                        "PIB Industrial", 
                        "PIB Serviços", 
                        "PIB Total",
                        "Meta para taxa over-selic",
                        "Taxa de câmbio") %>% t() %>% data.frame() 
references <- list(2021,2022,2023) %>% map(as.character)

tictoc::tic()
test <- map(indicators, ~bcb(indicator = .x, first_date = NULL, last_date = NULL))
tictoc::toc()

tictoc::tic()
teste <- dplyr::bind_rows(test)
tictoc::toc()

library(microbenchmark)
performance=microbenchmark(
  {pmap(list(indicators), ~bcb(indicator = .x, use_memoise = TRUE))},
  {pmap(list(indicators_rbcb), ~rbcb::get_annual_market_expectations(indic = .x))},
  times = 1
) %>% print()




my_url=list("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?
            $filter=Indicador%20eq%20'IPCA'%20and%20Data%20ge%20'2015-01-01'%20and%20Data%20le%20'2021-02-01'&
            $orderby=Data%20desc&$format=json")


performance=microbenchmark(
  {pmap(my_url, ~try(suppressWarnings(jsonlite::fromJSON(readLines(.x))$value), silent = TRUE))},
  {pmap(my_url, ~try(suppressWarnings(jsonlite::fromJSON(my_url)$value), silent = TRUE))},
  times = 1
) %>% print()
