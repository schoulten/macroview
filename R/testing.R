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

nomes <- c("2020","2021","2022")


teste = map2_dfr(.x = list("Fiscal"),
                 .y = list("2020","2021","2022","2023"),
                 .f = ~bcb(indicator      = .x,
                           first_date     = "2021-01-01",
                           reference_date = .y)
                 )



library(microbenchmark)
library(purrr)
performance=microbenchmark(
  {pmap(list(indicators), ~bcb(indicator = .x, use_memoise = FALSE, first_date = "2021-01-01", last_date = "2021-02-01"))},
  {pmap(list(indicators_rbcb), ~rbcb::get_annual_market_expectations(indic = .x, start_date = "2021-01-01", end_date = "2021-02-01"))},
  times = 5
) %>% print()





library(microbenchmark)
library(purrr)
performance=microbenchmark(
  {df = bcb(indicator      = "Fiscal",
            detail         = NULL,
            first_date     = "2021-01-01",
            last_date      = "2021-03-03",
            be_quiet       = FALSE,
            reference_date = NA,
            use_memoise    = FALSE)},
  {df_rbcb = rbcb::get_annual_market_expectations(
    indic      = "Fiscal",
    start_date = "2021-01-01",
    end_date   = "2021-03-03"
  )},
  times = 20
) %>% print()

