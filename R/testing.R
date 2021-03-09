library(GetBCBData)
GetBCBData::gbcbd_get_default_cache_folder()
GetBCBData::gbcbd_get_JSON_fct()
GetBCBData::gbcbd_get_series("189", first.date = "2018-01-01", do.parallel = TRUE, use.memoise = FALSE)
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
library(magrittr)
indicators <- c("Balança Comercial", 
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
                   "Taxa de câmbio")
indicators_rbcb <- c("Balança Comercial", 
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
                        "Taxa de câmbio")


library(microbenchmark)
library(purrr)

future::plan(future::multisession, workers = floor(future::availableCores())/2)

performance=microbenchmark(
  df = bcb(indicator      = c("PIB Total", "Fiscal"),
           first_date     = "2018-01-01",
           use_memoise    = FALSE,
           do_parallel    = FALSE),
  df_rbcb = rbcb::get_annual_market_expectations(
    indic = c("PIB Total", "Fiscal"),
    start_date     = "2018-01-01"),
  times = 1
) %>% print()


bcb(
  indicator      = indicators,
  detail         = NULL,
  first_date     = "2020-01-01",
  last_date      = "2021-03-03",
  be_quiet       = FALSE,
  reference_date = NA,
  use_memoise    = F,
  do_parallel    = TRUE
)
