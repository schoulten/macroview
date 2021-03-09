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
rbcb::get_monthly_market_expectations()
rbcb::get_quarterly_market_expectations()

library(microbenchmark)
library(magrittr)

# get_annual testing ------------------------------------------------------
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
  df = get_annual(indicator      = c("PIB Total", "Fiscal"),
           first_date     = "2018-01-01",
           use_memoise    = FALSE,
           do_parallel    = FALSE),
  df_rbcb = rbcb::get_annual_market_expectations(
    indic = c("PIB Total", "Fiscal"),
    start_date     = "2018-01-01"),
  times = 1
) %>% print()


get_annual(detail = "teste")
get_annual(indicator = "Fiscal")
get_annual(indicator = "Fiscalasas")
get_annual(indicator = "Fiscal", detail = NULL)
get_annual(indicator = "Fiscal", detail = "Resultado Nominal")
get_annual(indicator = "Fiscal", detail = "DFSFSDFS")
get_annual(indicator = NULL, detail = "DFSFSDFS")
get_annual(indicator = NA, detail = "DFSFSDFS")
get_annual(indicator = "Fiscal", detail = "Resultado Nominal", first_date = "2021-01-30")
get_annual(detail = "DFSFSDFS")
get_annual(indicator = "Fiscal", detail = NA)

get_annual(indicator = "Fiscal", first_date = "20210302")
get_annual(indicator = "Fiscal", first_date = "54564")
get_annual(indicator = c("Fiscal", "IPCA"), first_date = "2019-03-08", do_parallel = TRUE, use_memoise = FALSE)
get_annual(indicator = c("Fiscal", "as"), first_date = "2021-03-02", do_parallel = TRUE, use_memoise = FALSE)
get_annual(indicator = "Fiscal", first_date = "2021-03-33")
get_annual(indicator = "Fiscal", first_date = "2021-33-02")
get_annual(indicator = "Fiscal", first_date = "2021/03/02")
get_annual(indicator = "Fiscal", first_date = "teste")

get_annual(indicator = "Fiscal", last_date = "20210302", first_date = "20210302")
get_annual(indicator = "Fiscal", last_date = "20210302")
get_annual(indicator = "Fiscal", last_date = "2021-03-02")
get_annual(indicator = "Fiscal", last_date = "2021-33-02")
get_annual(indicator = "Fiscal", last_date = "2021/03/02")
get_annual(indicator = "Fiscal", last_date = "2019/03/02")
get_annual(indicator = "Fiscal", last_date = "2019/03/33")
get_annual(indicator = "Fiscal", last_date = "2019/03/0a")
get_annual(indicator = "Fiscal", last_date = "2tes")

get_annual(indicator = "Fiscal", first_date = "2021-03-02", last_date = "2021-03-02")
get_annual(indicator = c("Fiscal", "IPC-FIPE", "IPCA"), first_date = "2021-01-02", last_date = "2021-02-02")
get_annual(indicator = "Fiscal", first_date = "2021-05-02", last_date = "2021-03-02")
get_annual(indicator = "IPC-FIPE", first_date = "2021-05-02", last_date = "2021-06-02")
get_annual(indicator = "Fiscal", first_date = "2021-05-02", last_date = "20210602")
get_annual(indicator = "Fiscal", first_date = "2021-05-02")
get_annual(indicator = "IPC-FIPE", first_date = NULL, last_date = "2021-06-02")
get_annual(indicator = "IPC-FIPE", first_date = NA, last_date = "2021-06-02") %>% dplyr::arrange(date)


get_annual(indicator = "Fiscal", reference_date = 2021)
get_annual(indicator = "Fiscal", reference_date = "2050", do_parallel = TRUE)
get_annual(indicator = "Fiscal", reference_date = "ssddSDS")
get_annual(indicator = "Fiscal", reference_date = "20255")

get_annual(indicator = "Fiscal", reference_date = 2021:2025)
get_annual(indicator = "Fiscal", reference_date = "2021:20255")
get_annual(indicator = "Fiscal", reference_date = "2021:20d5")
get_annual(indicator = "Fiscal", reference_date = "2021:2025")
get_annual(indicator = "IPC-FIPE", reference_date = NULL)
get_annual(indicator = "IPC-FIPE", reference_date = NA, do_parallel = TRUE, use_memoise = FALSE, first_date = "2021-02-01")




# get_monthly( ) testing --------------------------------------------------

### evaluate
indicator <- c(
  "IGP-DI", "IGP-M", "INPC", "IPA-DI", "IPA-M", "IPCA", "IPCA-15", "IPC-FIPE",
  "Produção industrial", "Meta para taxa over-selic", "Taxa de câmbio")
indicator_rbcb <- c("IGP-DI", "IGP-M", "INPC", "IPA-DI", "IPA-M", 
           "IPCA", "IPCA-15", "IPC-Fipe", "Produção industrial", 
           "Meta para taxa over-selic", "Taxa de câmbio")

performance=microbenchmark(
  df = get_monthly(indicator = indicator,
           first_date     = "2020-01-01",
           use_memoise    = FALSE,
           do_parallel    = TRUE),
  df_rbcb = rbcb::get_monthly_market_expectations(
    indic = indicator_rbcb,
    start_date     = "2020-01-01"),
  times = 5
) %>% print()



get_monthly()
get_monthly(indicator = "INPC")
get_monthly(indicator = "INPCsds")
get_monthly(indicator = "INPC")
get_monthly(indicator = NULL)
get_monthly(indicator = NA)

get_monthly(indicator = "INPC", first_date = "20210302")
get_monthly(indicator = "INPC", first_date = "54564")
get_monthly(indicator = c("INPC", "IPCA"), first_date = "2019-03-08", do_parallel = TRUE, use_memoise = FALSE)
get_monthly(indicator = c("INPC", "as"), first_date = "2021-03-02", do_parallel = TRUE, use_memoise = FALSE)
get_monthly(indicator = "INPC", first_date = "2021-03-33")
get_monthly(indicator = "INPC", first_date = "2021-33-02")
get_monthly(indicator = "INPC", first_date = "2021/02/01")
get_monthly(indicator = "INPC", first_date = NULL)
get_monthly(indicator = "INPC", first_date = NA)
get_monthly(indicator = "INPC", first_date = "teste")

get_monthly(indicator = "INPC", last_date = "20210302", first_date = "20210302")
get_monthly(indicator = "INPC", last_date = "20210302")
get_monthly(indicator = "INPC", last_date = "2021-03-02")
get_monthly(indicator = "INPC", last_date = "2021-33-02")
get_monthly(indicator = "INPC", last_date = "2021/03/02")
get_monthly(indicator = "INPC", last_date = "2019/03/02")
get_monthly(indicator = "INPC", last_date = "2019/03/33")
get_monthly(indicator = "INPC", last_date = "2019/03/0a")
get_monthly(indicator = "INPC", last_date = "2tes")

get_monthly(indicator = "INPC", first_date = "2021-03-02", last_date = "2021-03-02")
get_monthly(indicator = c("INPC", "IPC-FIPE", "IPCA"), first_date = "2021-01-02", last_date = "2021-02-02")
get_monthly(indicator = "INPC", first_date = "2021-05-02", last_date = "2021-03-02")
get_monthly(indicator = "IPC-FIPE", first_date = "2021-05-02", last_date = "2021-06-02")
get_monthly(indicator = "INPC", first_date = "2021-05-02", last_date = "20210602")
get_monthly(indicator = "INPC", first_date = "2021-05-02")
get_monthly(indicator = "IPC-FIPE", first_date = NULL, last_date = "2021-06-02")
get_monthly(indicator = "IPC-FIPE", first_date = NA, last_date = "2021-06-02") %>% dplyr::arrange(date)


get_monthly(indicator = "INPC", reference_date = 2021)
get_monthly(indicator = "INPC", reference_date = "2050", do_parallel = TRUE)
get_monthly(indicator = "INPC", reference_date = "ssddSDS")
get_monthly(indicator = "INPC", reference_date = "20255")

get_monthly(indicator = "INPC", reference_date = 2021:2025)
get_monthly(indicator = "INPC", reference_date = "2021:20255")
get_monthly(indicator = "INPC", reference_date = "2021:20d5")
get_monthly(indicator = "INPC", reference_date = "2021:2025")
get_monthly(indicator = "IPC-FIPE", reference_date = NULL)
get_monthly(indicator = "IPC-FIPE", reference_date = NA, do_parallel = TRUE, use_memoise = FALSE, first_date = "2021-02-01")

get_monthly(indicator = "INPC", reference_date = "01/2021")
get_monthly(indicator = "INPC", reference_date = "01/2028")
get_monthly(indicator = "INPC", reference_date = " 01/2021")
get_monthly(indicator = "INPC", reference_date = "0A/2021")
get_monthly(indicator = "INPC", reference_date = "01:2021")
get_monthly(indicator = "INPC", reference_date = "01-2021")
get_monthly(indicator = "INPC", reference_date = "001/2021")
get_monthly(indicator = "INPC", reference_date = "01/20201")
get_monthly(indicator = "INPC", reference_date = "13/2021")
get_monthly(indicator = "INPC", reference_date = "01/202A")
get_monthly(indicator = "INPC", reference_date = "01/20 21")
get_monthly(indicator = "INPC", reference_date = NULL)
get_monthly(indicator = "INPC", reference_date = NA)

get_monthly(indicator = "INPC", reference_date = NA, be_quiet = FALSE, use_memoise = TRUE, do_parallel = FALSE)
get_monthly(indicator = "INPC", reference_date = NULL, be_quiet = FALSE, use_memoise = TRUE, do_parallel = TRUE)




# get_quarterly testing ---------------------------------------------------

### evaluate
indicator <- c("PIB Agropecuária", "PIB Industrial", "PIB Serviços", "PIB Total")
indicator_rbcb <- c("PIB Agropecuária", "PIB Industrial", "PIB Serviços", "PIB Total")

performance=microbenchmark(
  df = get_quarterly(indicator = indicator,
                   first_date     = "2020-01-01",
                   use_memoise    = FALSE,
                   do_parallel    = TRUE),
  df_rbcb = rbcb::get_quarterly_market_expectations(
    indic = indicator_rbcb,
    start_date     = "2020-01-01"),
  times = 10
) %>% print()



get_quarterly()
get_quarterly(indicator = "PIB Total")
get_quarterly(indicator = "PIB Totalsds")
get_quarterly(indicator = "PIB Total")
get_quarterly(indicator = NULL)
get_quarterly(indicator = NA)

get_quarterly(indicator = "PIB Total", first_date = "20210302")
get_quarterly(indicator = "PIB Total", first_date = "54564")
get_quarterly(indicator = c("PIB Total", "PIB Serviços"), first_date = "2019-03-08", do_parallel = FALSE, use_memoise = FALSE)
get_quarterly(indicator = c("PIB Total", "as"), first_date = "2021-03-02", do_parallel = TRUE, use_memoise = FALSE)
get_quarterly(indicator = "PIB Total", first_date = "2021-03-33")
get_quarterly(indicator = "PIB Total", first_date = "2021-33-02")
get_quarterly(indicator = "PIB Total", first_date = "2021/02/01")
get_quarterly(indicator = "PIB Total", first_date = NULL)
get_quarterly(indicator = "PIB Total", first_date = NA)
get_quarterly(indicator = "PIB Total", first_date = "teste")

get_quarterly(indicator = "PIB Total", last_date = "20210302", first_date = "20210302")
get_quarterly(indicator = "PIB Total", last_date = "20210302")
get_quarterly(indicator = "PIB Total", last_date = "2021-03-02")
get_quarterly(indicator = "PIB Total", last_date = "2021-33-02")
get_quarterly(indicator = "PIB Total", last_date = "2021/03/02")
get_quarterly(indicator = "PIB Total", last_date = "2019/03/02")
get_quarterly(indicator = "PIB Total", last_date = "2019/03/33")
get_quarterly(indicator = "PIB Total", last_date = "2019/03/0a")
get_quarterly(indicator = "PIB Total", last_date = "2tes")

get_quarterly(indicator = "PIB Total", first_date = "2021-03-02", last_date = "2021-03-02")
get_quarterly(indicator = c("PIB Total", "IPC-FIPE", "IPCA"), first_date = "2021-01-02", last_date = "2021-02-02")
get_quarterly(indicator = "PIB Total", first_date = "2021-05-02", last_date = "2021-03-02")
get_quarterly(indicator = "PIB Serviços", first_date = "2021-05-02", last_date = "2021-06-02")
get_quarterly(indicator = "PIB Total", first_date = "2021-05-02", last_date = "20210602")
get_quarterly(indicator = "PIB Total", first_date = "2021-05-02")
get_quarterly(indicator = "PIB Serviços", first_date = NULL, last_date = "2021-06-02")
get_quarterly(indicator = "PIB Serviços", first_date = NA, last_date = "2021-06-02") %>% dplyr::arrange(date)


get_quarterly(indicator = "PIB Total", reference_date = 2021)
get_quarterly(indicator = "PIB Total", reference_date = "2050", do_parallel = TRUE)
get_quarterly(indicator = "PIB Total", reference_date = "ssddSDS")
get_quarterly(indicator = "PIB Total", reference_date = "20255")

get_quarterly(indicator = "PIB Total", reference_date = 2021:2025)
get_quarterly(indicator = "PIB Total", reference_date = "2021:20255")
get_quarterly(indicator = "PIB Total", reference_date = "2021:20d5")
get_quarterly(indicator = "PIB Total", reference_date = "2021:2025")
get_quarterly(indicator = "PIB Serviços", reference_date = NULL)
get_quarterly(indicator = "PIB Serviços", reference_date = NA, do_parallel = TRUE, use_memoise = FALSE, first_date = "2021-02-01")

get_quarterly(indicator = "PIB Total", reference_date = "1/2021")
get_quarterly(indicator = "PIB Total", reference_date = "1/2028")
get_quarterly(indicator = "PIB Total", reference_date = " 1/2021")
get_quarterly(indicator = "PIB Total", reference_date = "A/2021")
get_quarterly(indicator = "PIB Total", reference_date = "1:2021")
get_quarterly(indicator = "PIB Total", reference_date = "1-2021")
get_quarterly(indicator = "PIB Total", reference_date = "01/2021")
get_quarterly(indicator = "PIB Total", reference_date = "1/20201")
get_quarterly(indicator = "PIB Total", reference_date = "6/2021")
get_quarterly(indicator = "PIB Total", reference_date = "1/202A")
get_quarterly(indicator = "PIB Total", reference_date = "2/20 21")
get_quarterly(indicator = "PIB Total", reference_date = NULL)
get_quarterly(indicator = "PIB Total", reference_date = NA)

get_quarterly(indicator = "PIB Total", reference_date = NA, be_quiet = FALSE, use_memoise = TRUE, do_parallel = FALSE)
get_quarterly(indicator = "PIB Total", reference_date = NULL, be_quiet = FALSE, use_memoise = F, do_parallel = TRUE)


