library(GetBCBData)
GetBCBData::gbcbd_get_default_cache_folder()
GetBCBData::gbcbd_get_JSON_fct()
GetBCBData::gbcbd_get_series()
GetBCBData::gbcbd_get_single_series()
GetBCBData::gbcbd_message()
GetBCBData::gbcbd_test_internet()

library(BETS)
BETS::bcbExpectA(indicator = "IPCA",  start = "2021-01-01", end = "2021-03-03")

# testing purrrring wrapper -----------------------------------------------

indicators <- list("Balança Comercial", 
                   "Balanço de Pagamentos",
                   "Fiscal")
references <- list(2021,2022,2023) %>% map(as.character)

tictoc::tic()
test <- map2(.x = indicators,
             .y = references,
             .f =  ~bcb(indicator = .x, first_date = "2021-02-01", reference_date = .y)
             )
tictoc::toc()

tictoc::tic()
teste <- dplyr::bind_rows(test)
tictoc::toc()