library(GetBCBData)
GetBCBData::gbcbd_get_default_cache_folder()
GetBCBData::gbcbd_get_JSON_fct()
GetBCBData::gbcbd_get_series()
GetBCBData::gbcbd_get_single_series()
GetBCBData::gbcbd_message()
GetBCBData::gbcbd_test_internet()


# testing purrrring wrapper -----------------------------------------------

indicators <- list("IGP-DI", "IGP-M", "INPC", "IPA-DI", "IPA-M","IPCA", "IPCA-15", "IPC-FIPE")


tictoc::tic()
test <- map(indicators, ~bcb(indicator = .x,
                             first_date = "2021-02-01",
                             reference_date = "2021"))
tictoc::toc()

tictoc::tic()
teste <- dplyr::bind_rows(test)
tictoc::toc()