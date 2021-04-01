

retry(etl_econ_activity())
retry(etl_fiscal())
retry(etl_inflation())
retry(etl_international())
retry(etl_labor())
retry(etl_monetary())


retry(rmarkdown::render("index.Rmd"))



devtools::document()
devtools::load_all()
devtools::check()



library(tidyverse)
mutate(mtcars, across(where(is.numeric), ~as.character))
