library(tidyverse)

data <- here::here("./data/DynEmo_affdex") %>% 
  fs::dir_ls(regexp = "\\.csv$") %>% 
  purrr::map_dfr(readr::read_csv, .id = "source") %>% 
  dplyr::mutate(filename = base::basename(source))

readr::write_rds(data, here::here("./data/DynEmo_affdex.rds"))

data <- readr::read_rds(here::here("./data/DynEmo_affdex.rds"))