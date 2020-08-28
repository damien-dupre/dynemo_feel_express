################################################################################
#                  Data Wrangling: Automatic Recognition                       #
################################################################################

# setup ------------------------------------------------------------------------
library(here)
library(tidyverse)
library(janitor)

# data path --------------------------------------------------------------------
data_path <- here::here() %>% 
  dirname() %>% 
  dirname() %>% 
  file.path("data/dynemo_feel_data/human_self_report_data")

# upload data ------------------------------------------------------------------
self_report_data <- file.path(data_path,  "Base_Finale_20090114.rds") %>% 
  readr::read_rds() %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(C_Video = stringr::str_replace(fichier_video_v,".mpg","")) %>% 
  dplyr::select(
    n_sujet_c,
    age,
    induction_c,
    genre_c,
    duree_v,
    C_Video,          
    curiosite_interet,
    ennui,
    repulsion_degout,
    frayeur,
    gaiete_amusement,
    honte,
    fierte_satisfaction,
    stupefaction_surprise,
    emu,
    enervee_irritation,   
    vexe_vexation,
    decu
  ) %>%
  dplyr::mutate(induction_c = case_when(
    induction_c == "AM" ~ "PT_01",
    induction_c == "DE" ~ "NT_05",
    induction_c == "EM" ~ "NT_06",
    induction_c == "EN" ~ "NT_07",
    induction_c == "FR" ~ "NT_08",
    induction_c == "HO" ~ "NT_10",
    induction_c == "IN" ~ "PT_02",
    induction_c == "IR" ~ "NT_09",
    induction_c == "NE" ~ "Neutral",
    induction_c == "SA" ~ "PT_03",
    induction_c == "SU" ~ "PT_04"
    )
  )

# data wrangling ---------------------------------------------------------------

self_report_tidy <- self_report_data %>%
  dplyr::select(
    n_sujet_c,
    C_Video,
    induction_c,
    repulsion_degout,
    frayeur,
    gaiete_amusement,
    stupefaction_surprise,
    emu,
    enervee_irritation
  ) %>% 
  tidyr::gather(
    emotion,
    value,
    c(
      "repulsion_degout",
      "frayeur",
      "gaiete_amusement",
      "stupefaction_surprise",
      "emu",
      "enervee_irritation"  
    )
  ) %>%
  dplyr::mutate(emotion = case_when(
    emotion == "repulsion_degout" ~ "disgust",
    emotion == "frayeur" ~ "fear",
    emotion == "gaiete_amusement" ~ "happiness",
    emotion == "stupefaction_surprise" ~ "surprise",
    emotion == "emu" ~ "sadness",
    emotion == "enervee_irritation" ~ "anger",
    )
  )

# emotion score ----------------------------------------------------------------
self_report_score <- self_report_tidy %>%
  dplyr::group_by(C_Video) %>%
  dplyr::filter(value == max(value)) %>% 
  dplyr::distinct(n_sujet_c, C_Video,induction_c,.keep_all = TRUE) %>% # remove videos with more than one label recognized
  dplyr::mutate(n = 1)  #reduce count to 1 in case of multiple similare max value per video

list_automatic_ties <- self_report_tidy %>%
  dplyr::group_by(C_Video) %>%
  dplyr::filter(value == max(value)) %>% 
  dplyr::group_by(n_sujet_c, C_Video,induction_c) %>%
  dplyr::summarise(n = n()) %>% 
  dplyr::filter(n > 1) %>% 
  dplyr::mutate(ties = "yes") %>% 
  dplyr::select(-n)

self_report_score <- dplyr::left_join(self_report_score, list_automatic_ties, by = c("n_sujet_c", "C_Video","induction_c")) %>% 
  dplyr::mutate(emotion = case_when(
    ties == "yes" ~ "undetermined",
    TRUE ~ emotion
  )) %>% 
  dplyr::select(-ties)