################################################################################
#                  Data Wrangling: Automatic Recognition                       #
################################################################################

# setup ------------------------------------------------------------------------
library(tidyverse)
library(here)

# data path --------------------------------------------------------------------
data_path <- here::here() %>% 
  dirname() %>% 
  dirname() %>% 
  file.path("data/dynemo_feel_data/automatic_recognition_data")

# upload data ------------------------------------------------------------------
automatic_recognition_data <- file.path(data_path,  "automatic_recognition_data.rds") %>% 
  readr::read_rds()

# data wrangling ---------------------------------------------------------------
automatic_recognition_score <- automatic_recognition_data %>% 
  dplyr::select(C_Video = filename, TimeStamp, happiness = joy, fear, disgust, sadness, anger, surprise) %>% 
  dplyr::mutate(C_Video = stringr::str_replace(C_Video, ".csv", "")) %>% 
  na.omit() %>% 
  tidyr::gather(
    emotion, 
    value, 
    c("happiness", "fear", "disgust", "sadness", "anger", "surprise")
  ) %>% 
  dplyr::mutate(probabilities = value/100) %>% 
  dplyr::mutate(odds = probabilities/(1-probabilities)) %>% 
  dplyr::select(C_Video, TimeStamp, emotion, odds) %>% 
  dplyr::group_by(C_Video, emotion) %>%
  dplyr::summarise(sum_emotion = sum(odds)) %>% 
  dplyr::group_by(C_Video) %>% 
  dplyr::mutate(sum_video = sum(sum_emotion)) %>% 
  dplyr::mutate(confidence_score = sum_emotion/sum_video) %>% 
  dplyr::filter(confidence_score == max(confidence_score))

list_ties <- automatic_recognition_score %>%
  dplyr::group_by(C_Video) %>%
  dplyr::summarise(n = n()) %>% 
  dplyr::filter(n > 1) %>% 
  dplyr::mutate(ties = "yes") %>% 
  dplyr::select(-n)

# emotion score ----------------------------------------------------------------
automatic_recognition_score <- automatic_recognition_score %>% 
  dplyr::left_join(list_ties, by = c("C_Video")) %>% 
  dplyr::mutate(emotion = case_when(
    ties == "yes" ~ "undetermined",
    TRUE ~ emotion
  )) %>% 
  dplyr::select(-ties) %>% 
  dplyr::distinct(C_Video, .keep_all = TRUE)
