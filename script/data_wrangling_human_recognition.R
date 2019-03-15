# setup ------------------------------------------------------------------------
library(tidyverse)
library(splitstackshape)
# upload data ------------------------------------------------------------------
metadata_video <- readr::read_rds(here::here("data/human_recognition_data/metadata_video.rds"))

human_recognition_data <- here::here("./data/human_recognition_data") %>% 
  fs::dir_ls(regexp = "\\.csv$") %>% 
  purrr::map_dfr(readr::read_csv2, .id = "source", col_types = cols(
    C_INDUC = col_character(),
    SEX_Sujet = col_character(),
    SEXE_Juge = col_character(),
    C_Juge = col_double(),
    C_Video = col_character(),
    E_Detectee = col_character(),
    TC_Debut = col_double(),
    TC_Fin = col_double()
  )) %>% 
  dplyr::mutate(source = base::basename(source)) %>% 
  dplyr::mutate_at(c("TC_Debut", "TC_Fin"), function(x){round(x/1000,0)}) %>% # time form millisec to sec
  dplyr::mutate(C_Video = stringr::str_replace(C_Video,".mpg","")) %>% 
  dplyr::mutate(SEXE_Juge = ifelse(is.na(SEXE_Juge),"N",SEXE_Juge))

# data wrangling ---------------------------------------------------------------
unique_ppt_count <- human_recognition_data %>% 
  dplyr::select(source,SEXE_Juge, C_Juge) %>% 
  unique()

unique_ppt <- human_recognition_data %>% 
  dplyr::select(-E_Detectee, -TC_Debut, -TC_Fin) %>% 
  unique() %>% 
  dplyr::left_join(metadata_video, by = "C_Video")

ppt_per_video <- unique_ppt %>% 
  dplyr::group_by(C_Video) %>% 
  dplyr::summarise(n = n())

full_timeline <- unique_ppt %>% 
  splitstackshape::expandRows("ffprobe_duration", drop = FALSE) %>% 
  dplyr::group_by(source, C_INDUC, SEX_Sujet, SEXE_Juge, C_Juge, C_Video) %>% 
  dplyr::mutate(timeline = 1:n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_if(is.factor,as.character)

data_timeline <- human_recognition_data %>% 
  mutate(timeline = map2(TC_Debut, TC_Fin, `:`)) %>% 
  select(-TC_Debut, -TC_Fin) %>% 
  unnest() %>% 
  dplyr::mutate_if(is.factor,as.character)

final_timeline <- dplyr::left_join(full_timeline,data_timeline, by = c("source", "C_INDUC", "SEX_Sujet", "SEXE_Juge", "C_Juge", "C_Video", "timeline")) %>% 
  replace(is.na(.),"no_rec") %>% 
  dplyr::mutate(Curiosite = ifelse(E_Detectee == "Curiosite",1,0)) %>% 
  dplyr::mutate(Deception = ifelse(E_Detectee == "Deception",1,0)) %>% 
  dplyr::mutate(Degout = ifelse(E_Detectee == "Degout",1,0)) %>% 
  dplyr::mutate(Emu = ifelse(E_Detectee == "Emu",1,0)) %>% 
  dplyr::mutate(Enervement = ifelse(E_Detectee == "Enervement",1,0)) %>% 
  dplyr::mutate(Ennui = ifelse(E_Detectee == "Ennui",1,0)) %>% 
  dplyr::mutate(Fierte = ifelse(E_Detectee == "Fierte",1,0)) %>% 
  dplyr::mutate(Frayeur = ifelse(E_Detectee == "Frayeur",1,0)) %>% 
  dplyr::mutate(Gaiete  = ifelse(E_Detectee == "Gaiete",1,0)) %>% 
  dplyr::mutate(Honte = ifelse(E_Detectee == "Honte",1,0)) %>% 
  dplyr::mutate(RAS = ifelse(E_Detectee == "RAS",1,0)) %>% 
  dplyr::mutate(Stupefaction = ifelse(E_Detectee == "Stupefaction",1,0)) %>% 
  dplyr::mutate(Vexation = ifelse(E_Detectee == "Vexation",1,0))

average_timeline <- final_timeline %>% 
  dplyr::group_by(C_Video,timeline) %>% 
  dplyr::summarise(
    Curiosite = sum(Curiosite),
    Deception = sum(Deception),
    Degout  = sum(Degout),
    Emu = sum(Emu),
    Enervement = sum(Enervement),
    Ennui = sum(Ennui),
    Fierte = sum(Fierte),
    Frayeur = sum(Frayeur),
    Gaiete = sum(Gaiete),
    Honte = sum(Honte),
    RAS = sum(RAS),
    Stupefaction = sum(Stupefaction),
    Vexation = sum(Vexation)) %>% 
  dplyr::inner_join(ppt_per_video, by = "C_Video") %>% 
  dplyr::select(
    C_Video,
    n,
    timeline,
    disgust = Degout,
    sadness = Emu,
    anger = Enervement,
    fear = Frayeur,
    happiness = Gaiete,
    surprise = Stupefaction) %>% 
  dplyr::mutate(
    disgust = disgust/n,
    sadness = sadness/n,
    anger = anger/n,
    fear = fear/n,
    happiness = happiness/n,
    surprise = surprise/n)

human_recognition_score <- average_timeline %>%
  tidyr::gather(emotion,value, c("disgust","sadness","anger", "fear", "happiness", "surprise")) %>% 
  dplyr::group_by(C_Video, emotion) %>%
  dplyr::summarise(sum_emotion = sum(value)) %>% 
  dplyr::group_by(C_Video) %>% 
  dplyr::mutate(sum_video = sum(sum_emotion)) %>% 
  dplyr::mutate(confidence_score = sum_emotion/sum_video) %>% 
  dplyr::filter(confidence_score == max(confidence_score))

list_ties <- human_recognition_score %>%
  dplyr::group_by(C_Video) %>%
  dplyr::summarise(n = n()) %>% 
  dplyr::filter(n > 1) %>% 
  dplyr::mutate(ties = "yes") %>% 
  dplyr::select(-n)

human_recognition_score <- dplyr::left_join(human_recognition_score, list_ties, by = c("C_Video")) %>% 
  dplyr::mutate(emotion = case_when(
    ties == "yes" ~ "undetermined",
    TRUE ~ emotion
  )) %>% 
  dplyr::select(-ties) %>% 
  dplyr::distinct(C_Video,.keep_all = TRUE)
