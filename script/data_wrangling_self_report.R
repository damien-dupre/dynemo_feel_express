# setup ------------------------------------------------------------------------
library(here)
library(tidyverse)
library(janitor)
# upload data ------------------------------------------------------------------
self_report_data <- readr::read_rds("C:/Users/dupred/OneDrive/data/dynemo_feel_data/human_self_report_data/Base_Finale_20090114.rds") %>% 
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


# self_report_tidy <- self_report_tidy %>%
#   dplyr::group_by(induction_c, emotion) %>%
#   dplyr::summarise(mean_value = mean(value))

# self_report_tidy %>% 
#   ggplot(mapping = aes(x =  induction_c, y = emotion)) +
#   geom_tile(aes(fill = mean_value), colour = "white") +
#   geom_text(aes(label = round(mean_value,1)),
#             color = "black",
#             size = 3,
#             #family="serif",
#             #fontface = "bold",
#             parse = FALSE,
#             lineheight = 0.7
#   ) +
#   scale_x_discrete(name = "Elicitation Task") +
#   scale_y_discrete(name = "Emotion Felt") +
#   scale_fill_gradient(name = "Intensity (avg.)",low = "white",high = "red") +
#   theme_minimal() +
#   theme(text = element_text(size=18,family="serif"),
#         axis.text.x = element_text(angle = 45, hjust = 0.75,vjust=0.9),
#         strip.text.x = element_text(face="bold",size=18),
#         axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
#         legend.text = element_text(size=8),
#         legend.position = "bottom")


# 
# test3 <- test2 %>%
#   dplyr::group_by(Induction_C) %>%
#   dplyr::count(emotion)
# 
# unique(test3$Induction_C)
# 
# list_emotion_sr <- c(
#   "CURIOSITE/INTERET",
#   "ENNUI",
#   "REPULSION/DEGOUT",
#   "FRAYEUR",
#   "GAIETE/AMUSEMENT",
#   "HONTE",
#   "FIERTE/SATISFACTION",
#   "STUPEFACTION/SURPRISE",
#   "EMU",
#   "ENERVEE/IRRITATION",
#   "VEXE/VEXATION",
#   "DECU",
#   "AUTRE"
# )
# 
# list_induction <- c("AM","DE","EM","EN","FR","HO","IN","IR","NE","SA","SU")
# 
# test4 <- expand.grid(list_induction,list_emotion_sr) %>%
#   dplyr::rename(Induction_C = Var1, emotion = Var2) %>%
#   dplyr::mutate(n = 0)
# 
# test5 <- dplyr::anti_join(test4,test3, by = c("Induction_C", "emotion")) %>%
#   bind_rows(test3)
# 
# test5 %>%
#   ggplot(aes(reorder(emotion, -n), n)) +
#   geom_col() +
#   coord_flip() +
#   facet_wrap(~Induction_C) +
#   theme_minimal() +
#   theme(text = element_text(family = "serif", size=8))
#   
# data_sr <- data_sr %>% 
#   dplyr::filter(!is.na(`N° sujet__C`))
# 
# write_rds(data_sr, here::here("data/human_self_report_data/Base_Finale_20090114.rds"))
