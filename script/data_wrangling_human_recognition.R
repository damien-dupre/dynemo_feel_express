# test <- data.frame(ID = c("a","b","c"),
#                    start_time = c(0,20,50),
#                    stop_time = c(10,35,60))
# timeline <- data.frame(timeline = 0:100)
# 
# z1 <- data.frame(
#   time = seq(from = test$start_time[1], to = test$stop_time[1], by = 1),
#   label = 1)
# 
# z2 <- seq(from = test$start_time[2], to = test$stop_time[2], by = 1)
# 
# ################################################################################
# test2 <- test %>% 
#   mutate(timeline = map2(start_time, stop_time, `:`)) %>% 
#   select(-start_time, -stop_time) %>% 
#   unnest
# 
# test3 <- dplyr::full_join(timeline,test2,by = "timeline")
# ################################################################################
# data <- read.csv2("C:/Users/dupred/Desktop/DynEmo_human_recognition/Data_Pretraitees_03_11/Data_Pretraitees_03_11/final_2.csv")
# 
# head_data <- head(data) %>% 
#   dplyr::mutate(C_Video = stringr::str_replace(C_Video,".mpg",""))
# 
# str(head_data)
# 
# head_data <- dplyr::left_join(head_data,metadata_video, by = "C_Video")
# 
# str(metadata_video)
# 
# 
# test4 <- head_data %>% 
#   mutate(timeline = map2(TC_Debut, TC_Fin, `:`)) %>% 
#   select(-TC_Debut, -TC_Fin) %>% 
#   unnest
# # DVD10_1 = 259.16032s -> 259160.32ms 
# 
# timeline <- data.frame(timeline = 0:259160)
# 
# test6 <- dplyr::full_join(timeline,test4,by = "timeline")
# 
# 
# ##########################
# 
# unique_ppt <- head_data %>% 
#   dplyr::select(-E_Detectee, -TC_Debut, -TC_Fin) %>% 
#   unique()
# 
# test <- unique_ppt[rep(1:nrow(unique_ppt), unique_ppt$ffprobe_duration),] %>% 
#   dplyr::mutate(timeline = 1:unique_ppt$ffprobe_duration)
# 
# test4 <- head_data %>% 
#   mutate(timeline = map2(TC_Debut, TC_Fin, `:`)) %>% 
#   select(-TC_Debut, -TC_Fin) %>% 
#   unnest
# 
# test6 <- dplyr::left_join(test,test4, by = c("C_INDUC", "SEX_Sujet", "SEXE_Juge", "C_Juge", "C_Video", "ffprobe_duration", "timeline"))
# colnames(test4)
# 
# ################################################################################
# test5 <- data %>% 
#   mutate(timeline = map2(TC_Debut, TC_Fin, `:`)) %>% 
#   select(-TC_Debut, -TC_Fin) %>% 
#   unnest
# ################################################################################
# unique_ppt <- data %>% 
#   dplyr::select(-E_Detectee, -TC_Debut, -TC_Fin) %>% 
#   unique() %>% 
#   dplyr::mutate(C_Video = stringr::str_replace(C_Video,".mpg",""))
# 
# test <- dplyr::left_join(unique_ppt,metadata_video, by = "C_Video")
# 
# head_test <- head(test)
# 
# test <- head_test[rep(1:nrow(head_test), head_test$ffprobe_duration),]
# 
# test2 <- test %>% 
#   dplyr::group_by(C_Video) %>% 
#   dplyr::mutate(timeline = 1:n())

################################################################################
data <- read.csv2("C:/Users/dupred/Desktop/DynEmo_human_recognition/Data_Pretraitees_03_11/Data_Pretraitees_03_11/final_2.csv") %>% 
  dplyr::mutate_at(c("TC_Debut", "TC_Fin"), function(x){round(x/1000,0)}) %>% 
  dplyr::mutate(C_Video = stringr::str_replace(C_Video,".mpg",""))

unique_ppt <- data %>% 
  dplyr::select(-E_Detectee, -TC_Debut, -TC_Fin) %>% 
  unique() %>% 
  dplyr::left_join(metadata_video, by = "C_Video")

full_timeline <- unique_ppt[rep(1:nrow(unique_ppt), unique_ppt$ffprobe_duration),] %>% # full timeline dataframe
  dplyr::group_by(C_Video) %>% 
  dplyr::mutate(timeline = 1:n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_if(is.factor,as.character)

data_timeline <- data %>% 
  mutate(timeline = map2(TC_Debut, TC_Fin, `:`)) %>% 
  select(-TC_Debut, -TC_Fin) %>% 
  unnest() %>% 
  dplyr::mutate_if(is.factor,as.character)

str(data_timeline)
str(full_timeline)

final_timeline <- dplyr::left_join(full_timeline,data_timeline, by = c("C_INDUC", "SEX_Sujet", "SEXE_Juge", "C_Juge", "C_Video", "timeline")) %>% 
  replace(is.na(.),"no_rec") %>% 
  dplyr::mutate(Curiosite = ifelse(E_Detectee == "Curiosite",1,0)) %>% 
  dplyr::mutate(Deception = ifelse(E_Detectee == "Deception",1,0)) %>% 
  dplyr::mutate(Degout = ifelse(E_Detectee == "Degout",1,0)) %>% 
  dplyr::mutate(Emu = ifelse(E_Detectee == "Emu",1,0)) %>% 
  dplyr::mutate(Enervement = ifelse(E_Detectee == "Enervement",1,0)) %>% 
  dplyr::mutate(Ennui = ifelse(E_Detectee == "Ennui",1,0)) %>% 
  dplyr::mutate(Fierte = ifelse(E_Detectee == "Fierte",1,0)) %>% 
  dplyr::mutate(Frayeur = ifelse(E_Detectee == "Frayeur",1,0)) %>% 
  dplyr::mutate(Gaiete  = ifelse(E_Detectee == "Gaiete ",1,0)) %>% 
  dplyr::mutate(Honte = ifelse(E_Detectee == "Honte",1,0)) %>% 
  dplyr::mutate(RAS = ifelse(E_Detectee == "RAS",1,0)) %>% 
  dplyr::mutate(Stupefaction = ifelse(E_Detectee == "Stupefaction",1,0)) %>% 
  dplyr::mutate(Vexation = ifelse(E_Detectee == "Vexation",1,0))




