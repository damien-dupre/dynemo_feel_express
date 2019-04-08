self_report_score_all <- self_report_tidy %>% 
  dplyr::select(C_Video, emotion, sr_value = value)

human_recognition_score_all <- average_timeline %>%
  tidyr::gather(emotion,value, c("disgust","sadness","anger", "fear", "happiness", "surprise")) %>% 
  dplyr::group_by(C_Video, emotion) %>%
  dplyr::summarise(sum_emotion = sum(value)) %>% 
  dplyr::group_by(C_Video) %>% 
  dplyr::mutate(sum_video = sum(sum_emotion)) %>% 
  dplyr::mutate(confidence_score = sum_emotion/sum_video) %>% 
  dplyr::select(C_Video, emotion, hr_value = confidence_score)

automatic_recognition_score_all <- automatic_recognition_data %>% 
  dplyr::select(C_Video = filename, TimeStamp, happiness = joy, fear, disgust, sadness, anger, surprise) %>% 
  dplyr::mutate(C_Video = stringr::str_replace(C_Video,".csv","")) %>% 
  na.omit() %>% 
  tidyr::gather(emotion, value, c("happiness", "fear", "disgust", "sadness", "anger", "surprise")) %>% 
  dplyr::mutate(probabilities = value/100) %>% 
  dplyr::mutate(odds = probabilities/(1-probabilities)) %>% 
  dplyr::select(C_Video, TimeStamp, emotion, odds) %>% 
  dplyr::group_by(C_Video, emotion) %>%
  dplyr::summarise(sum_emotion = sum(odds)) %>% 
  dplyr::group_by(C_Video) %>% 
  dplyr::mutate(sum_video = sum(sum_emotion)) %>% 
  dplyr::mutate(confidence_score = sum_emotion/sum_video) %>% 
  dplyr::select(C_Video, emotion, ar_value = confidence_score)

recognition_result_all <- self_report_score_all %>% 
  dplyr::left_join(human_recognition_score_all, by = c(
    "C_Video",
    "emotion"
  )
  ) %>% 
  dplyr::left_join(automatic_recognition_score_all, by = c(
    "C_Video",
    "emotion"
  )
  ) %>% 
  replace(is.na(.), 0)

recognition_result_all %>% 
  dplyr::select(-C_Video, -emotion) %>% 
  cor(method = c("pearson")) %>%
  ggcorrplot::ggcorrplot(
    type = "lower",
    outline.col = "white", 
    lab = TRUE, 
    legend.title = "Correlation\n(Pearson)", 
    lab_size = 4)


