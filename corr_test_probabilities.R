list_video <- self_report_score$C_Video
list_emotion <- c("disgust","fear","happiness","surprise","sadness","anger")

grid_data <- expand.grid(list_video = list_video, list_emotion = list_emotion)

self_report_result <- self_report_score %>% 
  dplyr::filter(sr_emotion != "undetermined") %>% 
  dplyr::mutate(sr_score = 1)

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

recognition_result_probabilities <- grid_data %>% 
  dplyr::left_join(self_report_result, by = c(
    "list_video" = "C_Video",
    "list_emotion" = "sr_emotion"
  )
  ) %>% 
  dplyr::left_join(human_recognition_score_all, by = c(
    "list_video" = "C_Video",
    "list_emotion" = "emotion"
  )
  ) %>% 
  dplyr::left_join(automatic_recognition_score_all, by = c(
    "list_video" = "C_Video",
    "list_emotion" = "emotion"
  )
  ) %>% 
  replace(is.na(.), 0)


recognition_result_probabilities %>% 
  dplyr::select(-list_video, -list_emotion) %>% 
  cor(method = c("pearson")) %>%
  ggcorrplot::ggcorrplot(
    type = "lower",
    outline.col = "white", 
    lab = TRUE, 
    legend.title = "Correlation\n(Pearson)", 
    lab_size = 4)


recognition_result_probabilities %>% 
  ggplot(aes(d = sr_score, m = hr_value)) + 
  plotROC::geom_roc(labels = FALSE,pointsize = 0) 

recognition_result_probabilities %>% 
  ggplot(aes(d = sr_score, m = ar_value)) + 
  plotROC::geom_roc(labels = FALSE,pointsize = 0) 

human_roc <- pROC::roc(recognition_result_probabilities$sr_score, recognition_result_probabilities$hr_value)
human_auc <- pROC::auc(human_roc)
human_auc

automatic_roc <- pROC::roc(recognition_result_probabilities$sr_score, recognition_result_probabilities$ar_value)
automatic_auc <- pROC::auc(automatic_roc)
automatic_auc

# by emotion -------------------------------------------------------------------

recognition_result_probabilities %>% 
  ggplot(aes(d = sr_score, m = hr_value, color = list_emotion)) + 
  plotROC::geom_roc(labels = FALSE,pointsize = 0)

recognition_result_probabilities %>% 
  ggplot(aes(d = sr_score, m = ar_value, color = list_emotion)) + 
  plotROC::geom_roc(labels = FALSE,pointsize = 0) 


recognition_result_probabilities %>% 
  ggplot(aes(x = hr_value, y = list_emotion)) + 
  ggridges::geom_density_ridges_gradient(aes(fill = ..density..), size = 0.1)

recognition_result_probabilities %>% 
  ggplot(aes(x = ar_value, y = list_emotion)) + 
  ggridges::geom_density_ridges_gradient(aes(fill = ..density..), size = 0.1)


ggplot(recognition_result_probabilities, aes(x=hr_value, y=sr_score)) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)+
  facet_wrap(~list_emotion)

ggplot(recognition_result_probabilities, aes(x=ar_value, y=sr_score)) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)+
  facet_wrap(~list_emotion)
