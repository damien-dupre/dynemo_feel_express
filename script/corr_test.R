list_video <- self_report_score$C_Video
list_emotion <- c("disgust","fear","happiness","surprise","sadness","anger")

grid_data <- expand.grid(list_video = list_video, list_emotion = list_emotion)

self_report_result <- self_report_score %>% 
  dplyr::filter(sr_emotion != "undetermined") %>% 
  dplyr::mutate(sr_score = 1)

human_recognition_result <- human_recognition_score %>% 
  dplyr::filter(hr_emotion != "undetermined") %>% 
  dplyr::mutate(hr_score = 1)

automatic_recognition_result <- automatic_recognition_score %>% 
  dplyr::filter(ar_emotion != "undetermined") %>% 
  dplyr::mutate(ar_score = 1)

recognition_result <- grid_data %>% 
  dplyr::left_join(self_report_result, by = c(
    "list_video" = "C_Video",
    "list_emotion" = "sr_emotion"
    )
  ) %>% 
  dplyr::left_join(human_recognition_result, by = c(
    "list_video" = "C_Video",
    "list_emotion" = "hr_emotion"
    )
  ) %>% 
  dplyr::left_join(automatic_recognition_result, by = c(
    "list_video" = "C_Video",
    "list_emotion" = "ar_emotion"
    )
  ) %>% 
  replace(is.na(.), 0)

recognition_result %>% 
  dplyr::select(-list_video, -list_emotion) %>% 
  cor(method = c("pearson")) %>%
  ggcorrplot::ggcorrplot(
    type = "lower",
    outline.col = "white", 
    lab = TRUE, 
    legend.title = "Correlation\n(Pearson)", 
    lab_size = 4)

recognition_result %>% 
  ggplot(aes(d = sr_score, m = hr_score)) + 
  plotROC::geom_roc(labels = FALSE,pointsize = 0) 

recognition_result %>% 
  ggplot(aes(d = sr_score, m = ar_score)) + 
  plotROC::geom_roc(labels = FALSE,pointsize = 0) 


human_roc <- pROC::roc(recognition_result$sr_score, recognition_result$hr_score)
human_auc <- pROC::auc(human_roc)
human_auc

automatic_roc <- pROC::roc(recognition_result$sr_score, recognition_result$ar_score)
automatic_auc <- pROC::auc(automatic_roc)
automatic_auc


# ggplot(recognition_result, aes(x=sr_score, y=ar_score)) +
#   geom_point() +
#   geom_smooth(method = "glm", 
#               method.args = list(family = "binomial"), 
#               se = FALSE) 

human_logistic <- lme4::glmer(
  sr_score ~ hr_score + (1 | list_video) + (1 | list_emotion), 
  data = recognition_result, 
  family = binomial, 
  control = lme4::glmerControl(optimizer = "bobyqa")
  )

summary(human_logistic)

automatic_logistic <- lme4::glmer(
  sr_score ~ ar_score + (1 | list_video) + (1 | list_emotion), 
  data = recognition_result, 
  family = binomial, 
  control = lme4::glmerControl(optimizer = "bobyqa")
)

summary(automatic_logistic)

# see prediction accuracy


