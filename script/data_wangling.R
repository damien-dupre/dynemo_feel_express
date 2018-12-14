# setup -------------------------------------------------------------------
library(here)
library(tidyverse)
library(data.table)
# upload data -------------------------------------------------------------
data_sr <- read_rds(here::here("data/Base_Finale_20090114.rds"))

test <- data_sr %>%
  dplyr::select(
    "N° sujet__C",
    "Induction_C",
    "CURIOSITE/INTERET",
    "ENNUI",
    "REPULSION/DEGOUT",
    "FRAYEUR",
    "GAIETE/AMUSEMENT",
    "HONTE",
    "FIERTE/SATISFACTION",
    "STUPEFACTION/SURPRISE",
    "EMU",
    "ENERVEE/IRRITATION",
    "VEXE/VEXATION",
    "DECU",
    "AUTRE"
  )

test2 <- test %>%
  tidyr::gather(
    emotion,
    value,
    c(
      "CURIOSITE/INTERET",
      "ENNUI",
      "REPULSION/DEGOUT",
      "FRAYEUR",
      "GAIETE/AMUSEMENT",
      "HONTE",
      "FIERTE/SATISFACTION",
      "STUPEFACTION/SURPRISE",
      "EMU",
      "ENERVEE/IRRITATION",
      "VEXE/VEXATION",
      "DECU",
      "AUTRE"
    )
  ) %>%
  dplyr::group_by(`N° sujet__C`) %>%
  dplyr::filter(value == max(value))

ggplot(data = test2, mapping = aes(x = Induction_C, y = emotion)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "blue",mid = "green",high = "red", midpoint = 4) +
  theme_minimal() +
  theme(text = element_text(family = "serif", size=8))

test3 <- test2 %>%
  dplyr::group_by(Induction_C) %>%
  dplyr::count(emotion)

unique(test3$Induction_C)

list_emotion_sr <- c(
  "CURIOSITE/INTERET",
  "ENNUI",
  "REPULSION/DEGOUT",
  "FRAYEUR",
  "GAIETE/AMUSEMENT",
  "HONTE",
  "FIERTE/SATISFACTION",
  "STUPEFACTION/SURPRISE",
  "EMU",
  "ENERVEE/IRRITATION",
  "VEXE/VEXATION",
  "DECU",
  "AUTRE"
)

list_induction <- c("AM","DE","EM","EN","FR","HO","IN","IR","NE","SA","SU")

test4 <- expand.grid(list_induction,list_emotion_sr) %>%
  dplyr::rename(Induction_C = Var1, emotion = Var2) %>%
  dplyr::mutate(n = 0)

test5 <- dplyr::anti_join(test4,test3, by = c("Induction_C", "emotion")) %>%
  bind_rows(test3)

test5 %>%
  ggplot(aes(reorder(emotion, -n), n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~Induction_C) +
  theme_minimal() +
  theme(text = element_text(family = "serif", size=8))
  
