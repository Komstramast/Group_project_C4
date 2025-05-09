# 04_detect_anomalies.R

# --- Загрузка библиотек ---
library(tidyverse)
library(isotree)

# --- Загрузка подготовленных данных ---
commits <- read_csv("data/commits_transformed.csv")

# --- Подготовка признаков ---
# Отбираем числовые переменные, влияющие на поведение
features <- commits %>%
  select(loc_change, added, deleted, hour, message_length, n_files) %>%
  mutate_all(~replace_na(., 0))  # заменим NA на 0

# --- Обучение модели Isolation Forest ---
model <- isolation.forest(features, ntrees = 100, sample_size = 256)

# --- Предсказание аномалий ---
commits$anomaly_score <- predict(model, features)
commits$is_anomaly <- commits$anomaly_score > 0.65  # Порог можно подбирать

# --- Сохраняем результат ---
write_csv(commits, "outputs/anomalies.csv")

