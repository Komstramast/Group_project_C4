# scripts/02_clean_transform.R

library(dplyr)
library(lubridate)
library(readr)
library(stringr)

# 📥 Загружаем сырые данные
commits <- read_csv("data/commits_clean.csv")

# 🧹 Очистка и обогащение
commits_clean <- commits %>%
  mutate(
    author = tolower(author),
    weekday = wday(date, label = TRUE),    # день недели
    hour = hour(date),                     # час коммита
    message_length = str_length(message),  # длина сообщения
    is_merge = str_detect(tolower(message), "^merge"),
    loc_change = added + deleted,
    date = as_datetime(date)
  ) %>%
  filter(!is.na(author), !is.na(date)) %>%
  arrange(author, date)

# ✨ Удалим дубликаты и очень старые коммиты, если надо
commits_clean <- commits_clean %>%
  distinct(sha, .keep_all = TRUE) %>%
  filter(year(date) >= 2018)  # например, отсекаем до 2018

# 💾 Сохраняем
write_csv(commits_clean, "data/commits_transformed.csv")
message("✅ Трансформация завершена: data/commits_transformed.csv")
