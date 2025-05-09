# scripts/03_analyze_profiles.R

library(dplyr)
library(readr)

# 📥 Загружаем подготовленные данные
commits <- read_csv("data/commits_transformed.csv")

# 🧠 Агрегируем поведенческие метрики по разработчикам
profiles <- commits %>%
  group_by(author) %>%
  summarise(
    commits_total = n(),
    avg_loc_change = mean(loc_change, na.rm = TRUE),
    avg_added = mean(added, na.rm = TRUE),
    avg_deleted = mean(deleted, na.rm = TRUE),
    avg_msg_length = mean(message_length, na.rm = TRUE),
    avg_commit_hour = mean(hour, na.rm = TRUE),
    std_commit_hour = sd(hour, na.rm = TRUE),
    n_days_active = n_distinct(date),
    avg_files_changed = mean(n_files, na.rm = TRUE)
  ) %>%
  arrange(desc(commits_total))

# 💾 Сохраняем результат
if (!dir.exists("outputs")) dir.create("outputs")
write_csv(profiles, "outputs/developer_profiles.csv")

message("✅ Поведенческие профили сохранены в outputs/developer_profiles.csv")

