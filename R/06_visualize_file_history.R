# 06_visualize_file_history.R

library(tidyverse)
library(lubridate)
library(ggplot2)

# --- Загрузка данных ---
file_history <- read_csv("data/file_history.csv")

# --- Препроцессинг ---
file_history <- file_history %>%
  mutate(date = as_datetime(date),
         ext = tools::file_ext(filename),
         week = floor_date(date, "week"))

# --- Визуализация 1: Кол-во изменений по времени (недельно) ---
p1 <- file_history %>%
  group_by(week) %>%
  summarise(total_changes = sum(additions + deletions)) %>%
  ggplot(aes(x = week, y = total_changes)) +
  geom_line(color = "#2c3e50", size = 1) +
  geom_point(color = "#3498db") +
  labs(title = "Объём изменений в репозитории по неделям",
       x = "Неделя", y = "Строк кода (add + del)") +
  theme_minimal()

# --- Визуализация 2: Активность по типам файлов ---
p2 <- file_history %>%
  group_by(week, ext) %>%
  summarise(total_changes = sum(additions + deletions), .groups = "drop") %>%
  filter(ext != "") %>%
  ggplot(aes(x = week, y = total_changes, fill = ext)) +
  geom_area(alpha = 0.8) +
  labs(title = "Изменения по типам файлов", x = "Неделя", y = "Изменения", fill = "Расширение") +
  theme_minimal()

# --- Сохранение ---
ggsave("outputs/file_changes_timeline.png", p1, width = 10, height = 5)
ggsave("outputs/file_type_activity.png", p2, width = 10, height = 5)
