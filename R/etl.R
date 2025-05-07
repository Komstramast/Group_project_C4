library(dplyr)
library(lubridate)

log_df <- log_df %>%
  mutate(
    hour = hour(date),
    wday = wday(date, label = TRUE, abbr = FALSE),
    weekday = !(wday %in% c("Saturday", "Sunday")),
    date_only = as.Date(date)
  )

profile <- log_df %>%
  group_by(author) %>%
  summarise(
    total_commits = n_distinct(hash),
    total_files_changed = n(),
    total_lines_added = sum(added, na.rm = TRUE),
    total_lines_deleted = sum(deleted, na.rm = TRUE),
    avg_commit_size = mean(added + deleted, na.rm = TRUE),
    first_commit = min(date),
    last_commit = max(date)
  ) %>%
  arrange(desc(total_commits))

print(profile)

library(ggplot2)

activity <- log_df %>%
  group_by(wday, hour) %>%
  summarise(commits = n()) %>%
  ungroup()

ggplot(activity, aes(x = hour, y = wday, fill = commits)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Тепловая карта активности коммитов",
       x = "Час дня", y = "День недели") +
  theme_minimal()

log_df <- log_df %>%
  mutate(time_of_day = case_when(
    hour >= 9 & hour <= 18 ~ "Рабочее время",
    TRUE ~ "Ночь/выходной"
  ))

time_dist <- log_df %>%
  group_by(author, time_of_day) %>%
  summarise(commits = n()) %>%
  tidyr::pivot_wider(names_from = time_of_day, values_from = commits, values_fill = 0)

print(time_dist)