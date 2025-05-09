# 05_file_history.R

library(gh)
library(tidyverse)
library(jsonlite)
library(lubridate)

# --- Настройки ---
repo_owner <- "Komstramast"
repo_name <- "Group_project_C4"
max_commits <- 200  # лимит для примера

# --- Получаем коммиты ---
commits <- gh::gh(
  endpoint = "/repos/:owner/:repo/commits",
  owner = repo_owner,
  repo = repo_name,
  .limit = max_commits
)

# --- Функция для получения файлов коммита ---
get_commit_files <- function(sha) {
  commit_details <- gh::gh(
    endpoint = "/repos/:owner/:repo/commits/:sha",
    owner = repo_owner,
    repo = repo_name,
    sha = sha
  )
  
  files <- commit_details$files
  if (is.null(files)) return(NULL)
  
  tibble(
    sha = sha,
    filename = map_chr(files, "filename"),
    status = map_chr(files, "status"),
    additions = map_dbl(files, "additions"),
    deletions = map_dbl(files, "deletions"),
    changes = map_dbl(files, "changes")
  )
}

# --- Собираем изменения по каждому коммиту ---
file_history <- map_df(commits, function(cmt) {
  message("Fetching: ", cmt$sha)
  files <- get_commit_files(cmt$sha)
  if (!is.null(files)) {
    files$date <- ymd_hms(cmt$commit$author$date)
    files$author <- cmt$commit$author$name
    return(files)
  }
  return(NULL)
})

# --- Сортируем по дате ---
file_history_sorted <- file_history %>%
  arrange(date)

# --- Сохраняем ---
write_csv(file_history_sorted, "data/file_history.csv")
