# scripts/05_file_history.R

# 🔧 Зависимости
library(gh)
library(tidyverse)
library(lubridate)
library(stringr)

# ✅ Получение настроек из переменных окружения
repo_path <- Sys.getenv("REPO_PATH")
github_token <- Sys.getenv("GITHUB_PAT")

if (repo_path == "" || github_token == "") {
  stop("❌ Переменные окружения REPO_PATH и GITHUB_PAT должны быть установлены.")
}

# 📦 Разделение owner/repo
parts <- str_split(repo_path, "/", simplify = TRUE)
if (ncol(parts) != 2) {
  stop("❌ Формат REPO_PATH должен быть: owner/repo")
}
owner <- parts[1]
repo <- parts[2]

# 🔐 Установка токена
Sys.setenv(GITHUB_PAT = github_token)

# 🔢 Лимит коммитов
max_commits <- 200

# 📥 Получаем коммиты
message("📥 Получаем список коммитов для истории файлов...")
commits <- gh::gh(
  endpoint = "/repos/:owner/:repo/commits",
  owner = owner,
  repo = repo,
  .limit = max_commits
)

# 🧾 Функция для получения файлов по коммиту
get_commit_files <- function(sha) {
  commit_details <- gh::gh(
    endpoint = "/repos/:owner/:repo/commits/:sha",
    owner = owner,
    repo = repo,
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

# 🔄 Собираем историю изменений файлов
file_history <- map_df(commits, function(cmt) {
  message("🔎 Обрабатываем коммит: ", cmt$sha)
  files <- get_commit_files(cmt$sha)
  if (!is.null(files)) {
    files$date <- ymd_hms(cmt$commit$author$date)
    files$author <- cmt$commit$author$name
    return(files)
  }
  return(NULL)
})

# 📊 Сортировка и сохранение
file_history_sorted <- file_history %>%
  arrange(date)

if (!dir.exists("data")) dir.create("data")
write_csv(file_history_sorted, "data/file_history.csv")
message("✅ История изменений файлов сохранена в data/file_history.csv")