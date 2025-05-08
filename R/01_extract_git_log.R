# scripts/01_extract_git_log.R

# 🔧 Зависимости
library(gh)
library(dplyr)
library(purrr)
library(lubridate)
library(readr)

# ✅ Настройки репозитория
owner <- "Komstramast"  # ЗАМЕНИ!
repo <- "Group_project_C4"         # ЗАМЕНИ!
n_commits <- 100                 # Сколько коммитов извлечь

# 💡 Убедись, что токен установлен
Sys.setenv(GITHUB_PAT = "ghp")  

# 📥 Получение коммитов
message("📥 Получаем список коммитов...")
commits <- gh(
  "/repos/{owner}/{repo}/commits",
  owner = owner, repo = repo,
  per_page = n_commits
)

commits_df <- map_df(commits, function(x) {
  tibble(
    sha     = x$sha,
    author  = if (!is.null(x$commit$author$name)) x$commit$author$name else NA,
    email   = if (!is.null(x$commit$author$email)) x$commit$author$email else NA,
    date    = ymd_hms(x$commit$author$date),
    message = x$commit$message
  )
})

# 📊 Функция: получаем детали по коммиту
get_commit_details <- function(sha) {
  detail <- gh("/repos/{owner}/{repo}/commits/{sha}",
               owner = owner, repo = repo, sha = sha)
  
  tibble(
    sha     = detail$sha,
    total   = detail$stats$total,
    added   = detail$stats$additions,
    deleted = detail$stats$deletions,
    n_files = length(detail$files)
  )
}

# 🔄 Обходим все SHA и получаем детали
message("📊 Получаем подробности по каждому коммиту...")
details_list <- map(commits_df$sha, safely(get_commit_details))

# Отфильтруем удачные
details_df <- map_df(details_list, function(res) {
  if (!is.null(res$result)) return(res$result)
  else return(NULL)
})

# 🧩 Объединение таблиц
final_df <- left_join(commits_df, details_df, by = "sha")

# 💾 Сохраняем
if (!dir.exists("data")) dir.create("data")
write_csv(final_df, "data/commits_clean.csv")
message("✅ Готово! Сохранено в data/commits_clean.csv")
