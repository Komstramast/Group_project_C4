# run_analysis.R

# === Настройки ===
repo_path <- readline(prompt = "📁 Введите абсолютный путь к репозиторию (формат: owner/repo): ")
github_token <- readline(prompt = "🔐 Введите ваш GitHub токен (GITHUB_PAT): ")

if (repo_path == "" || github_token == "") {
  stop("❌ Необходимо ввести путь к репозиторию и токен GitHub.")
}

# Сохраняем в переменные окружения
Sys.setenv(REPO_PATH = repo_path)
Sys.setenv(GITHUB_PAT = github_token)

# === Запуск анализа ===
scripts <- c(
  "R/01_extract_git_log.R",
  "R/02_clean_transform.R",
  "R/03_analyze_profiles.R",
  "R/04_detect_anomalies.R",
  "R/05_file_history.R"
)

for (script in scripts) {
  message("\n🚀 Запускаем ", script)
  source(script, echo = TRUE, max.deparse.length = 10000)
}

# === Запуск Shiny приложения ===
message("\n🎯 Запускаем Shiny-приложение...")
shiny::runApp("app")
