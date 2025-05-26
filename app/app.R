library(shiny)
library(tidyverse)
library(DT)
library(fmsb)

# === Функция для запуска анализа и обновления данных ===
run_analysis <- function(repo_path, github_token) {
  Sys.setenv(REPO_PATH = repo_path, GITHUB_PAT = github_token)
  scripts <- c(
    "../R/01_extract_git_log.R",
    "../R/02_clean_transform.R",
    "../R/03_analyze_profiles.R",
    "../R/04_detect_anomalies.R",
    "../R/05_file_history.R"
  )
  for (script in scripts) {
    message("🚀 Запускаем ", script)
    source(script, echo = TRUE, max.deparse.length = 10000)
  }
}

# === Функция для загрузки подготовленных данных ===
load_data <- function() {
  list(
    commits      = read_csv("../data/commits_transformed.csv"),
    anomalies    = read_csv("../outputs/anomalies.csv"),
    profiles     = read_csv("../outputs/developer_profiles.csv"),
    file_history = read_csv("../data/file_history.csv") %>% mutate(ext = tools::file_ext(filename))
  )
}

# === UI ===
ui <- fluidPage(
  tags$head(tags$style(HTML(
    "html, body {width:100%;height:100%;margin:0;padding:0;overflow-x:hidden;} .container-fluid {padding: 5px;}"
  ))),
  titlePanel("Анализ поведения разработчиков из GitHub"),
  tabsetPanel(
    id = "mainTabs", type = "tabs",
    
    tabPanel("📊 Главная",
             fluidRow(
               column(6, DT::dataTableOutput("topAuthors")),
               column(6, plotOutput("locHistogram", width = "100%", height = "400px"))
             )
    ),
    tabPanel("🧬 Профиль разработчика",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("author", "Выберите разработчика:", choices = NULL)
               ),
               mainPanel(
                 width = 9,
                 plotOutput("radarPlot", width = "100%", height = "600px")
               )
             )
    ),
    tabPanel("🚨 Аномалии",
             fluidRow(
               column(12, DT::dataTableOutput("anomalyTable")),
               column(12, plotOutput("anomalyHoursPlot", width = "100%", height = "300px"))
             )
    ),
    tabPanel("🗂 История изменений файлов",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("fh_author", "Разработчик:", choices = NULL),
                 selectInput("fh_ext", "Тип файла:", choices = NULL),
                 dateRangeInput("fh_date", "Диапазон дат:", start = NULL, end = NULL)
               ),
               mainPanel(
                 width = 9,
                 DT::dataTableOutput("fileHistoryTable"),
                 plotOutput("fileChangePlot", width = "100%", height = "300px")
               )
             )
    ),
    tabPanel("⚙️ Репозиторий",
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 textInput("repo_path", "Путь к репозиторию (owner/repo):", value = Sys.getenv("REPO_PATH")),
                 textInput("github_token", "GitHub токен (GITHUB_PAT):", value = Sys.getenv("GITHUB_PAT")),
                 actionButton("updateRepo", "Обновить данные"), br(), br(), verbatimTextOutput("updateStatus")
               ),
               mainPanel(
                 width = 8,
                 h4("Нажмите 'Обновить данные' для перезагрузки всех вкладок.")
               )
             )
    )
  )
)

# === Server ===
server <- function(input, output, session) {
  rv <- reactiveValues(data = load_data())
  observe({
    df <- rv$data
    updateSelectInput(session, "author", choices = unique(df$commits$author))
    updateSelectInput(session, "fh_author", choices = unique(df$file_history$author))
    updateSelectInput(session, "fh_ext", choices = unique(df$file_history$ext))
    updateDateRangeInput(session, "fh_date",
                         start = min(df$file_history$date),
                         end   = max(df$file_history$date)
    )
  })
  
  observeEvent(input$updateRepo, {
    output$updateStatus <- renderText("Обновление данных...")
    req(input$repo_path, input$github_token)
    withProgress(message = "Запуск анализа", value = 0, {
      run_analysis(input$repo_path, input$github_token)
      incProgress(0.7)
      rv$data <- load_data()
      incProgress(1)
    })
    output$updateStatus <- renderText("Данные успешно обновлены!")
  })
  
  output$topAuthors <- DT::renderDataTable({
    df <- rv$data$profiles
    df %>%
      top_n(10, commits_total) %>%
      arrange(desc(commits_total)) %>%
      rename(
        Автор = author,
        `Всего коммитов` = commits_total,
        `Среднее изменение LoC` = avg_loc_change,
        `Среднее добавление строк` = avg_added,
        `Среднее удаление строк` = avg_deleted,
        `Средняя длина сообщений` = avg_msg_length,
        `Средний час коммита` = avg_commit_hour,
        `Станд. отклонение часа` = std_commit_hour,
        `Активных дней` = n_days_active,
        `Среднее число файлов` = avg_files_changed
      ) %>% datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$locHistogram <- renderPlot({
    df <- rv$data$commits
    ggplot(df, aes(x = loc_change)) +
      geom_histogram(bins = 50, fill = "steelblue", color = "white") +
      labs(title = "Распределение изменений строк кода (LoC)", x = "LoC", y = "Количество коммитов") +
      theme_minimal()
  })
  
  output$radarPlot <- renderPlot({
    df <- rv$data$commits
    metrics <- df %>%
      group_by(author) %>%
      summarise(
        avg_loc = mean(loc_change, na.rm = TRUE),
        avg_added = mean(added, na.rm = TRUE),
        avg_deleted = mean(deleted, na.rm = TRUE),
        msg_len = mean(message_length, na.rm = TRUE),
        activity_hour = mean(hour, na.rm = TRUE),
        files = mean(n_files, na.rm = TRUE)
      ) %>% ungroup()
    vals <- metrics %>% filter(author == input$author) %>% select(-author)
    max_vals <- sapply(metrics[-1], max, na.rm = TRUE)
    min_vals <- rep(0, length(max_vals))
    radar_df <- as.data.frame(rbind(max_vals, min_vals, vals))
    rus_names <- c(
      avg_loc = "Среднее изменение LoC",
      avg_added = "Среднее добавление строк",
      avg_deleted = "Среднее удаление строк",
      msg_len = "Средняя длина сообщения",
      activity_hour = "Средний час коммита",
      files = "Среднее число файлов"
    )
    colnames(radar_df) <- rus_names[colnames(radar_df)]
    rownames(radar_df) <- c("max", "min", input$author)
    axis_max    <- ceiling(max(max_vals) / 10) * 10
    axis_breaks <- seq(0, axis_max, length.out = 5)
    radarchart(
      radar_df,
      axistype    = 1,
      pcol        = "blue",
      pfcol       = rgb(0.2, 0.5, 1, 0.4),
      plwd        = 2,
      cglcol      = "gray",
      cglty       = 1,
      cglwd       = 0.8,
      axislabcol  = "gray",
      caxislabels = axis_breaks,
      vlcex       = 0.8,
      title       = paste("Радар-профиль:", input$author)
    )
  })
  
  output$anomalyTable <- DT::renderDataTable({
    rv$data$anomalies %>%
      filter(is_anomaly) %>%
      rename(
        SHA = sha,
        Автор = author,
        Дата = date,
        `Длина сообщения` = message_length,
        `Всего изменений` = loc_change,
        `Добавлено` = added,
        `Удалено` = deleted,
        `Файлов изменено` = n_files,
        `Скор аномалии` = anomaly_score
      ) %>% datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$anomalyHoursPlot <- renderPlot({
    ggplot(rv$data$anomalies %>% filter(is_anomaly), aes(x = hour)) +
      geom_histogram(bins = 24, fill = "red", color = "black") +
      labs(title = "Часы активности аномальных коммитов", x = "Час", y = "Кол-во аномалий") +
      theme_minimal()
  })
  
  output$fileHistoryTable <- DT::renderDataTable({
    rv$data$file_history %>%
      filter(
        author == input$fh_author,
        ext == input$fh_ext,
        date >= input$fh_date[1],
        date <= input$fh_date[2]
      ) %>% rename(
        Дата = date,
        Автор = author,
        Файл = filename,
        Добавлено = additions,
        Удалено = deletions
      ) %>%
      arrange(desc(Дата)) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$fileChangePlot <- renderPlot({
    rv$data$file_history %>%
      filter(
        author == input$fh_author,
        ext == input$fh_ext,
        date >= input$fh_date[1],
        date <= input$fh_date[2]
      ) %>% group_by(week = lubridate::floor_date(date, "week")) %>% summarise(changes = sum(additions + deletions, na.rm = TRUE)) %>%
      ggplot(aes(x = week, y = changes)) +
      geom_line(color = "purple") +
      labs(title = "Изменения по неделям", x = "Неделя", y = "LoC") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
