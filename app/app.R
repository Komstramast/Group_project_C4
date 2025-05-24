library(shiny)
library(tidyverse)
library(DT)
library(fmsb)

# === Загрузка данных ===
commits <- read_csv("../data/commits_transformed.csv")
anomalies <- read_csv("../outputs/anomalies.csv")
profiles <- read_csv("../outputs/developer_profiles.csv")
file_history <- read_csv("../data/file_history.csv") %>%
  mutate(ext = tools::file_ext(filename))

ui <- fluidPage(
  titlePanel("Анализ поведения разработчиков из GitHub"),
  tabsetPanel(
    tabPanel("📊 Главная",
             DT::dataTableOutput("topAuthors"),
             plotOutput("locHistogram")
    ),
    tabPanel("🧬 Профиль разработчика",
             sidebarLayout(
               sidebarPanel(
                 selectInput("author", "Выберите разработчика:", choices = unique(commits$author))
               ),
               mainPanel(
                 plotOutput("radarPlot")
               )
             )
    ),
    tabPanel("🚨 Аномалии",
             DT::dataTableOutput("anomalyTable"),
             plotOutput("anomalyHoursPlot")
    ),
    tabPanel("🗂 История изменений файлов",
             sidebarLayout(
               sidebarPanel(
                 selectInput("fh_author", "Разработчик:", choices = unique(file_history$author)),
                 selectInput("fh_ext",    "Тип файла:", choices = unique(file_history$ext)),
                 dateRangeInput("fh_date","Диапазон дат:", start = min(file_history$date), end = max(file_history$date))
               ),
               mainPanel(
                 DT::dataTableOutput("fileHistoryTable"),
                 plotOutput("fileChangePlot")
               )
             )
    )
  )
)

server <- function(input, output) {
  # === Сводка профилей ===
  output$topAuthors <- DT::renderDataTable({
    profiles %>%
      top_n(10, commits_total) %>%
      arrange(desc(commits_total)) %>%
      rename(
        Автор                 = author,
        "Всего коммитов"     = commits_total,
        "Среднее изменение LoC" = avg_loc_change,
        "Среднее добавление строк" = avg_added,
        "Среднее удаление строк"   = avg_deleted,
        "Средняя длина сообщений"   = avg_msg_length,
        "Средний час коммита"        = avg_commit_hour,
        "Станд. отклонение часа"     = std_commit_hour,
        "Активных дней"              = n_days_active,
        "Среднее число файлов"       = avg_files_changed
      ) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$locHistogram <- renderPlot({
    ggplot(profiles, aes(x = avg_loc_change)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      labs(title = "Среднее изменение строк кода (LoC)", x = "LoC", y = "Количество разработчиков") +
      theme_minimal()
  })
  
  # === Радар-карта профиля ===
  output$radarPlot <- renderPlot({
    metrics_all <- commits %>%
      group_by(author) %>%
      summarise(
        avg_loc       = mean(loc_change,     na.rm = TRUE),
        avg_added     = mean(added,          na.rm = TRUE),
        avg_deleted   = mean(deleted,        na.rm = TRUE),
        msg_len       = mean(message_length, na.rm = TRUE),
        activity_hour = mean(hour,           na.rm = TRUE),
        files         = mean(n_files,        na.rm = TRUE)
      ) %>% ungroup()
    
    max_vals <- sapply(metrics_all[-1], max, na.rm = TRUE)
    min_vals <- setNames(rep(0, length(max_vals)), names(max_vals))
    
    df_author <- metrics_all %>%
      filter(author == input$author) %>%
      select(-author)
    
    rus_names <- c(
      avg_loc       = "Среднее изменение LoC",
      avg_added     = "Среднее добавление строк",
      avg_deleted   = "Среднее удаление строк",
      msg_len       = "Средняя длина сообщения",
      activity_hour = "Средний час коммита",
      files         = "Среднее число файлов"
    )
    
    radar_df <- as.data.frame(rbind(max_vals, min_vals, df_author))
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
      cglcol      = "grey",
      cglty       = 1,
      cglwd       = 0.8,
      axislabcol  = "grey",
      caxislabels = axis_breaks,
      vlcex       = 0.8,
      title       = paste("Радар-профиль:", input$author)
    )
  })
  
  # === Аномалии ===
  output$anomalyTable <- DT::renderDataTable({
    anomalies %>%
      filter(is_anomaly) %>%
      rename(
        SHA               = sha,
        Автор             = author,
        Email             = email,
        Дата              = date,
        День_недели       = weekday,
        Час_коммита       = hour,
        Длина_сообщения   = message_length,
        "Merge-коммит"   = is_merge,
        Сообщение         = message,
        Общие_изменения   = total,
        "Всего изменений"= loc_change,
        "Добавлено строк" = added,
        "Удалено строк"   = deleted,
        "Файлов изменено" = n_files,
        "Скор аномалии"   = anomaly_score,
        "Признак аномалии" = is_anomaly
      ) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$anomalyHoursPlot <- renderPlot({
    anomalies %>%
      filter(is_anomaly) %>%
      ggplot(aes(x = hour)) +
      geom_histogram(bins = 24, fill = "red", color = "black") +
      labs(title = "Часы активности аномальных коммитов", x = "Час", y = "Кол-во аномалий") +
      theme_minimal()
  })
  
  # === История изменений файлов ===
  output$fileHistoryTable <- DT::renderDataTable({
    file_history %>%
      filter(
        author == input$fh_author,
        ext    == input$fh_ext,
        date   >= input$fh_date[1],
        date   <= input$fh_date[2]
      ) %>%
      rename(
        Дата       = date,
        Автор      = author,
        Файл       = filename,
        Статус     = status,
        Добавлено  = additions,
        Удалено    = deletions,
        Расширение = ext
      ) %>%
      arrange(desc(Дата)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$fileChangePlot <- renderPlot({
    file_history %>%
      filter(
        author == input$fh_author,
        ext    == input$fh_ext,
        date   >= input$fh_date[1],
        date   <= input$fh_date[2]
      ) %>%
      group_by(week = lubridate::floor_date(date, "week")) %>%
      summarise(changes = sum(additions + deletions, na.rm = TRUE)) %>%
      ggplot(aes(x = week, y = changes)) +
      geom_line(color = "purple") +
      labs(title = "Изменения по неделям", x = "Неделя", y = "LoC") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)