library(shiny)
library(tidyverse)
library(DT)
library(fmsb)

# === Загрузка данных ===
commits <- read_csv("../data/commits_transformed.csv")
anomalies <- read_csv("../outputs/anomalies.csv")
profiles <- read_csv("../outputs/developer_profiles.csv")
file_history <- read_csv("../data/file_history.csv") %>%
  mutate(ext = tools::file_ext(filename)) # Добавление расширения файла

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
                 selectInput("author", "Выберите разработчика:",
                             choices = unique(commits$author))
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
                 selectInput("fh_author", "Разработчик:",
                             choices = unique(file_history$author)),
                 selectInput("fh_ext", "Тип файла:",
                             choices = unique(file_history$ext)),
                 dateRangeInput("fh_date", "Диапазон дат:",
                                start = min(file_history$date),
                                end = max(file_history$date))
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
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$locHistogram <- renderPlot({
    ggplot(profiles, aes(x = avg_loc_change)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      labs(title = "Среднее изменение строк кода (LoC)",
           x = "LoC", y = "Количество разработчиков") +
      theme_minimal()
  })
  
  # === Радар-карта профиля ===
  output$radarPlot <- renderPlot({
    df <- commits %>%
      filter(author == input$author) %>%
      summarise(
        avg_loc = mean(loc_change, na.rm = TRUE),
        avg_added = mean(added, na.rm = TRUE),
        avg_deleted = mean(deleted, na.rm = TRUE),
        msg_len = mean(message_length, na.rm = TRUE),
        activity_hour = mean(hour, na.rm = TRUE),
        files = mean(n_files, na.rm = TRUE)
      )
    
    radar_df <- rbind(
      max = c(200, 150, 100, 100, 24, 20),
      min = c(0, 0, 0, 0, 0, 0),
      df
    )
    rownames(radar_df) <- c("max", "min", input$author)
    
    radarchart(radar_df,
               axistype = 1,
               pcol = "blue",
               pfcol = rgb(0.2, 0.5, 1, 0.4),
               plwd = 2,
               cglcol = "grey", cglty = 1,
               axislabcol = "grey", caxislabels = seq(0, 200, 50), cglwd = 0.8,
               vlcex = 0.8,
               title = paste("Радар-профиль:", input$author))
  })
  
  # === Аномалии ===
  output$anomalyTable <- DT::renderDataTable({
    anomalies %>%
      filter(is_anomaly == TRUE) %>%
      select(author, date, message, loc_change, hour) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$anomalyHoursPlot <- renderPlot({
    anomalies %>%
      filter(is_anomaly == TRUE) %>%
      ggplot(aes(x = hour)) +
      geom_histogram(bins = 24, fill = "red", color = "black") +
      labs(title = "Часы активности аномальных коммитов",
           x = "Час", y = "Количество аномалий") +
      theme_minimal()
  })
  
  # === История изменений файлов ===
  output$fileHistoryTable <- DT::renderDataTable({
    file_history %>%
    filter(author == input$fh_author,
           ext == input$fh_ext,
           date >= input$fh_date[1],
           date <= input$fh_date[2]) %>%
      select(date, author, filename, status, additions, deletions, ext) %>%
      arrange(desc(date)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$fileChangePlot <- renderPlot({
    file_history %>%
      filter(author == input$fh_author,
             ext == input$fh_ext,
             date >= input$fh_date[1],
             date <= input$fh_date[2]) %>%
      group_by(week = lubridate::floor_date(date, "week")) %>%
      summarise(changes = sum(additions + deletions, na.rm = TRUE)) %>%
      ggplot(aes(x = week, y = changes)) +
      geom_line(color = "purple") +
      labs(title = "Изменения по неделям", x = "Неделя", y = "LoC") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)