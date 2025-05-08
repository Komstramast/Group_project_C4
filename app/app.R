library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

# Загружаем данные
commits <- read_csv("../data/commits_transformed.csv")


# UI
ui <- fluidPage(
  titlePanel("Анализ поведения разработчиков"),
  sidebarLayout(
    sidebarPanel(
      selectInput("author", "Выберите разработчика", choices = unique(commits$author))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Профиль активности",
                 plotOutput("heatmap"),
                 plotOutput("locPlot")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  author_data <- reactive({
    commits %>% filter(author == input$author)
  })
  
  output$heatmap <- renderPlot({
    author_data() %>%
      count(weekday = weekdays(date), hour) %>%
      ggplot(aes(x = hour, y = reorder(weekday, desc(weekday)), fill = n)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "blue") +
      labs(title = "Активность по времени", x = "Час", y = "День недели")
  })
  
  output$locPlot <- renderPlot({
    author_data() %>%
      ggplot(aes(x = date, y = loc_change)) +
      geom_line() +
      labs(title = "Изменения строк кода со временем", x = "Дата", y = "Изменения LoC")
  })
  
  
}

# Запуск приложения
shinyApp(ui = ui, server = server)
