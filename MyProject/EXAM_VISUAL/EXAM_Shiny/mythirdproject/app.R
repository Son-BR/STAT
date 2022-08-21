library(shiny)
library(ggplot2)

ui <- pageWithSidebar(
  
  headerPanel(h1("데이터테이블 예제")),
  
  sidebarPanel(
    
    checkboxGroupInput("showvars", "원하는 컬럼은?",
                       names(diamonds),
                       selected = names(diamonds))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("diamonds", dataTableOutput("mytable1")),
      tabPanel("iris", dataTableOutput("mytable2")),
      tabPanel("mtcars", dataTableOutput("mytable3"))
    )
  )
  
)

server <- function(input, output) {
  
  output$mytable1 <- renderDataTable({
    
    # diamonds데이터 중에서 체크된 컬럼만 보여주기
    diamonds[, input$showvars, drop = FALSE]
  })
  
  output$mytable2 <- renderDataTable({
    iris
  }, options = list(bSortClasses = TRUE))
  
  output$mytable3 <- renderDataTable({
    mtcars
  }, options = list(aLengthMenu = c(5, 30, 50),
                    iDisplayLength = 5))
}

shinyApp(ui, server)