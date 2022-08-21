library(shiny)
library(ggplot2)
library(palmerpenguins)

ui <- pageWithSidebar(
  
  headerPanel(h1("데이터테이블 예제")),
  
  sidebarPanel(
    
    checkboxGroupInput("showvars", "원하는 컬럼은?",
                       names(penguins),
                       selected = names(penguins))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("penguins", dataTableOutput("mytable1")),
      tabPanel("state.x77", dataTableOutput("mytable2")),
      tabPanel("airquality", dataTableOutput("mytable3"))
    )
  )
  
)

server <- function(input, output) {
  
  output$mytable1 <- renderDataTable({
    
    # penguin데이터 중에서 체크된 컬럼만 보여주기
    penguins[, input$showvars, drop = FALSE]
  })
  
  output$mytable2 <- renderDataTable({
    state.x77
  }, options = list(bSortClasses = TRUE))
  
  output$mytable3 <- renderDataTable({
    airquality
  }, options = list(aLengthMenu = c(5, 30, 50),
                    iDisplayLength = 5))
}

shinyApp(ui, server)