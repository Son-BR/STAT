library(shiny)

ui <- fluidPage(
  
  tags$h1("선택해주세요"),
  
  selectInput("dataset", # server에 넘겨줄 변수명
              
              # 화면에보이는 글 Dataset
              label = "Dataset",
              
              # 텍스트박스 만들어 선택받기
              choices = c("iris", "mtcars", "state.x77")),
  
  # server의 out$table 넘겨 받기
  tableOutput("table"),
  
  # server의 out$ummary 넘겨 받기
  verbatimTextOutput("summary")
  
  
)

server <- function (input, output) {
  
  # dataset 한번에 지정
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  # ui에 넘겨줄 summary 정의
  output$summary <- renderPrint({
    
    # dataset 지정(reactive로 지정하면 생략가능)
    # dataset <- get(input$dataset, "package:datasets")
    
    summary(dataset())
  })
  
  # ui에 넘겨줄 table 정의
  output$table <- renderTable({
    
    # dataset 지정(reactive로 지정하면 생략가능)
    # dataset <- get(input$dataset, "package:datasets")
    
    # 디버깅(콘솔에서 데이터셋 확인)
    cat(input$dataset, "\n")
    
    head(dataset(), 10)
  })
}

shinyApp(ui, server)

