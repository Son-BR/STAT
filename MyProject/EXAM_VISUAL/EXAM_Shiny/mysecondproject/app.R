library(shiny)

mpgData <- mtcars
mpgData$am <- factor(mpgData$am,
                     labels = c("Automatic", "Manual"))

ui <- pageWithSidebar(
  
  headerPanel(
    tags$h1("Miles Per Gallon")
  ),
  
  sidebarPanel(
    selectInput("variable", "선택해보세요",
                
                # 보여줄땐 한글, 넘겨주는 변수명은 영어
                list("실린더" = "cyl",
                     "트랜스미션" = "am",
                     "기어" = "gear")
                ),
    checkboxInput("outliers", "이상치 보여줌",
                  FALSE)
    
  ),
  
  mainPanel(
    
    tags$h3("포뮬러: "),
    
    tags$h3(textOutput("caption")),
    
    plotOutput("mpgPlot")
  )
  
)

server <- function(input, output) {
  
  formulaText <- reactive({
    
    # 스트링 합치기
    paste("mpg ~ ", input$variable)
  })
  
  output$caption <- renderText({
    formulaText()
  })
  
  output$mpgPlot <- renderPlot({
    
    boxplot(as.formula(formulaText()),
            data = mpgData,
            col = "orange",
            outline = input$outliers)
  })
}
















shinyApp(ui, server)