library(shiny)
library(palmerpenguins)

pg <- penguins
pg <- pg[complete.cases(pg),]

mpgData <- mtcars
mpgData$am <- factor(mpgData$am,
                     labels = c("Automatic", "Manual"))

ui <- pageWithSidebar(
  
  headerPanel(
    tags$h1("Miles Per Gallon")
  ),
  
  sidebarPanel(
    
    selectInput("indvar", "독립변수",
                
                # 보여줄땐 한글, 넘겨주는 변수명은 영어
                list("종류별" = "species",
                     "섬별" = "island",
                     "성별" = "sex")
    ),
    
    selectInput("depvar", "종속변수",
                
                # 보여줄땐 한글, 넘겨주는 변수명은 영어
                list("부리 길이" = "bill_length_mm",
                     "부리 깊이" = "bill_depth_mm",
                     "날개 길이" = "flipper_length_mm",
                     "체질량" = "body_mass_g")
    ),
    checkboxInput("outliers", "이상치 보여줌",
                  FALSE)
    
  ),
  
  mainPanel(
    
    tags$h3("포뮬러: "),
    
    tags$h3(textOutput("caption")),
    
    plotOutput("pgPlot")
  )
  
)

server <- function(input, output) {
  
  formulaText <- reactive({
    
    # 스트링 합치기
    paste(input$depvar, "~", input$indvar)
  })
  
  output$caption <- renderText({
    formulaText()
  })
  
  output$pgPlot <- renderPlot({
    
    
    plot(as.formula(formulaText()),
            data <- pg,
            col = c("#B3EFFF", "#00CFFF", "#046B99", "#1C304A"),
            outline = input$outliers)
  })
}












shinyApp(ui, server)