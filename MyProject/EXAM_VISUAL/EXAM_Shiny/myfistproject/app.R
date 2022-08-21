library(shiny)
library(gapminder)
library(ggplot2)

      # 여러 사이드로 구성된 페이지 생성
ui <- pageWithSidebar(
  
  # 헤드 패널
  headerPanel(
    tags$h1("header 헤더")
  ),
  
  # 사이드바 패널
  sidebarPanel(
    
  # # 슬라이드바 생성(변수명, 텍스트, 최소값, 최대값, 기본값)
  #   sliderInput("count", "니가 원하는 게 얼마니?",
  #               min = 1, max = 10000, value = 5000)

    
    # 셀렉트바 생성
    selectInput("year", "몇 년도를 보여드릴까요?",
                seq(1952, 2007, 5))
),  
    
    # 메인 패널
  mainPanel(
    
    # # distPlot을 입력받아 그리기
    
    # tags$h3("니가 원하는 그림이 이거지?"),
    # plotOutput("distPlot")
    
    tags$h3("해당 연도의 GDP 대비 기대수명 그래프 입니다."),
    plotOutput("distPlot")
    
  )

)

server <- function(input, output) {
  
  # 그래프 랜더링
  output$distPlot <- renderPlot({
    
  #   # count입력받아 dist생성
  #   dist <- rnorm(input$count)
  #   
  #   # 히스토그램 그려서 반환
  #   hist(dist, col = "tomato", breaks = 20)
  # })

        
    # 갭마인더
    
    ggplot(data = subset(gapminder, year == input$year),
           
           # x축, y축 지정
           mapping = aes(x = gdpPercap, y = lifeExp)) +
      
      # 그래프모양, 색깔
      geom_point(aes(color = continent)) +
      
      # x값에 로그 씌우기
      scale_x_log10() +
      
      # 기하곡선 그리기
      geom_smooth()
    })
    
}









shinyApp(ui, server)