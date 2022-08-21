library(shiny)
library(gapminder)
library(ggplot2)

ui <- fluidPage(
  
  tags$h1("갭마인더 따라해보기:"),
  
  # 그래프 보여주기
  tags$h3("그림그리기:"),
  plotOutput("plot", click = "plot_click"),
  
  # 사용자 클릭에 반응하기
  tags$h3("사용자 클릭에 반응하기:"),
  tableOutput("data")
)

server <- function (input, output) {
  
  # 그래프 그리기
  output$plot <- renderPlot({
    
           # 2007년 데이터만
    ggplot(data = subset(gapminder, year == 2007),
           
           # x축, y축 지정
           mapping = aes(x = gdpPercap, y = lifeExp)) +
      
      # 그래프모양, 색깔
      geom_point(aes(color = continent)) +
      
      # x값에 로그 씌우기
      scale_x_log10() +
      
      # 기하곡선 그리기
      geom_smooth()
  })
  
  output$data <- renderTable({
    
    # plot_click라는 input이 있을 때 랜더링
    req(input$plot_click)
    
    # 클릭을 입력받아 포인트근처의 점들을 데이터로 반환
    nearPoints(gapminder, input$plot_click)
  })
}


shinyApp(ui, server)