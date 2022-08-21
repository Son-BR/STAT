
# 웹페이지 만들기 연습

library(shiny)

# UI

# fluidPage: 함수, 괄호안에 파라미터(인수), 콤마로 구분
ui <- fluidPage(
  # h1: 마크다운 #과 동일한 기능
  tags$h1("h1 안녕"),
  # 이미지 삽입
  tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0f/IU_posing_for_Marie_Claire_Korea_March_2022_issue_03.jpg/375px-IU_posing_for_Marie_Claire_Korea_March_2022_issue_03.jpg")
)

# Server
server <- function (input, output) {
  
}

shinyApp(ui, server)













