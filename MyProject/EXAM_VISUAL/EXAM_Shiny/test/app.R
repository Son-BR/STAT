# test

# ======================================================================================

# bar

df <- read.csv("C:/STAT/Project_Visual/result.csv")
df <- df[df$항만명 == "부산", ]
df.test <- df[df$입출항구분=="출항"&df$Measures=="총계",c(1,2,7:37)]

# 연월 합쳐서 새컬럼
df.test$time <- c(paste(df.test$조회년도, df.test$조회월, sep = "_"))
df.test <- df.test[,c(-1,-2)]

value <- c(unlist(t(df.test[,-32])))
data <- data.frame(subject = rep(c(names(df.test)[1:31]),times=60), values=value,time=df.test$time)

# 상위 10 품목
data.name <- names(head(sort(tapply(data$value, data$subject,mean),decreasing = T),10))
data10 <- data[data$subject %in% data.name,]

# data10 <- data10[data10$time == "2017_1",]

# 그래프 그리기
# myPlot <- ggplot(data10, aes(x=subject, y=values, fill=subject)) + 
#   geom_bar(stat='identity') +
#   theme_bw() +
#   # x축 순서 변경
#   scale_x_discrete(limits = c(data.name))

# ======================================================================================

# map
library(ggplot2)
library(dplyr)
library(maps)
library(ggrepel)

# 지도, 위도경도 데이터
ko.map <- map_data("world") %>% filter(region=="South Korea")
data <- world.cities %>% filter(country.etc=="Korea South")
data <- data[data$name %in% c("Pusan","Inchon","Kwangyang","Ulsan"),]
data <- data[,c(1,4:5)]
data$name <- c("인천", "광양", "부산", "울산")

df.all <- read.csv("C:/STAT/Project_Visual/result.csv")
df.all <- df.all[df.all$조회년도 == 2021 & df.all$입출항구분 %in% c("입항", "출항") & df.all$Measures == "수출입계", c(2:4,6)]

# 데이터에 위도 경도 추가
# lat
# c(data[3,2],data[1,2],data[2,2],data[4,2])
# long
# c(data[3,3],data[1,3],data[2,3],data[4,3])
df.all$lat <- rep(c(data[3,2],data[1,2],data[2,2],data[4,2]),time=12,each=2)
df.all$long <- rep(c(data[3,3],data[1,3],data[2,3],data[4,3]),time=12,each=2)
df.ex <- df.all[df.all$입출항구분=="입항",]
df.all <- df.all[df.all$입출항구분=="출항",]

# 기본
# mapplot <- ggplot() +
#   geom_polygon(data = ko.map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.2) +
#   theme_void() + coord_map() + ylim(34,38.5) + xlim(126, 129.6) + 
#   geom_point(data=data, aes(x=long, y = lat, size=3), col="tomato")+
#   theme(legend.position="none")
# mapplot

# ======================================================================================

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  
  dashboardSidebar(
    selectInput("time", "연, 월을 선택하세요",
                               unique(df.test$time))),
  dashboardBody(
    
    tabsetPanel(
      tabPanel("map", plotOutput("mapPlot", click = "plot_click"),
               tableOutput("mapdata")),
      tabPanel("subject10", plotOutput("barPlot"))
    )
  )
)

server <- function(input, output, session) {
  
  output$barPlot <- renderPlot({
    ggplot(data=subset(data10, time == input$time),
           aes(x=subject, y=values, fill=subject)) + 
      geom_bar(stat = 'identity') +
      theme_bw() +
      scale_x_discrete(limits = c(data.name))
  })
  
  output$mapPlot <- renderPlot({
    ggplot() +
        geom_polygon(data = ko.map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.2) +
        theme_void() + coord_map() + ylim(34,38.5) + xlim(126, 129.6) +
        geom_point(data=df.all, aes(x=long, y = lat, size=3), col="tomato")+
        theme(legend.position="none")
  })
  
  output$mapdata <- renderTable({
    req(input$plot_click)
    nearPoints(df.ex[c(1:4),], input$plot_click)
    nearPoints(df.ex[c(5:8),], input$plot_click)
  })
}



shinyApp(ui, server)

