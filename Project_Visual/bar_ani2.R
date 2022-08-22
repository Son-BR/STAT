# 년/월 애니메이션
df <- read.csv("./Project_Visual/result.csv")
df <- df[df$항만명 == "부산", ]

df
# libraries:
library(ggplot2)
library(gganimate)

# df.test.2017 <- df[df$조회년도==2017&df$입출항구분=="입항"&df$Measures=="총계",c(2,7:37)]
# df.test.2018 <- df[df$조회년도==2018&df$입출항구분=="입항"&df$Measures=="총계",c(2,7:37)]
# df.test.2019 <- df[df$조회년도==2019&df$입출항구분=="입항"&df$Measures=="총계",c(2,7:37)]
# df.test.2020 <- df[df$조회년도==2020&df$입출항구분=="입항"&df$Measures=="총계",c(2,7:37)]
# df.test.2021 <- df[df$조회년도==2021&df$입출항구분=="입항"&df$Measures=="총계",c(2,7:37)]

df.test <- df[df$입출항구분=="입항"&df$Measures=="총계",c(1,2,7:37)]
df.test

# Make 2 basic states and concatenate them:

# 연월 합쳐서 새컬럼
df.test$time <- c(paste(df.test$조회년도, df.test$조회월, sep = "_"))
df.test <- df.test[,c(-1,-2)]
df.test
names(df.test)
length(row.names(df.test))

#value <- c(unlist(t(df.test[,-32])))
value <- c(unlist(t(df.test[,-32])))
length(value)

data <- data.frame(subject = rep(c(names(df.test)[1:31]),times=60), values=value,time=df.test$time)

length(data$values)
# 상위 10개 품목 이름
data.name <- names(head(sort(tapply(data$value, data$subject,mean),decreasing = T),10))
data.name

# 상위 10개 품목 테이터 프레임
data10 <- data[data$subject %in% data.name,]
data10

 

# Make a ggplot, but add frame=year: one image per year
myPlot <- ggplot(data10, aes(x=subject, y=values, fill=subject)) + 
  geom_bar(stat='identity') +
  theme_bw() +

  # x축 순서 변경
  scale_x_discrete(limits = c(data.name)) +
  # gganimate specific bits:
  transition_states(
    time,
    transition_length = 2,
    state_length = 1
  ) +
  # 그래프 이름
  ggtitle('Now showing {closest_state}',
          #subtitle = 'Frame {frame} of {nframes}'
          ) +

  ease_aes('sine-in-out')

# Save at gif:

library(gifski)
library(png)

animate(myPlot, duration = 10, fps = 20, width = 2000, height = 400, renderer = gifski_renderer())
anim_save("year_month_slow.gif")

