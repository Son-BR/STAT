# 데이터 불러오기
df <- read.csv("./Project_Visual/result.csv")
df <- df[df$항만명 == "부산", ]

str(df)

head(df)

sum(is.na(df))

# 부산만 추출
df <- df[df$항만명 == "부산", ]

# 결측치 확인
sum(is.na(df))
library(VIM)
aggr(df, prop = FALSE, numbers = TRUE)

# ==================================================================================

# 히스토그램 애니메이션 예시

# libraries:
library(ggplot2)
library(gganimate)
 
# Make 2 basic states and concatenate them:
a <- data.frame(group=c("A","B","C"), values=c(3,2,4), frame=rep('a',3))
b <- data.frame(group=c("A","B","C"), values=c(5,3,7), frame=rep('b',3))
data <- rbind(a,b)
data
 
# Basic barplot:
ggplot(a, aes(x=group, y=values, fill=group)) + 
  geom_bar(stat='identity')
 
# Make a ggplot, but add frame=year: one image per year
myPlot <- ggplot(data, aes(x=group, y=values, fill=group)) + 
  geom_bar(stat='identity') +
  theme_bw() +
  # gganimate specific bits:
  transition_states(
    frame,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out')

# Save at gif:

library(gifski)
library(png)

animate(myPlot, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
anim_save("288-animated-barplot-transition.gif")



# ==================================================================================

# 히스토그램 애니메이션

# libraries:
library(ggplot2)
library(gganimate)

df.test <- df[df$조회년도==2017&df$입출항구분=="입항"&df$Measures=="총계",c(2,7:38)]
names(df.test)[2:33]
df.test
# Make 2 basic states and concatenate them:
# a <- data.frame(subject = c(names(df.test)[2:33]), values=as.numeric(df.test[1,c(2:33)]),month=c(1))
# b <- data.frame(subject = c(names(df.test)[2:33]), values=as.numeric(df.test[2,c(2:33)]),month=c(2))

data <- data.frame(subject = c(names(df.test)[2:33]), values=as.numeric(df.test[1,c(2:33)]),month=c(1))

for (i in (2:12)) {
  b <- data.frame(subject = c(names(df.test)[2:33]), values=as.numeric(df.test[i,c(2:33)]),month=c(i))
  data <- rbind(data,b)
}
data
 
# Basic barplot:
ggplot(b, aes(x=subject, y=values, fill=subject)) + 
  geom_bar(stat='identity')
 
# Make a ggplot, but add frame=year: one image per year
myPlot <- ggplot(data, aes(x=subject, y=values, fill=subject)) + 
  geom_bar(stat='identity') +
  theme_bw() +
  # gganimate specific bits:
  transition_states(
    month,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out')

# Save at gif:

library(gifski)
library(png)

animate(myPlot, duration = 5, fps = 20, width = 2000, height = 400, renderer = gifski_renderer())
anim_save("288-animated-barplot-transition.gif")


# ======================================================================

#tapply(벡터,그룹화기준,함수)

df.test10 <- df[df$조회년도==2017&df$입출항구분=="입항"&df$Measures=="총계",c(2,7:37)]
# Make 2 basic states and concatenate them:

c(names(df.test)[2:33])
data.10 <- data.frame(subject = c(names(df.test)[2:32]), values=as.numeric(df.test[1,c(2:32)]),month=c(1))

for (i in (2:12)) {
  b <- data.frame(subject = c(names(df.test)[2:32]), values=as.numeric(df.test[i,c(2:32)]),month=c(i))
  data.10 <- rbind(data.10,b)
}
data.10
data.10.names<- names(head(sort(tapply(data.10$values,data.10$subject,mean),decreasing = T),10))

data.10 <- data.10[data.10$subject %in% names(head(sort(tapply(data.10$values,data.10$subject,mean),decreasing = T),10)),]

length(row.names(data.10[data.10$subject %in% names(head(sort(tapply(data.10$values,data.10$subject,mean),decreasing = T),10)),]))

data.10.sort <- data.10[order(data.10$month,-data.10$values),]

data.10.sort
# Make a ggplot, but add frame=year: one image per year
myPlot10.sort <- ggplot(data.10.sort, aes(x=subject, y=values, fill=subject)) + 
  geom_bar(stat='identity') +
  theme_bw() +
  # x축 순서 변경
  scale_x_discrete(limits = c(data.10.names)) + 
  # gganimate specific bits:
  transition_states(
    month,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out')

# Save at gif:

library(gifski)
library(png)


animate(myPlot10.sort, duration = 5, fps = 20, width = 2000, height = 400, renderer = gifski_renderer())
anim_save("data.10.sort.gif")
data.10.sort
# =======================================================================================================

# 연도별 평균
df <- read.csv("./Project_Visual/result.csv")
df <- df[df$항만명 == "부산", ]


# libraries:
library(ggplot2)
library(gganimate)

# df.test.2017 <- df[df$조회년도==2017&df$입출항구분=="입항"&df$Measures=="총계",c(2,7:37)]
# df.test.2018 <- df[df$조회년도==2018&df$입출항구분=="입항"&df$Measures=="총계",c(2,7:37)]
# df.test.2019 <- df[df$조회년도==2019&df$입출항구분=="입항"&df$Measures=="총계",c(2,7:37)]
# df.test.2020 <- df[df$조회년도==2020&df$입출항구분=="입항"&df$Measures=="총계",c(2,7:37)]
# df.test.2021 <- df[df$조회년도==2021&df$입출항구분=="입항"&df$Measures=="총계",c(2,7:37)]

df.test <- df[df$입출항구분=="입항"&df$Measures=="총계",c(1,7:37)]
df.test

# Make 2 basic states and concatenate them:

df.test[,c(2:32)][1]
length(df.test[,c(2:32)])

rep(c(names(df.test)[2:32]))
df.test[,-1]
df.values <- unlist(t(df.test[,-1]))

str(df.values)
length(df.values)
c(df.values)


data <- data.frame(subject = rep(c(names(df.test)[2:32]),times=60), values=c(df.values),year=rep(c(2017:2021), each=372), month=rep(c(1:12), times=5 ,each=31))


# 상위 10개 품목 이름
data.name <- names(head(sort(tapply(data$values, data$subject,mean),decreasing = T),10))

# 상위 10개 품목 테이터 프레임
data10 <- data[data$subject %in% data.name,]


data10[data10$year==2017,]
data10

tapply(data10[data10$year==2017,]$values, data10[data10$year==2017,]$subject, mean)
 
a <- data.frame(tapply(data10[data10$year==2017,]$values, data10[data10$year==2017,]$subject, mean))
a$tapply.data10.data10.year....2017....values..data10.data10.year....
names(a) <- "ave"
a$year <- c(2017)
a
for (i in (2018:2021)) {
  b <- data.frame(tapply(data10[data10$year==i,]$values, data10[data10$year==i,]$subject, mean))
  names(b) <- "ave"
  b$year <- c(i)
  a <- rbind(a,b)
}
c(row.names(a))
length(a$ave)
length(c(row.names(a)))
a$subject <- rep(a$subject[1:10],5)
a
# Make a ggplot, but add frame=year: one image per year
myPlot <- ggplot(a, aes(x=subject, y=ave, fill=subject)) + 
  geom_bar(stat='identity') +
  theme_bw() +

  # x축 순서 변경
  scale_x_discrete(limits = c(data.name)) +
  # gganimate specific bits:
  transition_states(
    year,
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

animate(myPlot, duration = 5, fps = 20, width = 2000, height = 400, renderer = gifski_renderer())
anim_save("5year10.gif")

a
