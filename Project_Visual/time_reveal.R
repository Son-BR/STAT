library(tidyverse)
library(tsibble)
library(colorspace)
library(gganimate)
library(ggplot2)

tsbl <- as_tsibble(EuStockMarkets)
tsbl_esm <- tsbl %>%
  group_by_key() %>%
  index_by(year = ~year(.))
head(tsbl_esm)



ggesm <- 
  ggplot(tsbl_esm, aes(x=index, y=value, group=key, color=key))+
  geom_line()+
  geom_point(aes(group = seq_along(index))) +
  geom_point()+
  scale_color_discrete_sequential('Sunset')+
  theme_minimal()+
  theme(legend.position='bottom')

str(tsbl_esm)
ggesm + transition_time(index)


## 엑셀파일로 저장
library(openxlsx)
write.xlsx(tsbl_esm,"tsbl.test.xlsx")


anim <- ggplot(tsbl_esm, aes(index, value, group = key)) +
  geom_line() +
  transition_time(index)
  
anim

# transition_reveal()

library(gifski)
library(png)

animate(anim, duration = 5, fps = 20, width = 2000, height = 400, renderer = gifski_renderer())
anim_save("anim.test.gif")


# ==================================================================================

df <- read.csv("./Project_Visual/result.csv")
df <- df[df$항만명 == "부산", ]

df.ex <- df[df$입출항구분 %in% c("입항", "출항") & df$Measures == "총계", c(1:6)]
df.ex <- df.ex[,-c(3,5)]

df.ex

df.in <- df.ex[df.ex$입출항구분=="입항",]
df.out <- df.ex[df.ex$입출항구분=="출항",]
#df.ex<-rbind(df.in,df.out)
df.ex

# 연월 합쳐서 새컬럼
df.ex$time <- c(paste(df.ex$조회년도, df.ex$조회월, sep = "-"))
df.ex <- df.ex[,-c(1,2)]
df.ex$time <- c(paste(df.ex$time, c("01"), sep = "-"))
df.ex
df.ex$time <- as.Date(df.ex$time)
row.names(df.ex)<-NULL
df.ex

ggplot(df.ex, aes(x=time, y=합계, group=입출항구분, color=key))+
  geom_line()+
  geom_point(aes(group = seq_along(time))) +
  geom_point()+
  scale_color_discrete_sequential('Sunset')+
  theme_minimal()+
  theme(legend.position='bottom')


ggesm

ggesm + transition_time(df.ex$time)

