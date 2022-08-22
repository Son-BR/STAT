# ====================================

# 기본

# Data
set.seed(1)
exdf <- data.frame(x = c(rnorm(300, -3, 1.5),
                       rnorm(300, 0, 1)),
                 group = c(rep("A", 300),
                           rep("B", 300)))

library(ggplot2)
                 
ggplot(exdf, aes(x = x, fill = group)) +
  geom_density(alpha = 0.5)

exdf

# ====================================================

library(ggplot2)
library(gganimate)


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
# df.ex$time <- c(paste(df.ex$조회년도, df.ex$조회월, sep = "."))
# df.ex <- df.ex[,-c(1,2)]
# df.ex

library(viridis)
library(hrbrthemes)

# 연도별 평균 입항/출항 따로
c(tapply(df.in$합계,df.in$조회년도,mean))
c(tapply(df.out$합계,df.out$조회년도,mean))
newdf <- data.frame(year = rep(2017:2021,time=2),
                    values = c(tapply(df.in$합계,df.in$조회년도,mean),
                               tapply(df.out$합계,df.out$조회년도,mean)),
                    inout = rep(c("입항","출항"), each=5))
newdf

ggplot(newdf, aes(x=year, y=values, group=inout, color=inout)) +
    geom_line() +
    geom_point() +
    scale_color_viridis(discrete = TRUE)
    # ggtitle("Popularity of American names in the previous 30 years") +
    # theme_ipsum() +
    # ylab("Number of babies born")

