# 02장 내장 데이터셋

# 연습문제 6.1
st<-state.x77
st<-data.frame(st)
dim(st)
mean(st$Income)
st[st$Population>10000,c(1,2)]
st[rownames(st)=='Florida',c(1,2)]
st[st$Population<10000&st$Income<4436,]

mean(st[st$Income<5000,]$Illiteracy)
mean(st[st$Income>5000,]$Illiteracy)

# 연습문제 6.2
st[st$Population<1000&st$Income<5000,]
mean(st[st$Income<5000,]$Illiteracy)
mean(st[st$Income>5000,]$Illiteracy)

# 연습문제 6.3
library(readxl)
df<- read_excel("scores.xlsx",sheet=1)
df
df$Sum <- df$Kor + df$Eng + df$Math
df

df$Mean<-apply(df[,3:5],MARGIN = 1,mean)
df

write.csv(df,"scores.csv",row.names = F)


# 연습문제 7.1
library(mice)
df<-nhanes
str(df)

na.omit(df)
# df[complete.cases(df),]
df[!complete.cases(df),]


nrow(df[!complete.cases(df),])

is.na(df)
sum(is.na(df$bmi))
sum(is.na(df$hyp))
sum(is.na(df$chl))

library(VIM)
aggr(df)

# 연습문제 7.2

