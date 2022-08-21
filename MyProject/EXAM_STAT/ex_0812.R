# 회귀분석의 유형


# 단순 선형회귀
library(car)
data(Prestige)
df <- Prestige
str(df)

table(df$type)
barplot(table(df$type), col = "orange")


hist(df$income, col = "tomato", breaks = 20)

# 정규분포 따르는지 검사
shapiro.test(df$income)

hist(df$education, col="tomato", breaks = 20)
hist(df$women, col="tomato", breaks = 20)
hist(df$prestige, col="tomato", breaks = 20)

shapiro.test(df$prestige)


plot(df[, -(5:6)], pch = 19, col = "skyblue")

# 선형회귀분석석
lm(income~education,data=df)

cor(df[,-(5:6)])

# education
model<-lm(income~education, data=df)
summary(model)

plot(income~education,data=df,
     col='skyblue',pch=19)
abline(model,col='tomato',lwd=3)

###
summary(resid(model))
confint(model)
anova(model)

# women
model<-lm(income~women, data=df)
summary(model)

plot(income~women,data=df,
     col='skyblue',pch=19)
abline(model,col='tomato',lwd=3)

# prestige
model<-lm(income~prestige, data=df)
summary(model)

plot(income~prestige,data=df,
     col='skyblue',pch=19)
abline(model,col='tomato',lwd=3)



# 다중선형회귀(multiple linear regression)
# 종속변수에 영향을 미치는 독립변수가 여러개
# 다중회귀식: y=b0+b1x1+b2x2+....+bnxn

# income~education+women+prestige
formula=income~education+women+prestige
model<-lm(formula,data=df)
summary(model)

formula=income~education+women
model<-lm(formula,data=df)
summary(model)

formula=income~education+prestige
model<-lm(formula,data=df)
summary(model)

formula=income~women+prestige
model<-lm(formula,data=df)
summary(model)


# stargazer: 논문에 들어가는 표형태로 제작
library(stargazer)
# 타입지정 안하면 수식(마크다운 html느낌)으로 반환
stargazer(model,type="text")

par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1,1))


# 다항 선형회귀
model<-lm(income~education,data=df)
plot(income~education,data=df,col='skyblue',pch=19)
summary(model)
abline(model)


library(tidyverse)
model<-lm(income~education+I(education^2),data=df)
plot(income~education,data=df,col='skyblue',pch=19)
with(df,
     lines(arrange(data.frame(education,
                              fitted(model)),
                   education),
           lty=1,lwd=3,col='tomato'))
summary(model)


# 회귀모델의 설명력
df<-mtcars
str(df)
df<-mtcars[,1:6]

plot(df,col='green',pch=19)

cor(df)

library(corrgram)
corrgram(df)


lm(mpg~.,data=df)
model<-lm(mpg~.,data=df)
summary(model)

# 변수 줄이면 p-value 줄어듬(유의성 올라감)
model<-lm(mpg~hp+wt,data=df)
summary(model)


# 후진선택법 회귀모델 구축
# 모든 독립변수가 포함된 모델에서 시작해서 단계별로 독립변수를 제거
model<-lm(mpg~disp+drat+hp+wt,data=df)
step(model,direction='backward')

mod.selected<-step(model,direction='backward')
summary(mod.selected)


# 연습문제
# 다중선형회귀의 변수 선택을 통해 최적의 독립변수 조합 찾기

###### 1. 전진선택법
# df<-read.csv('House Price.csv')
# model<-lm(SalePrice~1,data=df)
# mod.selected<-step(model,
#                    direction='forward',
#                    scope = list(lower=~1,
#                                 upper= 변수들일일이))

# 2. 후진선택법
df<-read.csv('House Price.csv')
str(df)
dim(df)

# 수치형인지 벡터생성[T,F]
is.num<-c()
for (i in 1:80) {
    is.num[i]<-is.numeric(df[,i])
}
is.num

# 수치형이 아닌 컬럼 제외
df<-df[,is.num]
# ID 컬럼 제외
df<-df[,-1]

dim(df)

# 결측치 행 제거
df<-df[complete.cases(df),]
str(df)


model<-lm(SalePrice~.,data=df)
summary(model)

mod.selected<- step(model,direction='backward')
summary(mod.selected)


# 더미변수를 이용한 회귀분석
# 회귀분석을위한 변수가 연속형 변수가 아닐때
# -> 더미변수로 변환하여 회귀본석

df<-InsectSprays
str(df)

# spray는 범주형이기때문에 자동으로 더미변수로 변환하여 회귀분석
# 'A'를 기준으로 함
lm(count~spray,data=df)
model<-lm(count~spray,data=df)
summary(model)

# 더미변수 확인
contrasts(df$spray)

# mtcars 더미변수
df<-mtcars[,1:6]
str(df)
df$cyl<-factor(df$cyl)
head(df)
table(df$cyl)

# cyl4 -> 기준변수
lm(mpg~.,data=df)

model<-lm(mpg~.,data=df)
summary(model)



# 선형 모델의 일반화

# 종속변수가 범주형일때 
df<-split(iris,f=iris$Species)
df<-rbind(df$setosa,df$versicolor)
plot(df[,c(1,5)])



# glm(): 일반화 선형모델 함수 
# lm()과 유사하나 추가로 family= 파라미터 지정
# family= : gaussian(정규분포), binomial(이항분포), poisson(포아송분포),
#           inverse.gaussian(역정규분포), gamma(감마분포),quasi(유사가능도모형:응답분포 확실x)

library(robust)

data(breslow.dat)
df<-breslow.dat

df<-df[,c('Base','Age','Trt','sumY')]
str(df)
dim(df)

model<-glm(sumY~., data=df,family=poisson)
summary(model)

# exp(x): e^x 
exp(coef(model))



# 이항 로지스틱 회귀분석
df<-split(iris,f=iris$Species)
df<-rbind(df$setosa,df$versicolor)
plot(df[,c(3,5)])

model<-glm(Species~Petal.Length,data=df,
    family=binomial(link='logit'))

# 로지스틱 회귀분석은 분산이 의미가 없음(p-value도,,)
# 분류가 중요
summary(model)

# 분류(classification):계산된 예측확률로 (주로 이진)분류 


# 혼동 행렬(Confusion Matrix)
# 이진분류기의 분류결과를 2x2 행렬로 표시한 행렬
# 이진 분류기가 분류(예측)할때 얼마나 많이 헷갈렸는가를 나타냄
# TP TN FP FN
# FN(1종오류), FP(2종오류) : 분류 종류에 따라 심각도 달라짐





















