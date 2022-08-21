# 두 집단의 차이 검정


# t검정 : 두집단간 비교 용이(세집단이상 불가능)

# 10명의 사람 약물복용 전후 수면시간 증감 관찰
str(sleep)
sleep

# 롱포맷, 와이드포맷 변경
library(tidyr)
# long -> wide
wide.df<-spread(sleep,key=group,value=extra)
summary(wide.df)

tapply(sleep$extra,
       INDEX=list(sleep$group),
       FUN=mean)

t.test(extra ~ group, data=sleep,paird=T)
t.test(wide.df$`1`,wide.df$`2`,paird=T)


# x^2(카이제곱)분포와 검정
# 교차표를 통해 범주형 두 변수의 조합별 빈도 파악

# 카이스퀘어분포를 따르는 랜덤 밸류
v<-rchisq(n=10000,df=1)
hist(v,col='orange')

x<-seq(0,15,length=200)

curve(dchisq(x,df=1),0,15,
      col='red',lwd=2,lty=1)
curve(dchisq(x,df=5),0,15,add=T,
      col='tomato',lwd=2,lty=1)
curve(dchisq(x,df=10),0,15,add=T,
      col='steelblue',lwd=2,lty=1)

# 넓이(신뢰구간)이 0.95가 되는 q값
qchisq(p=0.95,df=1)

# 값이 q일때 신뢰구간의 넓이 p
pchisq(q=2.5,df=1)

pchisq(q=3.85,df=1)

# 우연히 q값이 될 확률(유의수준): 1-pchisq(q=5,df=1)
pchisq(q=5,df=1,lower.tail=F)


# 매트릭스생성: nrow=행개수, ncol=열개수
mt<-matrix(c(1443,151,47,1781,312,135),nrow=3)
mt
df<-data.frame(mt)
df

colnames(df)<-c("With",'Without')
rownames(df)<-c('경상','중상','사망')

df

oij<-c(1443,1781,151,312,47,135)
eij<-c(1367,1855.9,196.9,267.4,77.1,104.7)

# 카이스퀘어값
cs.value<-sum((oij-eij)^2/eij)
cs.value


# 타이타닉
Titanic
class(Titanic)

# margin=1 행별데이터총합계, margin=2 열별데이터총합계 
tb<-margin.table(Titanic,margin=c(4,1))
tb

chisq.test(tb)



# F분포와 분산분석
# F 검정: F test
# 집단 간의 평균의 차이를 검정 할 때 사용

# F분포 랜덤 밸류
# df1,df2: 표본 1,2의 자유도
v<- rf(n=10000, df1=1, df2 = 30)
hist(v,col='steelblue')

x<- seq(0,15,length=200)
curve(df(x,df1=1,df2=30),0,15,
      col='tomato', lwd=2,lty=1)
curve(df(x,df1=5,df2=50),0,15,add=T,
      col='blue', lwd=2,lty=1)
curve(df(x,df1=10,df2=80),0,15,add=T,
      col='magenta', lwd=2,lty=2)

qf(p=0.95,df=1,df2=30)
pf(q=4.171,df=1,df2=30)
# 유의수준
pf(q=4.171,df=1,df2=30,lower.tail = F)


# 일원분산분석과 이원분산분석

df<-InsectSprays
str(df)
table(df$spray)

round(tapply(df$count,INDEX=list(df$spray),
       FUN=mean),3)
# 종속변수:count(살아남은벌레수)
boxplot(count~spray,data=df,
        col=2:7)

#분산분석
aov.result<-aov(count~spray,data=df)
summary(aov.result)

# 분산분석차이를 기준으로 사후분석
# 두 변수 유의한 차이있는지 검증
TukeyHSD(aov.result)


# 집단별 평균도표(mean plot)
library(gplots)
plotmeans(count~spray,data=df,
          col='tomato',
          barcol='orange',
          lwd=3,
          barwidth = 3)

model.tables(aov.result,type='mean')

model.tables(aov.result,type='effect')

# las : x축, y축 레이블 눕힐지, 세울지... 1,2,3,4...
plot(TukeyHSD(aov.result),col='blue',
     las=1)



# 정규성 검증: shapiro.test()
# H0: 정규분포 따를것이다
# shapiro.test() 유의확률(p-value) 낮으면 정규분포 따르지 않음

library(car)
qqPlot(df$count,pch=19, col='orange')
shapiro.test(df$count)

# 등분산 검증: leveneTest(),bartlett.test()
# H0: 등분산따를 것이다
# leveneTest() Pr 낮으면 등분산 따르지 않음
# bartlett.test() 유의확률(p-value) 낮으면 등분산 따르지 않음
leveneTest(count~spray,data=df)

bartlett.test(count~spray,data=df)

# 분산자체가 다른 집단(등분산 가정 충족x 집단)을 어떻게 비교???
# 일원분산분석
oneway.test(count~spray,data=df)



# 이원분산분석
# 집단을 구분하는 독립변수가 두 개인 경우에 모집단 간 평균의 동일성 검정

df<-ToothGrowth
str(df)
# supp : oj:오렌지쥬스, vc:비타민씨

unique(df$dose)

# 투여량 범주화(팩터)
df$dose<-factor(df$dose,
                levels=c(0.5,1.0,2.0),
                labels=c('L','M','H'))
str(df)


tapply(df$len,
       INDEX = list(SUPP=df$supp,DOSE=df$dose),
       FUN=mean)

with(df,tapply(len, list(SUPP=supp,DOSE=dose),mean))


# Y(종속변수) ~ X(독립변수)
# 주 효과(독립변수의 각 효과) => Y ~ X1+X2
# 주 효과, 상호작용 효과(독립변수 상호작용 효과) => Y ~ X1+X2+X1:X2, Y ~ X1*X2

boxplot(len~supp*dose,data=df,
        col=c('orange','tomato'))

aov.result<-aov(len~supp*dose,data=df)

# Pr(P-Value)
summary(aov.result)

TukeyHSD(aov.result)
plot(TukeyHSD(aov.result),las=1)



# 상관관계와 상관분석
library(MASS)
cor(cats$Bwt,cats$Hwt)
plot(cats$Bwt,cats$Hwt,
     pch=19, col='tomato')

# 상관계수의 종류

# 피어슨:기본적
cor(cats$Bwt,cats$Hwt,method='pearson')
# 스피어만: 순위로, 이상치 많을때(이상치에 덜민감)
cor(cats$Bwt,cats$Hwt,method='spearman')
# 켄달: 순위로, 샘플 적거나 동률이 많을때
cor(cats$Bwt,cats$Hwt,method='kendall')



# 선형회귀

library(HistData)
df<-GaltonFamilies
str(df)

# Y(childHeight)~X(midparentHeight)
cor(df$midparentHeight,df$childHeight)

# col=adjustcolor('색',alpha=투명도)
# jitter(): 노이즈발생(점의 정렬이 흔들리게)
plot(jitter(childHeight)~jitter(midparentHeight), data=df,
     col=adjustcolor('steelblue',alpha=0.5),
     pch=19)

# lm(formula = childHeight ~ midparentHeight, data = df)
# linear regression model(선형회귀모델)
model<-lm(childHeight~midparentHeight,data=df)
# Coefficients(계수)에 기울기(0.6374), 절편(22.6362)의 정보 : (선형)회귀계수

# abline(): 직선그리는 함수
# 직선 그리기: 선형회귀식 y=0.64x+22.64
abline(model,col='tomato',lwd=3)


# 선형회귀식 연습
x<-runif(n=100,min=0,max=100)
y<- 3*x+5+rnorm(100,0,20)
plot(x,y,pch=19,col='skyblue')

cor(x,y)
model<-lm(y~x)
abline(model,col='tomato',lwd=2)

# Residuals(잔차)-> 잔차에대한 정보
# Coefficients(계수)
# F-statistic: p-value 회귀식 전체의 유의성
summary(model)


# abline a:기울기 b:절편
abline(a=1,b=5,col='red',lwd=1,lty=2)































