
# 새창띄우기(새창에서 그래프)
windows(width=7,height=5)

# 사용가능한 색 확인
colors()

# 베르누이 시행 : 가능한 결과가 두 개 밖에 없고 , 성공의 확률이 정해져 있는 확률 시행


# n:관찰횟수(동전던진횟수) 
# size:시행횟수(한번에 던진 동전갯수)
# prob: 성공확률(동전앞면이나올확률)
v<-rbinom(n=100000,size=1000,prob=0.5)

# break:x축구간 n개로 나눔
hist(v,col='orange',breaks=20)


v.2<-rbinom(n=100000,size=1000,prob=0.4)
hist(v.2,col='orange',breaks=20)


# 난수생성

# 균일분포
# 0~100 사이 랜덤한 숫자 100개
v<-runif(n=10000,min=0,max=100)
hist(v,col='tomato')

# seed설정시 계속 동일한 결과
set.seed(2022)
v<-runif(n=10000,min=0,max=100)
hist(v,col='tomato')

mean(v)
# sd(): 표준편차
sd(v)

# 정규분포모양으로 난수 생성
# n:시행횟수 mean:평균 sd:표준편차
v<-rnorm(n=100000,mean=50, sd=20)
hist(v,col='violet',breaks=20)


# 정규분포 곡선

# 범위안에서 일정한 간격으로 length개
x<-seq(0,100,length=100)

#  dnorm:정규분포의 확률밀도함수 
y<-dnorm(x,mean=50,sd=20)

# lwd:선굵기
plot(x,y,
     type='l',
     col='tomato',
     lwd=3)


x<-seq(0,100,length=100)
# 균일분포함수??
y<-dunif(x,min=0,max=100)
plot(x,y,
     type='l',
     col='tomato',
     lwd=3)


x<-seq(140,200,length=100)
y<-dnorm(x,mean=170,sd=10)
plot(x,y,
     type='l',
     col='tomato',
     lwd=3)

# pnorm: 정규분포의 누적분포함수
# pnorm(q:확인할분위수, mean, sd)

# 평균30000, 표준편차10000일때 35000값의 누적확률(35000이하일 확률)
pnorm(35000,30000,10000)

pnorm(25000,30000,10000)

# $25,000 ~ $35,000 사이에 있을 확률
pnorm(35000,30000,10000)-pnorm(25000,30000,10000)

# 디폴트: pnorm(q, mean=0,sd=1)

pnorm(1)-pnorm(-1)


pnorm(2)-pnorm(-2)


pnorm(2.58)-pnorm(-2.58)

# 평균이 68점인 시험에서 87점을받았을때
pnorm(87,mean=68,sd=10)

# 상위 백분율
1-pnorm(87,mean=68,sd=10)

# 50명일때 등수
(1-pnorm(87,mean=68,sd=10))*50
pnorm(87,mean=68,sd=10,lower.tail=F)*50

# 수학70점 영어80점
# 단 , 수학 점수점수~𝑁(60,10^2), 영어 점수점수~𝑁(70,20^2)
pnorm(70,60,10,lower.tail=F)

pnorm(80,70,20,lower.tail=F)

# 베르누이
x<-rbinom(10000,size=100,prob=0.5)

# porb=T : y축 확률로 표시(디폴트:횟수)
hist(x,col='skyblue',breaks=20,prob=T)

# curve(곡선그릴함수, x축시작값,x축끝값)
# add:논리값 설정, TRUE일 경우 이전 그림에 겹쳐그림
# lty:선종류(점선?)
curve(dnorm(x,50,5),25,75,
      col='tomato',add=T, lwd=2, lty=2)


# 키
library(MASS)
height<-na.omit(survey$Height)
length(height)
hist(height,col='skyblue',breaks=20)

# 30개 랜덤 추출
samp<-height[sample(1:209,size=30)]
x.bar<-mean(samp)
x.sd<-sd(samp)


mean(height)
sd(height)

x.bar
x.sd

x.bar<-c()
for (i in 1:100000) {
  samp<-height[sample(1:209,size=30)]
  x.bar[i]<-mean(samp)
  x.sd[i]<-sd(samp)
}

hist(x.bar,col='skyblue',breaks=20)

# 제대로안나옴
# x<-seq(160,180,length=200)
# curve(dnorm(x,mean(height),sd(x.bar)),
#       160,180,
#       col='tomato',add=T, lwd=3, lty=2)



x.1<- rnorm(n=5000,mean=70,sd=5)
x.2<- rnorm(n=5000,mean=50,sd=5)
x<-c(x.1,x.2)
hist(x,col='skyblue',breaks=20)

# 에러
# x.bar<-c()
# for (i in 1:100000) {
#   samp<-x[sample(x,size=30)]
#   x.bar[i]<-mean(samp)
#   }
# 
# hist(x.bar,col='skyblue',breaks=20,prob=T)
# 
# x.samp<-seq(30,90,length=200)
# curve(dnorm(x.samp,mean(x),sd(x.samp),
#             30,90,col='tomato',add=T,lwd=3,lyt=2)
# 

# 상관계수의 통계적 유의성 
cor(iris[,-5])
cor.test(iris$Petal.Width,iris$Petal.Length)


# 이항분포와 가설검정
# 이항분포 B(100,0.5)에서 성공확률이 p=0.6이 나올 확률
binom.test(x=60,n=100,p=0.5)



# qnorm(), pnomr()
# N(mean,sd^2)에서 확률(넓이)이  p가되는 x값
qnorm(p=0.5,mean=50,sd=10)

qnorm(p=0.68,mean=50,sd=10)

qnorm(p=0.975,mean=50,sd=10)
qnorm(p=0.025,mean=50,sd=10)

qnorm(p=0.005,mean=50,sd=10)
qnorm(p=0.995,mean=50,sd=10)

# N(mean,sd^2)에서 0~ q값이 가지는 확률
# pnorm, qnorm역관계
pnorm(q=75.75829,mean=50,sd=10)


binom.test(x=65,n=100,p=0.5)

# 신뢰 수준 변경 
# conf.level = 0.99 -> 유의수준 0.01
binom.test(x=60,n=100,p=0.5,conf.level = 0.99)


# 정규성 검정
shapiro.test(survey$Height)
hist(survey$Height)

# p-value가 0.05보다 크면 정규성 따른다고 볼수있음
shapiro.test(survey$Age)
shapiro.test(iris$Petal.Length)
shapiro.test(mtcars$mpg)

# 시각화 일치하면 정규분포를 따른다고 볼 수 있다????
qqnorm(survey$Height,col='skyblue')
qqline(survey$Height,col='tomato',lwd=3)


qqnorm(survey$Age,col='skyblue')
qqline(survey$Age,col='tomato',lwd=3)


# t-분포와 평균검정

# t분포로 랜덤생성
# t분포따르는 데이터 10000개 자유도 29
v<-rt(n=10000,df=29)
hist(v,col='skyblue',prob=T)

x<-seq(-4,4,length=200)

# dt:t분포 확률밀도함수
curve(dt(x,df=29),-4,4,add=T,
      col='tomato', lwd=3,lty=2)

curve(dt(x,df=29),-4,4,add=T,
      col='violet', lwd=3,lty=2)


pt(q=2.04523,df=29)
pt(q=2.756386,df=29)

qt(p=0.975,df=29)
qt(p=0.995,df=29)


# 일표본 평균검정(MASS패키지의 cats)

str(cats)
table(cats$Sex)
# 독립변수 범주형(성별), 종속변수 수치형(무게) -> t에 적합
t.test(Bwt~Sex,data=cats)

# 신뢰구간 0.99 (유의수준 0.01)
t.test(Bwt~Sex,data=cats,conf.level=0.99)


mean(cats$Bwt)
tapply(cats$Bwt,INDEX=list(sex=cats$Sex),mean)
# 성별에 따라 무게다름 채택



# 한개의 집단 사전,사후의 차이(ex:신약테스트)
str(sleep)
t.test(extra ~ group, data=sleep, paired=T)
s





getwd()

setwd('C:/R_Project/MyProject/EXAM_STAT')













