# 데이터프레임복사(원본 보호) 
df<-iris

str(df)
class(df)

dim(df)  
nrow(df)
ncol(df)
head(df)

rownames(df)
colnames(df)

# 이름부여
v<-c(85,77,96)
v
names(v)
names(v)<-c('Kor','Eng','Math')
names(v)
v

v['Kor']
v[c('Eng','Math')]

df$Sepal.Length
df$Sepal.Width

df$Sepal.Sum<-df$Sepal.Length+df$Sepal.Width

df$Sepal.Sep<-ifelse(df$Sepal.Sum>mean(df$Sepal.Sum),'Big','Small')


str(df)

df$Sepal.Sep<-factor(df$Sepal.Sep)

str(df)

head(df)

table(df$Sepal.Sep)

barplot(table(df$Sepal.Sep))


?state.x77
class(state.x77)

is.data.frame(state.x77)

st<-as.data.frame(state.x77)

class(st)
str(st)
head(st)

st[st$Population==max(st$Population),]

st[st$Murder==max(st$Murder),]

row.names(st[st$Murder==max(st$Murder),])


df<-iris
df$Sepal.Sum<-df$Sepal.Length+df$Sepal.Width
write.csv(df,'my.iris.csv',row.names = F)
getwd()


df<- read.csv('my.iris.csv',header = 1)
str(df)


library(readxl)

df<- read_excel("성적표.xlsx",sheet=1)
str(df)
class(df)


df<-data.frame(df)
str

# MARGIN=1 : 행별평균, MARGIN=2 : 열별평균
df$평균<-round(apply(df[,3:5],MARGIN = 1,mean),2)
df

write.csv(df,"score.csv",row.names = F)


aq<- airquality
str(aq)
mean(aq$Ozone)
mean(aq$Ozone,na.rm = T)
is.na(aq$Ozone)
aq$Ozone[is.na(aq$Ozone)]
sum(is.na(aq$Ozone))

ozone<-aq$Ozone
ozone[is.na(ozone)]<-0
ozone

# 결측치 평균으로 대체
ozone<-aq$Ozone
ozone[is.na(ozone)]<-mean(ozone,na.rm=T)
ozone

# 평균은 같음
mean(aq$Ozone,na.rm = T)
mean(ozone)

# 표준편차는 훨씬 좁아짐
sd(aq$Ozone,na.rm = T)
sd(ozone)

# sample(v,n) :v에서 n개 랜덤 추출
# na.omit() : 결측치가 있는 행단위 제거 후 남은 데이터
ozone<-aq$Ozone
ozone[is.na(ozone)]<-sample(na.omit(aq$Ozone),37)
ozone
mean(ozone)
sd(ozone)

# 행단위, Na하나라도있으면 False
aq<-airquality
complete.cases(aq)
aq[!complete.cases(aq),]
aq<-aq[complete.cases(aq),]
aq


# VIM 패키지의 aggr()함수:변수별 결츠치의 분포와 발생패턴 시각화
library(VIM)
aggr(airquality)


# boxplot() 함수 : 데이터셋에 이상치가 존재하는 지를 시각화
st<-data.frame(state.x77)
boxplot(st$Income,
        col='tomato',
        pch=19,
        boder='red')

boxplot.stats(st$Income)$out
st[st$Income==boxplot.stats(st$Income)$out,]


# iris 품종별 boxplot 그리기
df<- iris
boxplot(df$Petal.Length,
        col='skyblue')

boxplot(Petal.Width ~ Species, data=iris,
        pch=19, col="steelblue")

min(df[df$Species=='setosa','Petal.Length'])
min(df[df$Species=='versicolor','Petal.Length'])

Lenth.out<-boxplot(Petal.Length ~ Species,data=iris)$out
df[df$Petal.Length==Lenth.out[1]|df$Petal.Length==Lenth.out[2],]


# 이상치 
outlier<- boxplot.stats(iris[iris$Species=='setosa',4])$out
outlier

iris[iris$Petal.Width %in% outlier,]


# subset() : 조건에맞는 데이터프레임만들기


st<-data.frmae(state.x77)
st[st$Population==max(st$Population),c(3,6)]


subset(st,
       subset=st$Population==max(st$Population),
       select=c(3,6))

set<- iris[iris$Species=='setosa',]
vrs<- iris[iris$Species=='versicolor',]
vrg<- iris[iris$Species=='viginica',]

levels(iris$Species)

# split() : 데이터 프레임을 범주형 변수를 기준으로 여러 개로 분할
sp<-split(iris,f=iris$Species)
length(sp)
names(sp)
class(sp)

sp$setosa
sp$versicolor
sp$virginica

dim(sp$setosa)
dim(sp$versicolor)

# rbind():행방향합치기, cbind():열방향합치기
df.2<-rbind(sp$setosa,sp$versicolor)
dim(df.2)

iris[,1:2]
iris[,3:4]

df.3<-cbind(iris[,1:2],iris[,3:4])
df.3

# merge():특정 변수의 값이 같은 행을 기준으로 여러개의 데이터 프레임 병합

library(readxl)
df.1<-read_excel('성적표.xlsx', sheet=1)
df.2<-read_excel('성적표.xlsx', sheet=2)

df.1
df.2

cbind(df.1,df.2)

merge(df.1,df.2)


merge(df.1,df.2,all=T)



df.1<-read_excel('성적표.xlsx', sheet=1)
df.2<-read_excel('성적표.xlsx', sheet=2)
# merge(df.1,df.2,all=T)
df<-merge(df.1,df.2,all=T,by.x=c('번호','이름'),by.y=c('No','Name'))

df$`Deep Learning`


colnames(df)<-c('no','name','python','r','ml','dl','cloud')
df


# aggregate() : 범주별로 통계량을 확인하고 싶을 때 주로 활용

df<-iris
aggregate(df[,-5],
          by=list(df$Species),
          FUN=mean)

aggregate(df[,-5],
          by=list(품종=df$Species),
          FUN=mean)

aggregate(df[,-5],
          by=list(품종=df$Species),
          FUN=sd)


library(MASS)

data('survey')
df<-survey
str(df)

df<-na.omit(df)
df<-df.[complete.cases(df),]

dim(df)

# breaks:구간
hist(df$Height,breaks=20)

hist(df[df$Sex=='Male',]$Height,breaks = 20)
hist(df[df$Sex=='Female',]$Height,breaks = 20)

mean(df[df$Sex=='Male',]$Height)
mean(df[df$Sex=='Female',]$Height)


aggregate(df[,c(10,12)],
          by=list(df$Sex),
          FUN=mean)

table(df$Sex)
t.test(Height~Sex,data=df)

boxplot(Height~Sex,data=df,
        col=c('orange','tomato'))


# sort(v):v의 값 정렬
# order(v):v를 정렬했을때 인덱스 번호
# decreasing=T : 내림차순

v<-c(30,50,20,40,10)
v
sort(v)
sort(v,decreasing=T)



df<-data.frame(state.x77)
str(df)

sort(df$Illiteracy,decreasing=T)

ord<-order(df$Illiteracy,decreasing = T)

df[ord[1:10],c(3,2)]

# Illiteracy 같으면 Income로 내림차순
ord<-order(df$Illiteracy,df$Income,decreasing = T)
df[ord[1:10],c(3,2)]


x<-sample(1:10,size=5)
sum(x==7)


# saple()
s<-0
for (i in 1:10000) {
  x<-sample(1:10,size=5)
  s<-s+sum(x==7)
}
s

# replace=T : 복원추출
sample(1:10,size=5,replace=T)

# iris에서 랜덤하게 50개 행 추출
idx<-sample(1:nrow(iris),size=50)
iris[idx,]

# seed설정하면 sample 똑같이나옴
set.seed(2022)
sample(1:10,size=5,replace=T)

# 환경변수 전체 삭제
# rm(list=ls())


# 펭귄
library(palmerpenguins)

# 데이터확인
data() #내장데이터
data(package='palmerpenguins')


data("penguins")
pg<-data.frame(penguins)
str(pg)

library(VIM)

# numbers= : 숫자표시여부, prop= : 비율로표시여부(비율,갯수)
aggr(pg,numbers=T,prop=F)

pg<-na.omit(pg)
aggr(pg)
dim(pg)

str(pg)
table(pg$species)
barplot(table(pg$species))

table(pg$island)
barplot(table(pg$island))

table(pg$sex)
barplot(table(pg$sex))

str(pg[,3:6])
summary(pg[,3:6])


# 그래프 나눠서 그리기(분할)
hist(pg$bill_length_mm)
hist(pg$bill_depth_mm)
hist(pg$flipper_length_mm)
hist(pg$body_mass_g)

par(mfrow=c(2,2))
hist(pg$bill_length_mm)
hist(pg$bill_depth_mm)
hist(pg$flipper_length_mm)
hist(pg$body_mass_g)
par(mfrow=c(1,1))

par(mfrow=c(1,4))
hist(pg$bill_length_mm)
hist(pg$bill_depth_mm)
hist(pg$flipper_length_mm)
hist(pg$body_mass_g)
par(mfrow=c(1,1))

# 색깔 넣기
par(mfrow=c(2,2))
hist(pg$bill_length_mm,col=1:5)
hist(pg$bill_depth_mm)
hist(pg$flipper_length_mm)
hist(pg$body_mass_g)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(pg$bill_length_mm,col=2:5)
hist(pg$bill_depth_mm)
hist(pg$flipper_length_mm)
hist(pg$body_mass_g)
par(mfrow=c(1,1))



plot(pg$bill_length_mm,pg$bill_depth_mm,
     pch=19, col='tomato')


# 피어슨 상관계수
cor(pg$bill_length_mm,pg$bill_depth_mm)


# 종별 산점도 색 다르게 넣기

my.color<- ifelse(pg$species=='Gentoo',"tomato",
                  ifelse(pg$species=='Adelie','skyblue', 'orange'))
plot(pg$bill_length_mm,pg$bill_depth_mm,
     pch=19, col=my.color)

cor(pg[pg$species=='Adelie',]$bill_length_mm,
    pg[pg$species=='Adelie',]$bill_depth_mm)











