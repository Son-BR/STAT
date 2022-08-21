3+4
print("Hello, R!")

x=3

x <- 3
5 -> y

z<-x+y
print(z)
z

getwd()
plot(iris)

?iris

View(iris)

library(cowsay)
library(ggplot2)
# 패키지 설치 install.packages('ggplot2')

say('Hi!')
say('Hi!',by='chicken')

# str : structure
str(iris)

head(iris)

tail(iris)

# DataSet$Column: 칼럼만 추출
# class:자료형 확인(factor:범주형, numeric:수치형)

# 범주형
class(iris$Species)

levels(iris$Species)

table(iris$Species)

barplot(table(iris$Species))

# 수치형
class(iris$Petal.Length)

mean(iris$Petal.Length)

# 분산
var(iris$Petal.Length)
# 표준편차
sd(iris$Petal.Length)

hist(iris$Petal.Length)

# mtcars
plot(mtcars$mpg,mtcars$wt)


# cars
str(cars)
?cars

plot(cars$speed,cars$dist,
     col="tomato",
     pch=19)



mean(iris$Petal.Length)
var(iris$Petal.Length)
sd(iris$Petal.Length)
hist(iris$Petal.Length,col="tomato",
     breaks=5)


v.1<-10

7%%3
7%/%3
7/3

2^10
2**10

v<-1:100
class(v)
v

sum(1:100)

# if

score <-88

if (score>=90){
  grade = 'A'
} else if (score >= 80) {
  grade<- 'B'
} else {
  grade<-'F'
}
grade


# 피자나라치킨공주
n<-13
order<-'다이어트'
if (n%%15==0) {
  order<-'피자나라치킨공주'
} else if (n%%5==0) {
  order<-'치킨'
} else if (n%%3==0) {
  order<-'피자'
}
order

v<- c(10,20,30,40,50,60,70)

v[1]
v[7]

v[1:3]
v[3:6]

v[c(1,3,4,7)]
v[-1]
v[-c(1,3,4,7)]

v[8]
v[6:8]

# 에러
v[-1:3]

v[-(1:3)]

v[7]<-700
v

v[1:3]<-c(100,200,300)
v

v[1:3]<-seq(100,300,100)
v

v<-c(10,20,30,40,50,60,70)

v[c(T,T,F,F,F,T,F)]

v+1

c(10,20)+c(30,40)

c(10,20)+30

v>30

v[v>30]

# 1:2를 반복함
1:9 + 1:2

rep(1:2,times=5)

v

# : (T,F,T,F,T,F,T,F,...)
v[c(T,F)]

# 1에서 100까지의 수 중에서 7의 배수의 합
seq(7,100,7)
sum(seq(7,100,7))

v<-1:100
v[v%%7==0]
sum(v[v%%7==0])

v<-c()
for (i in 1:10) {
  v<-c(v,i)
}
v

v<-c()
for (i in 1:10) {
  v[i]<-i
}
v

v<-c(10,20,30)

v[7]<-70
v


iris$Sepal.Length
iris$Species


# lst(): 범주형 자료로 변환
f<-lst(c("Male","Female","Male","Female"))
levels(f)

f[f=='Male']

f[6]<-"Male"
f

# 생성할때 범주에 없는 값이라서  Na값 들어감
f[7]<-"TG"
f

f<-factor(c(1,2,1,2),
          levels=1:3,
          labels=c("Male","Female","TG"))

levels(f)

f[f=='Male']

f[6]<-"Male"
f

f[7]<-"TG"
f

v.1<-c(1,2,3)
v.2<-c("F","F","M")
c(v.1,v.2)

lst<-list(id=v.1,gender=v.2)
lst

lst$id
lst$gender

v<-c(10,20,30,40,50)

# 인덱스값을 리턴
which(v>30)

# 값을 리턴
v[which(v>30)]

n<-32

# n의 약수를 모두 출력하시오

num<-1:n
num[n%%num==0]

(1:n)[n%%1:n==0]

length((1:n)[n%%1:n==0])
sum(n%%num==0)


View(iris)
str(iris)
iris[1,]

iris[1:5,1]
iris[1:5,1:2]

iris[1:5,1:4]
iris[1:5,-5]

iris$Sepal.Length
iris[,1]
iris[iris$Sepal.Length<5,-5]

nrow(iris[iris$Sepal.Length<5,-5])


# Petal.Length가 평균보다 큰 데이터의 Petal.Width평균값은 얼마인가?

mean(iris[iris$Petal.Length>mean(iris$Petal.Length),4])

mean(iris[iris$Petal.Length>mean(iris$Petal.Length),"Petal.Width"])



fun <- function(x) {
  return (x+y+5)
}
y<-3
fun(5)
 

my.fun<-function(x,y,z=0) {
  return(x+2*y+3*z)
}
my.fun(1,2,3)
my.fun(z=3,y=2,x=1)
my.fun(1,z=3,y=2)

my.fun(1,2)


hist(iris$Sepal.Length,
     main="main",
     xlab="xlab",
     ylab="ylab")

# n의 약수 구해주는 함수

divisor <- function (n) {
  x<-(1:n)[n%%1:n==0]
  return (x)
}

divisor(32)

divisor(n=32)


# 약수의 개수/return 안적어도..
divisor <- function (n) {
  length((1:n)[n%%1:n==0])
}

divisor(32)

# 한줄로 적으면  {} 생략가능
divisor <- function (n) length((1:n)[n%%1:n==0])

divisor(32)

sapply(1:15,divisor)

# 소수 
(1:100)[sapply(1:100,divisor)==2]






