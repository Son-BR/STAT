# 02장 내장 데이터셋

# 연습문제 2.1
barplot(table(iris$Species),
        col='tomato',
        main='품종의 막대그래프',
        xlab='품종',
        ylab='개수',
        )

# 연습문제 2.2
mean(iris$Petal.Width)
var(iris$Petal.Width)
sd(iris$Petal.Width)

hist(iris$Petal.Width,
     col='tomato',
     main='꽃잎의 너비에 대한 히스토그램',
     xlab='꽃잎의 너비',
     ylab='빈도수',
     )

# 연습문제 2.3
hist(mtcars$hp,
     xlim=c(0,400),
     ylim=c(0,12)
       )

plot(mtcars$hp,mtcars$mpg,
     col='tomato',
     pch=10
     )

# 연습문제 2.4
str(cars)
summary(cars)     
plot(cars$speed,cars$dist,
     col='blue',
     pch=15,
     xlim=c(0,30),
     ylim=c(0,125)
)


# 연습문제 3.1

# 정사각형 넓이
n<-c(5,10,10)
area<-n*n
area

# 함수화 
area<-function(v) v^2
area(n)
sapply(n,area)

# 원의 둘레와 넓이
r<-c(5,10,15)     
round<-r*2*pi
area<-r*r*pi
round
area

#함수화
round<- function(r) r*2*pi
area<- function(r) r*r*pi

round(r)
area(r)

sapply(r,round)
sapply(r,area)



# 연습문제 3.2
n<-15
order<-'다이어트'
if (n%%15==0) {
  order<-'피자나라치킨공주'
} else if (n%%5==0) {
  order<-'치킨'
} else if (n%%3==0) {
  order<-'피자'
}
order

v<-1:15
order<-ifelse(v%%15==0, '피자나라치킨공주',
              ifelse(v%%5==0, '치킨',
                     ifelse(v%%3==0,'피자',
                            '다이어트')))
order

# 함수화
order<- function (v) ifelse(v%%15==0, '피자나라치킨공주',
                            ifelse(v%%5==0, '치킨',
                                   ifelse(v%%3==0,'피자', '다이어트')))

order(v)


# 연습문제 3.3

# 1에서 n까지의 세제곱의 합
n<-10
sum((1:n)^3)

cumsum<-function(x) sum((1:x)^3)
n<-c(10,15,20)
sapply(n,cumsum)

cumsum.1<- function(x) ((x*(x+1)/2)^2)
sapply(n,cumsum.1)


n<-20
num<-1
for (i in 1:n) {
  num<-num*i
  #print(num)
}
num

# 연습문제 3.4
n<-15
for (i in 1:n) {
  order<-'다이어트'
  if (i%%15==0) {
    order<-'피자나라치킨공주'
  } else if (i%%5==0) {
    order<-'치킨'
  } else if (i%%3==0) {
    order<-'피자'
  }
  cat(i, order,'\n')
}

# 연습문제 3.5
n<-5

for (i in 1:n) {
  for (j in 1:n) {
    cat('*')
  }
  cat('\n')
}

for (i in 1:n) {
  for (j in 1:i) {
    cat('*')
  }
  cat('\n')
}

for (i in 1:n) {
  if (i%%2==1) {
    for (j in 1:n) {
      cat('*')
    }
    cat('\n')
  }
  else {
    cat('*','\n')
    }
}

# 연습문제 3.6
n<-100

#약수 카운트 함수
div.cnt <- function (n) length((1:n)[n%%1:n==0])
#약수2개(소수)
(1:n)[sapply(1:n,div.cnt)==2]
# 소수개수
length((1:n)[sapply(1:n,div.cnt)==2])

# 연습문제 3.7
# 약수개수
div.cnt(100)
length((1:n)[n%%1:n==0])

maxval=0
val=0
for (i in 1:n) {
  cat(i,':',div.cnt(i),'\n')
  if (div.cnt(i)>=maxval) {
    maxval<-div.cnt(i)
    val<-i
  }
}
cat(val,':',maxval)

div.2<-function (n) {
  for (i in 1:sqrt(n)) {
    if (n%%i==0) {
      cat(i, n/i, '\n')
    }
  }
}
div.2(36)


# 연습문제 4.1

div<-sapply(1:15, div.cnt)
div

sum(div==2)

which(div==2)

sum(div)

# 연습문제 4.2
height<-c(163,175,182,178,161)
weight<-c(65,87,74,63,51)
blood<-factor(c("A","B","AB","O","A"))

lst<-list(height=height,weight=weight,blood=blood)
lst

mean(lst$height)
mean(lst$weight)
table(lst$blood)


# 연습문제 5.1
# 약수의 개수
div.cnt <- function (n) length((1:n)[n%%1:n==0])
sapply(1:15, div.cnt)

# 연습문제 5.2
# 소수의 개수
prime.cnt <- function (n) length((1:n)[sapply(1:n,div.cnt)==2])

prime.cnt(100)

#소수인지 판단
is.prime <- function (n) div.cnt(sqrt(n))==2
is.prime(97)

# 루트(sqrt)
is.prime2 <- function (n) div.cnt(sqrt(n))==2
#div.cnt2 <- function (n) length((1:sqrt(n))[n%%1:n==0])
is.prime2(97)
