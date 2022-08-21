# csv 파일 읽어오기
df <- read.csv('House_Prices.csv', na.strings = c("", " ", NA))

# 데이터의 기본적인 정보 확인
dim(df)

summary(df)

df_0 <- df[, c('LotArea','MasVnrArea','BsmtFinSF1','GarageArea','BsmtUnfSF','TotalBsmtSF','GarageArea','SalePrice')]
plot(df_0)
# **81**개의 컬럼이라 변수들 중에서 유의미하다고 생각하는 변수를 뽑아서 사용할 필요가 있다.

# 변수 확인
names(df)
summary(df)

# 가장 중요하다고 볼 수 있는 SalePrice(판매가)를 먼저 분석하자.

# 1. SalePrice 분석
summary(df$SalePrice)

# x
x <- seq(-4, 4, length.out = 101)
# normal
yn <- dnorm(x, mean = 0, sd = 1)
# t
yt003 <- dt(x, 3)
yt030 <- dt(x, 30)

# 도수분포표로 시각화
hist(round(df$SalePrice / 1000, 1),
     col = 'yellow',
     main = '판매가의 도수분포표',
     xlab = "가격  (Unit : 1000$)",
     ylab = "빈도수"
)
par(new =TRUE)
plot(x, yt003, lty = 5, col = 'blue', xlab ='', ylab = '',axes=FALSE,add = TRUE)

str(boxplot.stats(round(df$SalePrice / 1000, 1)))

# 너무 값이 크니까 1000을 나누어 표시 ## 팀원이 도와줌!

# 
#install.packages("moments")
library(moments)
str(df$SalePrice)

length(boxplot(df$SalePrice)$out)
sum(is.na(df$SalePrice))

skewness(df$SalePrice)
kurtosis(df$SalePrice)
# 정규분포의 왜도 첨도 기준은 아니니
# 데이터의 모델이 푸아성 분포 좋은 모델링이라고 할 수는 없다.
# 단위 시간 안에 어떤 사건이 몇 번 발생할 것인지를 표현
# ex) 일정 주어진 생산시간 동안 발생하는 불량 수

# 기본정보를 시각화
boxplot(round(df$SalePrice / 1000, 1),
        main = "판매가의 기본정보",
        col = 'yellow')

# 자료의 특징
#  1. 이상치가 많다.
#  2. 한쪽으로 치우쳐져 있다(왜도가 크다)
#  3. 특정값에 몰려있다.(첨도가 높다)

# 2. 다른 변수와 상관관계비교
# 2.1 양적변수
# 2.1.1 grlivarea(거실 면적 평방피트 0.03평) 변수와 가격 간의 상관관계
hist(df$GarageArea,
     main = '거실 면적에 대한 도수 분포표',
     xlab = '거실 면적',
     ylab = '빈도수',
     col = 'tomato'
     )
par(new =TRUE)
plot(x, yt003, lty = 5, col = 'blue', xlab ='', ylab = '',axes=FALSE,add = TRUE)


boxplot(df$GarageArea,
        main = "거실면적의 기본정보",
        col = 'tomato')

skewness(df$GarageArea)
kurtosis(df$GarageArea)

plot(round(df$SalePrice / 1000, 1),
     (df$GarageArea * 0.03),
     col = 'tomato',
     pch = '+',
     xlab = "가격 (Unit : 1000$)",
     ylab = "거실 면적 (단위 : 평)"
     )


# 2.2 질적변수
# 2.2.1 OverallQual(전체 상태 등급) 변수와 가격 간의 상관관계
table(df$OverallQual)
hist(df$OverallQual) 

boxplot(df$OverallQual)

boxplot(round(SalePrice/1000, 1) ~ OverallQual,
        data = df,
        xlab = "전체 상태 등급",
        ylab = "가격  (Unit : 1000$)",
        col = 3:13)

skewness(df$OverallQual)
kurtosis(df$OverallQual)

# 3. 이상치를 제거하고 자료값 확인

# 3.1
# 1. 자료에서 이상치 제거
df$SalePrice > 340000
df2 <- df

df2$SalePrice <- ifelse(df2$SalePrice > 340000, NA, df2$SalePrice)
df2$GarageArea <- ifelse(df$GarageArea > 954, NA, df2$GarageArea)

df3 <- df2[,c('SalePrice','GarageArea','OverallQual')]
df3
df3 <-  na.omit(df3)
df3
# 도수분포표로 시각화
par(mfrow = c(1, 2))
hist(round(df3$SalePrice / 1000, 1),
     col = 'yellow',
     main = '판매가의 도수분포표',
     xlab = "이상치를 제외한 가격  (Unit : 1000$)",
     ylab = "빈도수"
)
par(new =TRUE)
plot(x, yt003, type ='l' , col = 'blue', xlab ='', ylab = '',axes=FALSE)

hist(df3$GarageArea,
     main = '이상치를 제외한 거실면적에 대한 도수 분포표',
     xlab = '이상치를 제외한 거실면적',
     ylab = '빈도수',
     col = 'tomato'
)
par(new =TRUE)
plot(x, yt003, type ='l', col = 'blue', xlab ='', ylab = '',axes=FALSE)
par(mfrow = c(1, 1))

# 시각화를 통한 비교
# 이상치가 있냐 없냐에 따른 거실 면적 간의 산점도 비교
par(mfrow = c(1, 2))
plot(round(df$SalePrice / 1000, 1),
     (df$GarageArea * 0.03),
     col = 'tomato',
     pch = '+',
     xlab = "가격 (Unit : 1000$)",
     ylab = "거실 면적 (단위 : 평)"
)
plot(round(df3$SalePrice / 1000, 1),
     (df3$GarageArea * 0.03),
     col = 'cyan',
     pch = '+',
     xlab = "가격 (Unit : 1000$)",
     ylab = "이상치가 없는 거실 면적 (단위 : 평)"
)
par(mfrow = c(1, 1))

cor(df$SalePrice,df$GarageArea)
cor(df3$SalePrice,df3$GarageArea)
     
?cor

# 이상치를 제거하자 범위가 번짐도가 더 커졌다.

# 이상치를 제거하고 등급에 따른 산점도 비교
par(mfrow = c(1, 2))
boxplot(round(SalePrice/1000, 1) ~ OverallQual,
        data = df,
        xlab = "전체 상태 등급",
        ylab = "가격  (Unit : 1000$)",
        col = 3:13)
boxplot(round(SalePrice/1000, 1) ~ OverallQual,
        data = df3,
        xlab = "전체 상태 등급",
        ylab = "이상치가 없는 가격  (Unit : 1000$)",
        col = 3:13)
par(mfrow = c(1, 1))

# 이상치가 없는 경우 더 명확한 상관관계를 보였다.


## 첨도와 왜도, 상관계수
price_ske = skewness(df$SalePrice)
price_kur = kurtosis(df$SalePrice)

price_ske_3 = skewness(df3$SalePrice)
price_kur_3 = kurtosis(df3$SalePrice)

ga_ske = skewness(df$GarageArea)
ga_kur = kurtosis(df$GarageArea)

ga_ske_3 = skewness(df3$GarageArea)
ga_kur_3 = kurtosis(df3$GarageArea)

pg_cor = cor(df$SalePrice,df$GarageArea)
pg_cor_3 =cor(df3$SalePrice,df3$GarageArea)

po_cor = cor(df$SalePrice,df$OverallQual)
po_cor_3 =cor(df3$SalePrice,df3$OverallQual)

data_0 <-  c(price_ske, price_kur, ga_ske, ga_kur, pg_cor, po_cor)
data_3 <-  c(price_ske_3, price_kur_3, ga_ske_3, ga_kur_3, pg_cor_3, po_cor_3)
df_result <-  data.frame('모든 데이터' = round(data_0,2), '이상치 제거 데이터' = round(data_3,2))
rownames(df_result)=c('가격 왜도', '가격 첨도', '면적 왜도', '면적 첨도', '가격-면적 상관계수', '가격-등급 상관계수')
df_result

# 왜도와 첨도 둘다 0에 가까워 졌다.
# 왜도와 첨도는 정규분포에 가까울수록 자료의 신뢰성이 높아지는데
# r프로그램에서는 왜도와 첨도 모두 0에 가까울수록 좋다
# 상관계수는 그대로
# 더 유의미한 데이터라고 할 수 있다.

# 결론
# 데이터 정리가 데이터 간의 신뢰성을 상관관계에 필요한 가장 중요한 부분분
