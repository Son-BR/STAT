###title: "피마 인디언 당뇨병 데이터베이스"
# Pregnancies: 임신 횟수
##### Glucose: 포도당 부하 검사 수치
##### BloodPressure: 혈압(mm Hg)
##### SkinThickness: 팔 삼두근 뒤쪽의 피하지방 측정값(mm)
##### Insulin: 혈청 인슐린(mu U/ml)
##### BMI: 체질량지수(체중(kg)/키(m))^2
##### DiabetesPedigreeFunction: 당뇨 내력 가중치 값
##### Age: 나이
##### Outcome: 클래스 결정 값(0 또는 1)

# 파일 불러오기
df <- read.csv('./diabetes/diabetes.csv')
str(df)

# 결측치 확인, 0 처리해야할 듯?
sum(is.na(df))
summary(df)

# 0 <- NA로 바꾸기
df$Glucose[df$Glucose==0] <- NA
df$BloodPressure[df$BloodPressure==0] <- NA
df$SkinThickness[df$SkinThickness==0] <- NA
df$BMI[df$BMI==0] <- NA

# NA 그래프로 보기
library(VIM)
aggr(df, numbers=T, prop=F)

# NA <- 정상과 환자 구분하여 각 변수 평균으로 채우기
df$Glucose[is.na(df$Glucose) & df$Outcome==0] <- mean(df$Glucose[df$Outcome==0], na.rm=T)
df$BloodPressure[is.na(df$BloodPressure) & df$Outcome==0] <- mean(df$BloodPressure[df$Outcome==0], na.rm=T)
df$SkinThickness[is.na(df$SkinThickness) & df$Outcome==0] <- mean(df$SkinThickness[df$Outcome==0], na.rm=T)
df$BMI[is.na(df$BMI) & df$Outcome==0] <- mean(df$BMI[df$Outcome==0], na.rm=T)
  
df$Glucose[is.na(df$Glucose) & df$Outcome==1] <- mean(df$Glucose[df$Outcome==1], na.rm=T)
df$BloodPressure[is.na(df$BloodPressure) & df$Outcome==1] <- mean(df$BloodPressure[df$Outcome==1], na.rm=T)
df$SkinThickness[is.na(df$SkinThickness) & df$Outcome==1] <- mean(df$SkinThickness[df$Outcome==1], na.rm=T)
df$BMI[is.na(df$BMI) & df$Outcome==1] <- mean(df$BMI[df$Outcome==1], na.rm=T)

# 데이터 정상과 환자 비교
barplot(table(df$Outcome), col = 2:3)

# 정상과 환자 비교
summary(df[df$Outcome==0, ])
summary(df[df$Outcome==1, ])   # 전체적으로 평균이 당뇨 환자가 높다


# 그래프 그려보자
# 마음에 들지 않는다
par(mfrow=c(1,2))
barplot(df$Pregnancies[df$Outcome==0], col=2)
barplot(df$Pregnancies[df$Outcome==1], col=3)
par(mfrow=c(1,1))

# 임신 횟수
boxplot(Pregnancies ~ Outcome, data = df,
        pch = 19, col = c('orange', 'tomato'), border = 'brown')

# 포도당 부하 검사 수치
boxplot(Glucose ~ Outcome, data = df,
        pch = 19, col = c('orange', 'tomato'), border = 'brown')

# 혈압
boxplot(BloodPressure ~ Outcome, data = df,
        pch = 19, col = c('orange', 'tomato'), border = 'brown')

# 팔 삼두근 뒤쪽의 피하지방 측정값(mm)
boxplot(SkinThickness ~ Outcome, data = df,
        pch = 19, col = c('orange', 'tomato'), border = 'brown')

# 혈청 인슐린(mu U/ml)
boxplot(Insulin ~ Outcome, data = df,
        pch = 19, col = c('orange', 'tomato'), border = 'brown')

# 체질량지수(체중(kg)/키(m))^2
boxplot(BMI ~ Outcome, data = df,
        pch = 19, col = c('orange', 'tomato'), border = 'brown')

# 당뇨 내력 가중치 값
boxplot(DiabetesPedigreeFunction ~ Outcome, data = df,
        pch = 19, col = c('orange', 'tomato'), border = 'brown')

# Age: 나이
boxplot(Age ~ Outcome, data = df,
        pch = 19, col = c('orange', 'tomato'), border = 'brown')


## 데이터프레임을 각각 10개의 데이터를 샘플링한다


set.seed(2022)
df.origin <- data.frame(id=1:nrow(df), 
                        Outcome=df$Outcome,
                        Pregnancies=df$Pregnancies,
                        Glucose=df$Glucose,
                        BloodPressure=df$BloodPressure,
                        SkinThickness=df$SkinThickness,
                        Insulin=df$Insulin,
                        BMI=df$BMI,
                        DiabetesPedigreeFunction=df$DiabetesPedigreeFunction,
                        Age=df$Age)
idx <- sample(ro, size=20)
df.sample <- df.origin[idx,]
df.sample


set.seed(2022)
df.x <- df.sample[sample(as.numeric(rownames(df[df$Outcome==0,])), size=268), c(1,2,3:6)]
df.x[order(df.x$id), ]
df.y <- df.sample[sample(rownames(df[df$Outcome==0,]), size=268), c(1,2,7:10)]
df.y[order(df.y$id), ]
df.merge <- merge(x=df.x, y=df.y, by=c('id', 'Outcome'), all = T)
df.merge
a<-as.numeric(rownames(df[df$Outcome==0,]))
numeric
a
library(psych)
describe(dg.merge)[c(3:10),3]
describe(df)[-9,3]
names(df.merge)[3:10]
plot(describe(df.merge)[c(3:10),3], describe(df)[-9,3], col = 3:10, pch = 19)

cor(describe(df.merge)[c(3:10),3], describe(df)[-9,3])

names(df)
nrow(df.origin)
#########################################
set.seed(2022)
rows <- as.numeric(rownames(df[df$Outcome==0,]))
df.origin <- data.frame(Outcome=df[rows,]$Outcome,
                        Pregnancies=df[rows,]$Pregnancies,
                        Glucose=df[rows,]$Glucose,
                        BloodPressure=df[rows,]$BloodPressure,
                        SkinThickness=df[rows,]$SkinThickness,
                        Insulin=df[rows,]$Insulin,
                        BMI=df[rows,]$BMI,
                        DiabetesPedigreeFunction=df[rows,]$DiabetesPedigreeFunction,
                        Age=df[rows,]$Age)

idx <- sample(rows, size=268)
df.sample <- df[idx,]
nrow(df.sample) 
nrow(df[df$Outcome==1,]) 

#########################################

dfall <- merge(x = df.sample, y = df[df$Outcome==1,], all = T)
df0 <- dfall[dfall$Outcome==0, ]
df1 <- dfall[dfall$Outcome==1, ]
df0
data.frame(name=names(df),
  value0=describe(df0)[,3],
  value1=describe(df1)[,3])
