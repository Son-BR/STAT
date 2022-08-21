df <- read.csv('./SleepStudyData.csv')
str(df)

# 범주형 ~ 수치형 데이터 ----------------------
# 로지스틱 회귀분석 (Enough~.)

# 종속변수 : Enough 타입 변경 (chr -> factor)
df$Enough <- factor(ifelse(df$Enough=='Yes',1,0))
df <- subset(df, !is.na(Enough),c('Hours','Tired'))
model <- glm(Enough~Hours+Tired,
            family ='binomial'(link='logit'),
            data = df)

# ROC
library(pROC)
Hours_roc = roc()

summary(model)

# 오즈비 추출
exp(model$coefficients)

# 직접적으로 바로 신뢰구간을 포함한 오즈비 확인
exp(confint(model))
table(df)

# 범주형 ~ 범주형
# ===========================================================================
# 1. 수면만족도 ~ 손 닿는 거리 : 카이제곱분석(교차분석)
install.packages('gmodels')
library(gmodels)

df <- read.csv('./SleepStudyData.csv')
# Enough ~ PhoneReach
# '스마트폰에 손 닿는 거리에 따른 만족도 차이'테이블 만들기.
df$Enough[df$Enough=='Yes'] <-'수면만족'
df$Enough[df$Enough=='No'] <-'수면불만족'
df$Enough <- as.factor(df$Enough)

df$PhoneReach[df$PhoneReach=='Yes'] <-'손 닿는 거리'
df$PhoneReach[df$PhoneReach=='No'] <-'손 닿지 않는 거리'
df$PhoneReach <- as.factor(df$PhoneReach)
table(df$PhoneReach,df$Enough)

#동질성 검정
chisq.test(df$PhoneReach,df$Enough)
# p-value > 0.05
# H0(귀무가설 기각 불가능)
# 해석: 잘 때 손 닿는 곳에 스마트폰의 유무에 따라 수면 만족도는 "차이가 없다."

# ==============================================================================
# 2. 수면만족도 ~ 손 닿는 거리 : 카이제곱분석(교차분석)
df$PhoneTime[df$PhoneTime == 'Yes'] <-'자기 전 30분 이내 스마트폰 사용 O'
df$PhoneTime[df$PhoneTime == 'No'] <-'자기 전 30분 이내 스마트폰 사용 X'
df$PhoneTime <- as.factor(df$PhoneTime)
table(df$PhoneTime,df$Enough)

#동질성 검정
chisq.test(df$PhoneTime,df$Enough)
# p-value > 0.05
# H0(귀무가설 기각 불가능)
# 해석: 잠들기 전 30분 이내 스마트폰 사용 여부에 따라 수면 만족도는 "차이가 없다."
#==================================================================================
# 3. 수면만족도 ~ 손 닿는 거리 : 카이제곱분석(교차분석)
df$Breakfast[df$Breakfast == 'Yes'] <-'평소 아침식사 O'
df$Breakfast[df$Breakfast == 'No'] <-'평소 아침식사 X'
df$Breakfast <- as.factor(df$Breakfast)
table(df$Breakfast,df$Enough)

#동질성 검정
chisq.test(df$Breakfast,df$Enough)
# p-value > 0.05
# H0(귀무가설 기각 불가능)
# 해석: 평소 아침식사 여부에 따라 수면 만족도는 "차이가 없다."
#==================================================================================