
# 선형모델의 일반화

# 데이터
library(palmerpenguins)
pg <- penguins

# 결측치 제거
pg <- pg[complete.cases(pg), -8]

# 데이터 확인
str(pg)
dim(pg)

# 종이 아델리이면 Yes, 아니면 No 컬럼 생성
pg$is.adelie <- factor(ifelse(pg$species == "Adelie", "Yes", "No"))

# 그래프
barplot(table(pg$is.adelie))

# 종 컬럼 제거
pg <- pg[, -1]
str(pg)

# 일반화 선형모델(Generalized Linear Model) 생성
model <- glm(is.adelie ~ ., data = pg,
           family = binomial(link = logit))
summary(model)

# model$fitted: 0이될 확률
model$fitted

# model$fitted.values가 0.5보다 크면 Yes, 작으면 No
pg$pred <- ifelse(model$fitted.values > 0.5, "Yes", "No")

table(pg$is.adelie, pg$pred)


# Iris
df <- iris
df$Species <- factor(ifelse(df$Species == "virginica", "Yes", "No"))

model <- glm(Species ~ ., data = df,
           family = binomial(link = "logit"))
summary(model)

df$pred <- factor(ifelse(model$fitted.values > 0.5, "PYes", "PNo"))
tab <- table(df$Species, df$pred)
tab
TP <- tab[2, 2]
TN <- tab[1, 1]
FP <- tab[1, 2]
FN <- tab[2, 1]

# 정확성
accuracy <- (TP + TN) / (TP + TN + FP + FN)

# ROC(Receiver Operating Characteristic)커브가 좌상단에 붙어있을수록 좋은 이진분류기
library(pROC)
roc(Species ~ model$fitted.values, data = df,
    plot = TRUE, main = "ROC CURVE", col = "tomato")

# 공분산 분석


















