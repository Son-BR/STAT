# 범주 -> 종속 T검정, 분산분석


# Data Science Job Salaries

# 데이터 불러오기
df <- read.csv("ds_salaries.csv")

str(df)
df <- df[, -1]
sum(is.na(df))

df$work_year # 1
df$experience_level # 2
df$employment_type # 3
df$job_title # 4
df$salary_currency # 6
df$employee_residence # 8
df$remote_ratio # 9
df$company_location # 10
df$company_size # 11



df$salary # 5
df$salary_in_usd # 7

# 범주형 변수로
df$work_year <- factor(df$work_year)

str(df)
table(df$work_year)
prop.table(table(df$work_year))

# 빈도 교차표
# table(df$work_year, df$experience_level)

# 교차표의 행과 열에 대한 빈도합과 비율
# margin.table(교차표, margin=1) #행, 열 설정

# 다차원테이블
# with(df, table(컬럼1, 컬럼2, 컬럼3))





# 연속형 변수
median(df$salary_in_usd)
max(df$salary_in_usd)
min(df$salary_in_usd)

# 하위 5퍼센트, 상위 5퍼센트
quantile(df$salary_in_usd, prob = 0.05)
quantile(df$salary_in_usd, prob = 0.95)
quantile(df$salary_in_usd)
summary(df$salary_in_usd)

# 종속변수 정규성 검정
hist(df$salary_in_usd)

library(car)
qqPlot(df$salary_in_usd)
shapiro.test(df$salary_in_usd)



df$job_title <- factor(df$job_title)
tapply(df$salary_in_usd, df$job_title, mean)
sort(tapply(df$salary_in_usd, df$job_title, mean))

# 일원분산분석

# salary_in_usd ~ work_year

table(df$work_year)
barplot(tapply(df$salary_in_usd, df$work_year, mean))

# 이상치제거

year.out <- boxplot(salary_in_usd ~ work_year, data = df)$out
df.yearout <- df[!(df$salary_in_usd %in% year.out), ]
tapply(df.yearout$salary_in_usd, df.yearout$work_year, mean)





# df$salary_in_usd ~ df$experience_level
df$experience_level <- factor(df$experience_level)

prop.table(table(df$experience_level))
tapply(df$salary_in_usd, df$experience_level, mean)

boxplot(df$salary_in_usd ~ df$experience_level)

# 이상치 제거
ex.out <- boxplot(df$salary_in_usd ~ df$experience_level)$out
df.exout <- df[!(df$salary_in_usd %in% ex.out), ]
tapply(df.exout$salary_in_usd, df.exout$experience_level, mean)
outlierTest(ex.aov)

# aov()
ex.aov <- aov(df$salary_in_usd ~ df$experience_level)
summary(ex.aov)

exout.aov <- aov(df.exout$salary_in_usd ~ df.exout$experience_level)
summary(exout.aov)





# df$salary_in_usd ~ df$employment_type
df$employment_type <- factor(df$employment_type)
table(df$employment_type)
prop.table(table(df$employment_type))
boxplot(df$salary_in_usd ~ df$employment_type)

# 이상치 제거
ty.out <- boxplot(df$salary_in_usd ~ df$employment_type)$out
df.tyout <- df[!(df$salary_in_usd %in% ex.out), ]
tapply(df.tyout$salary_in_usd, df.tyout$employment_type, mean)
outlierTest(ex.aov)

# aov()
ty.aov <- aov(df$salary_in_usd ~ df$employment_type)
summary(ty.aov)

tyout.aov <- aov(df.tyout$salary_in_usd ~ df.tyout$employment_type)
summary(tyout.aov)







# df$salary_in_usd ~ df$remote_ratio
df$remote_ratio <- factor(df$remote_ratio)
unique(df$remote_ratio)

table(df$remote_ratio)
prop.table(table(df$remote_ratio))

tapply(df$salary_in_usd, df$remote_ratio, mean)

boxplot(df$salary_in_usd ~ df$remote_ratio)

# 이상치 제거
rem.out <- boxplot(df$salary_in_usd ~ df$remote_ratio)$out
df.remout <- df[!(df$salary_in_usd %in% rem.out), ]
tapply(df.remout$salary_in_usd, df.remout$remote_ratio, mean)

# aov()
rem.aov <- aov(df$salary_in_usd ~ df$remote_ratio)
summary(rem.aov)

remout.aov <- aov(df.remout$salary_in_usd ~ df.remout$remote_ratio)
summary(exout.aov)




# df$salary_in_usd ~ df$company_size

df$company_size <- factor(df$company_size)

table(df$company_size)
prop.table(table(df$company_size))

tapply(df$salary_in_usd, df$company_size, mean)

boxplot(df$salary_in_usd ~ df$company_size)

# 이상치 제거
cop.out <- boxplot(df$salary_in_usd ~ df$company_size)$out
df.copout <- df[!(df$salary_in_usd %in% cop.out), ]
tapply(df.copout$salary_in_usd, df.copout$company_size, mean)

# aov()
cop.aov <- aov(df$salary_in_usd ~ df$company_size)
summary(cop.aov)

copout.aov <- aov(df.copout$salary_in_usd ~ df.copout$company_size)
summary(copout.aov)



# df$salary_in_usd ~ df$job_title

df$job_title <- factor(df$job_title)


job.mean <- tapply(df$salary_in_usd, df$job_title, mean)
sort(job.mean, decreasing = T)

barplot(sort(job.mean, decreasing = T)[1:5], las = 2)
names(sort(job.mean, decreasing = T))


boxplot(df$salary_in_usd ~ df$job_title)

# 이상치 제거
job.out <- boxplot(df$salary_in_usd ~ df$job_title)$out
df.jobout <- df[!(df$salary_in_usd %in% job.out), ]
tapply(df.jobout$salary_in_usd, df.jobout$job_title, mean)

# aov()
job.aov <- aov(df$salary_in_usd ~ df$job_title)
summary(job.aov)

jobout.aov <- aov(df.jobout$salary_in_usd ~ df.jobout$job_title)
summary(jobout.aov)

jobout.mean <- tapply(df.jobout$salary_in_usd, df.jobout$job_title, mean)
sort(jobout.mean, decreasing = T)

barplot(sort(jobout.mean, decreasing = T)[1:5], las = 2)

# 연봉 분포 루트씌워서 정규모양
hist(sqrt(df$salary_in_usd))

# 범주 더미변수후 회귀분석
tapply(df$salary_in_usd, df$experience_level, mean)
exp.lm <- lm(df$salary_in_usd ~ df$experience_level)
summary(exp.lm)

contrasts(df$experience_level)



# 독립변수간 x^2
table(df$experience_level, df$employment_type)
chisq.test(df$experience_level, df$employment_type)


library(vcd)
assocstats(table(df$experience_level, df$employment_type))

names(df)


boxplot(salary_in_usd ~ employment_type*remote_ratio, data=df)

interaction.plot(df$employment_type, df$remote_ratio, df$salary_in_usd)

table(df$employment_type)
# 직급별 재택근무 비율 

df[df$employment_type=="FT",]

unique(df$company_location)
hist(table(df$company_location))
table(df$company_location)


tapply(df$salary_in_usd, df$work_year, mean)


# 규모, 직급 이원분산분석
library(gplots)
plotmeans(salary_in_usd ~ interaction(company_size, experience_level, sep = ' '),
         data = df, las=2,
         connect = list(c(1,4,7,10), c(2,5,8,11), c(3,6,9,12)),
         col = c('blue', 'red', 'yellow'))


boxplot(df$salary_in_usd)$out
df.2 <- df[!(df$salary_in_usd %in% boxplot(df$salary_in_usd)$out), ]
df.2$job_title

usd<-tapply(df.2$salary_in_usd, df.2$job_title, mean)
sort(usd)[1:10]
max(table(df.2$job_title))
