# 종별로 평균, 표준편차, 개수

# 펭귄
library(palmerpenguins)
df<-na.omit(penguins)

table(df$species)
prop.table(table(df$species))

table(df$island, df$species)

tapply(df$species,
       INDEX = list(df$species),
       FUN=length)


# 패키지설치 : install.packages("모듈이름") 
install.packages("gmodels") 
library(gmodels)
CrossTable(df$island,df$species,prop.t=F,prop.chisq=F)

install.packages("psych")
library(psych)
describe(df)
describe(df)[,c(2:4,8:9)]


aggregate(df[,3:6],
          by=list(species=df$species),
          FUN=mean)

tapply(df$bill_length_mm,
       INDEX = list(species=df$species),
       FUN=mean)

boxplot(flipper_length_mm ~ species,
        data=df,
        col=2:4)
####
# boxplot.stats(df[df$species=='Adelie',5])
# 
# df[df$species=='Adelie',5]
# df$flipper_length_mm[df$species=='Adelie']
# 
# boxplot.stats(df[df$species=='Adelie',5])$out
# outlier<- boxplot.stats(df[df$species=='Adelie',5])$out
# outlier
####

# boxplot.stats() 박스플롯 이상치 찾기 
df<-data.frame(df)
adelie<-split(df,df$species)$Adelie
adelie

outlier<-boxplot.stats(adelie$flipper_length_mm)$out
outlier
adelie[adelie$flipper_length_mm %in% outlier,]




library(palmerpenguins)
df<-na.omit(penguins)
df<-data.frame(df)

# 날개 길이 오름차순, 체질량 내림차순으로 정렬

ord<-order(df$flipper_length_mm,-df$body_mass_g)
ord
df[ord,]
head(df[ord,],n=10)

head(df[ord,5:6],n=10)


