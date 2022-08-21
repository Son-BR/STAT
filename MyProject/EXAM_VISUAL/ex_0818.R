# 데이터 시각화 실습

library(tidyverse)

str(diamonds)
str(mpg)


# anscombe 데이터
anscombe
ans <- anscombe

mean(ans$x1)
mean(ans$x2)
mean(ans$x3)
mean(ans$x4)

mean(ans$y1)
mean(ans$y2)
mean(ans$y3)
mean(ans$y4)

cor(ans$x1, ans$y1)
cor(ans$x2, ans$y2)
cor(ans$x3, ans$y3)
cor(ans$x4, ans$y4)

lm(y1 ~ x1, data = ans)
lm(y2 ~ x2, data = ans)
lm(y3 ~ x3, data = ans)
lm(y4 ~ x4, data = ans)

par(mfrow = c(2, 2))

plot(ans$x1, ans$y1, col = "orange", pch = 19)
abline(lm(y1 ~ x1, data = ans), col = "tomato")

plot(ans$x2, ans$y2, col = "orange", pch = 19)
abline(lm(y2 ~ x2, data = ans), col = "tomato")

plot(ans$x3, ans$y3, col = "orange", pch = 19)
abline(lm(y3 ~ x3, data = ans), col = "tomato")

plot(ans$x4, ans$y4, col = "orange", pch = 19)
abline(lm(y4 ~ x4, data = ans), col = "tomato")

par(mfrow = c(1, 1))



# ggplot

# aes
ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy)) +
       # aes 인수: x = x축, y = y축, color = 그래프색/테두리, shape=점모양, fill = 채우는색,
       #           size = 선굵기/점크기, alpha = 투명도, linetype = 선패턴, labels = 표나축의텍스트
       # 배경생성
    geom_point() # 산점도

p <- ggplot(data = mpg,
            mapping = aes(x = displ, y = hwy))

p + geom_point(mapping = aes(color = class))

table(mpg$class)

p + geom_point(mapping = aes(color = class,
                             size = class))

p + geom_point(mapping = aes(color = class,
                             size = class,
                             alpha = 0.3))

p + geom_point(mapping = aes(color = class,
                             shape = class))

p + geom_point(color = "tomato") +
    facet_wrap(~ class, nrow = 3) # 클래스별로 그래프 나눠 그리기

p + geom_point(color = "tomato") +
    facet_wrap(drv ~ cyl)

p + geom_point(mapping = aes(color = class)) +
    geom_smooth(color = "tomato") # 기하그래프


# diamonds 데이터
head(diamonds)

p <- ggplot(data = diamonds,
            mapping = aes(x = cut))

p + geom_bar() # 막대그래프(x축만 주면 자동으로 빈도 그래프 그려줌)

p + geom_bar(mapping = aes(fill = clarity)) # 누적막대그래프(집단별 색 다르게 채움)

p + geom_bar(mapping = aes(fill = clarity),
             position = "fill") # 비율 막대로 그리기

p + geom_bar(mapping = aes(fill = clarity),
             position = "dodge") # 집단별로 다른막대로 그리기


# 데이터 사우루스
library(datasauRus)

data(package = "datasauRus")
dd <- datasaurus_dozen

str(dd)
unique(dd$dataset)

plot(y ~ x, data = subset(dd, dataset == "dino"),
     pch = 19, col = "tomato")

ggplot(data = dd,
       mapping = aes(x = x, y = y)) +
    geom_point(mapping = aes(color = dataset)) +
    facet_wrap(~ dataset, nrow = 3)


# 여러가지 그래프
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, color = class),
            position = "jitter") # 포인트 흔들기

p <- ggplot(mpg, aes(x = class, y = hwy))
p + geom_boxplot(fill =  "tomato") +
    coord_flip() # x축, y축 뒤집기

p <- ggplot(diamonds, aes(x = cut, fill = cut))
p + geom_bar(show.legend = FALSE, width = 1) + # 범례숨기기, 막대굵기 조절
    coord_polar() # 폴라도표(장미도표)

# 지도 그래프
library(maps)
world <- map_data("world")
ggplot(world, aes(long, lat, group = group)) +
    geom_polygon(fill = "skyblue", color = "blue")


# %>% : 파이프라인 연산자

str(mpg)

# 파이프라인 사용 전 후
group_by(mpg, class)
mpg %>% group_by(class)


best.in.class <- mpg %>%
    group_by(class) %>%
    filter(row_number(desc(hwy)) == 1)

p <- ggplot(data = mpg,
            mapping = aes(x = displ, y = hwy))

p + geom_point(mapping = aes(color = class)) +
    geom_label(mapping = aes(label = model),
              data = best.in.class,
              nudge_y = 2, alpha = 0.5) + # label 위치, 투명도
    theme(legend.position = "top") + # 범례 위치
    theme_minimal() # 배경 투명

# 그래프 저장(마지막으로 그린 그래프 파일로 저장)
ggsave(file = "save.pdf02.pdf", width = 1920, height = 1080, units = "px")
ggsave(file = "save.png01.png",
       width = 1920, height = 1080, units = "px")

# 펭귄
library(palmerpenguins)
library(hrbrthemes)
pg <- penguins
pg <- pg[complete.cases(pg), ]

str(pg)
pg %>%
  ggplot(aes(x = body_mass_g, fill = sex)) +
    geom_histogram(color = "#e9ecef", alpha = 0.6, position = "identity") +
    scale_fill_manual(values = c("#69b3a2", "#404080")) +
    theme_ipsum() +
    labs(fill = "")

# 갭마인더
library(gapminder)

library(esquisse)
# R: tools-Addins-esquisse선택후 Excute
esquisse::esquisser()


library(dplyr)
library(ggplot2)

gapminder::gapminder %>%
 filter(year >= 2005L & year <= 2007L) %>%
 ggplot() +
 aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop) +
 geom_point(shape = "bullet") +
 scale_color_hue(direction = 1) +
 labs(x = "x라벨", y = "y라벨", title = "타이틀", subtitle = "서브타이틀",
 caption = "캡션", color = "컬러라벨", size = "사이즈라벨") +
 theme_minimal()


gapminder::gapminder %>%
  filter(year >= 1997L & year <= 2007L) %>%
  ggplot() +
  aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop) +
  geom_point(shape = "bullet") +
  scale_color_hue(direction = 1) +
  labs(x = "x라벨", y = "y라벨", title = "타이틀", subtitle = "서브타이틀",
       caption = "캡션", color = "컬러라벨", size = "사이즈라벨") +
  theme_classic() +
  theme(legend.position = "bottom") +
  facet_wrap(vars(year))

#####
