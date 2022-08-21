DIA1 <- read.csv("../남재환/diamonds.csv", header = T)
df <- DIA1
plot(price ~ carat, data = df, pch = 19, col = "steelblue")

cor(df$carat, df$price)
formula <- price ~ carat
model <- lm(price ~ carat, data = df)
model
abline(model, lwd=2, col='tomato')