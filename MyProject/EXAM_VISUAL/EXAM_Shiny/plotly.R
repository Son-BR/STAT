library(plotly)
library(ggplot2)
library(gapminder)

# ggplot그래프 ggplotly로 호출하면 상호작용 가능

p <- ggplot(data = subset(gapminder, year == 2007),
       mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent)) +
  scale_x_log10() +
  geom_smooth()

ggplotly(p)


data(sleep)
sleep %>%
  plot_ly() %>%
  add_trace(x = ~ID,
            y = ~extra,
            type = "bar") %>%
  layout(title = "Bar plot",
         xaxis = list(title = "Species"),
         yaxis = list(title = "Frequency"))


library(palmerpenguins)
penguins %>%
  plot_ly(x = ~bill_length_mm,
           y = ~bill_depth_mm,
           name = ~species,
           hovertext = ~ island,
           hoverinfo = "x+y+name+text")


# x축: gdpPercap, y축: lifeExp
# hover: country, pop
gapminder[gapminder$year == 2007,] %>%
  plot_ly(x = ~ gdpPercap,
          y = ~ lifeExp,
          name = ~country,
          hovertext = ~pop,
          hoverinfo = "x+y+name+text"
          )

library(esquisse)
esquisser()
