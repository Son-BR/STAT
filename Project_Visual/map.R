# Libraries
library(ggplot2)
library(dplyr)

# Get the world polygon and extract UK
library(maps)
library(ggrepel)

ko.map <- map_data("world") %>% filter(region=="South Korea")
data <- world.cities %>% filter(country.etc=="Korea South")
data <- data[data$name %in% c("Pusan","Inchon","Kwangyang","Ulsan"),]
data$name <- c("인천", "광양", "부산", "울산")

# 기본
mapplot <- ggplot() +
  geom_polygon(data = ko.map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.2) +
  theme_void() + coord_map() + ylim(34,38.5) + xlim(126, 129.6) + 
  geom_point(data=data, aes(x=long, y = lat, size=3), col="tomato")+
  theme(legend.position="none")



# plotly
library(plotly) 
ggplotly(mapplot)










# 영국 지도 예제

UK <- map_data("world") %>% filter(region=="UK")
data <- world.cities %>% filter(country.etc=="UK")

ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  #geom_text_repel( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
  geom_point( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void() + ylim(50,59) + coord_map() +
  theme(legend.position="none")


