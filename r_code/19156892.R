library(ggplot2)
library(ggfx)
library(ggthemes)
library(tidyverse)
library(countrycode)
library(ggrepel)
library(ggdark)
library(plotly)
library(shiny)



df <- read.csv("master.csv")
names(df)[1] <- "country"


#MUTATE CONTINENTS TO COUNTRIES----
continent <- c(countrycode(sourcevar = df$country, origin = "country.name",destination = "continent"))
df$continent <- continent

south_america <- c('Argentina', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Guyana', 'Paraguay', 'Suriname', 'Uruguay')

df$continent[df$country %in% south_america] <- 'South America'
df$continent[df$continent=='Americas'] <- 'North America'
#split the Americas into north and south

df$age <- factor(df$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))
#rearrange for chronological order

#average global suicide number by year----
#3 (Chosen)
generation_tibble <- df %>%
  select(year, sex, suicides_no, population,generation) %>%
  group_by(year, sex,generation) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))

p1<-ggplot(generation_tibble,aes(year,suicide_capita,colour=sex)) +
  stat_summary(fun ="mean", size = 2, geom = "point") +
  stat_summary(aes(group=generation),fun="mean", geom="line",size=1, colour="purple") +
  labs(title = "Global Suicides By Generation", x="",y="Suicide Capita") +
  geom_smooth(method="loess",se=FALSE) +
  dark_theme_gray() +
  facet_grid(.~generation)

ggplotly(p1, dynamicTicks = TRUE) %>%
  layout(hovermode = "x unified",
         legend=1)

#age and continents----
#1(Chosen)
p2<-ggplot(df,aes(continent,suicides.100k.pop,fill=continent)) +
  geom_bar(stat="summary_bin",fun="mean") +
  facet_grid(~age) +
  dark_theme_gray()+
  labs(title = "Global Suicides By Year", x=" ",y="Suicide Capita") +
  stat_summary(
    aes(label = round(stat(y))),
    fun = "mean",
    geom = "text_repel",
    min.segment.length = 0, # always draw segments
    position = position_nudge(y = -2)
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplotly(p2)


#continent and gender----
#1 (Chosen)
continent_sex_tibble <- df %>%
  select(continent, sex, suicides_no, population) %>%
  group_by(continent, sex) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
  ungroup()

continent_sex_tibble %>%
  mutate(mean_sex=mean(suicide_capita))

continent_sex_tibble <- continent_sex_tibble %>%
  ungroup() %>%
  add_row(continent="Africa",sex="mean",suicide_capita=1.27) %>%
  add_row(continent="Asia",sex="mean",suicide_capita=14.4) %>%
  add_row(continent="Europe",sex="mean",suicide_capita=18.5) %>%
  add_row(continent="North America",sex="mean",suicide_capita=10.6) %>%
  add_row(continent="Oceania",sex="mean",suicide_capita=13.0) %>%
  add_row(continent="South America",sex="mean",suicide_capita=5.63)




p3<-ggplot(continent_sex_tibble,aes(continent,suicide_capita,fill=sex))+
  geom_bar(stat="identity",position = "dodge") +
  labs(title = "Suicides By Continent & Gender", x="Continents",y="Suicide Capita") +
  geom_hline(aes(yintercept = mean(suicide_capita)), color="white") +
  scale_fill_manual(values = c("#F8766D","#619CFF","purple")) +
  dark_theme_gray()
ggplotly(p3)


#global suicides by age----
age_tibble <- df %>%
  select(year, age, suicides_no, population) %>%
  group_by(year, age) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))

p4<-ggplot(age_tibble,aes(year,suicide_capita,colour=age)) +
  geom_point() +
  geom_line() +
  labs(title = "Global Suicides By Age", x="Year",y="Suicide Capita") +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 1)) +
  dark_theme_gray()

ggplotly(p4, dynamicTicks = TRUE) %>%
  layout(hovermode = "closest",
         legend=1)

##global suicides by year----
#1
year_tibble <- df %>%
  select(year,suicides_no, population) %>%
  group_by(year) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))

p5<-ggplot(year_tibble,aes(year,suicide_capita)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_line() +
  labs(title = "Global Suicides By Year", x="Year",y="Suicide Capita") +
  theme(legend.title=element_blank()) +
  dark_theme_gray()

ggplotly(p5, dynamicTicks = TRUE) %>%
  rangeslider() %>%
  layout(hovermode = "closest",
         legend=1)


