library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggfx)
library(ggthemes)
library(tidyverse)
library(countrycode)
library(ggrepel)
library(ggdark) 
library(plotly)


#Plotting code----


#loading in dataset
df <- read.csv("master.csv")
names(df)[1] <- "country"


#MUTATE CONTINENTS TO COUNTRIES
continent <- c(countrycode(sourcevar = df$country, origin = "country.name",destination = "continent"))
df$continent <- continent

south_america <- c('Argentina', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Guyana', 'Paraguay', 'Suriname', 'Uruguay')

df$continent[df$country %in% south_america] <- 'South America'
df$continent[df$continent=='Americas'] <- 'North America'
#split the Americas into north and south

df$age <- factor(df$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))
#rearrange for chronological order


#average global suicide number by year and generation

generation_tibble <- df %>%
  select(year, sex, suicides_no, population,generation) %>%
  group_by(year, sex,generation) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))


#age and continents
continent_age_tibble <- df %>%
  select(continent, age, suicides_no, population,generation) %>%
  group_by(continent, age) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
  ungroup()



#continent and gender
continent_sex_tibble <- df %>%
  select(continent, sex, suicides_no, population) %>%
  group_by(continent, sex) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
  ungroup()

continent_sex_tibble %>%
  mutate(mean_sex=mean(suicide_capita))
#getting the average of each continent
continent_sex_tibble <- continent_sex_tibble %>%
  ungroup() %>%
  add_row(continent="Africa",sex="mean",suicide_capita=1.27) %>%
  add_row(continent="Asia",sex="mean",suicide_capita=14.4) %>%
  add_row(continent="Europe",sex="mean",suicide_capita=18.5) %>%
  add_row(continent="North America",sex="mean",suicide_capita=10.6) %>%
  add_row(continent="Oceania",sex="mean",suicide_capita=13.0) %>%
  add_row(continent="South America",sex="mean",suicide_capita=5.63)



#global suicides by age
age_tibble <- df %>%
  select(year, age, suicides_no, population) %>%
  group_by(year, age) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))


##global suicides by year
year_tibble <- df %>%
  select(year,suicides_no, population) %>%
  group_by(year) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))



#shiny----
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Interactive Dashboard"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Generational Plot",tabName = "p1", icon = icon("cars")),
                        menuItem("Age & Continents Plot",tabName = "p2", icon = icon("cars")),
                        menuItem("Continent & Gender Plot",tabName = "p3", icon = icon("cars")),
                        menuItem("Suicide by Age Plot",tabName = "p4", icon = icon("cars")),
                        menuItem("Suicide By Year Plot",tabName = "p5", icon = icon("cars"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem("p1",
                                fluidRow(align="center",
                                 plotlyOutput("plot1",height = 800, width = 1600)
                                ),box(sliderInput("slider1", label = h3("Slider Range"), min = 1985, 
                                                                  max = 2016, value = c(1, 80))),
                        ),
                        tabItem("p2",
                                fluidRow(align="center",
                                  plotlyOutput("plot2",height = 800, width = 1600)
                                )
                              ),
                        tabItem("p3",
                                fluidRow(align="center",
                                  plotlyOutput("plot3",height = 800, width = 1600)
                                )
                              ),
                        tabItem("p4",
                                fluidRow(align="center",
                                  plotlyOutput("plot4",height = 800, width = 1600)
                                ),box(sliderInput("slider4", label = h3("Slider Range"), min = 1985, 
                                                 max = 2016, value = c(1, 80))), 
                                
                        ),
                        
                        tabItem("p5",
                                fluidRow(align="center",
                                  plotlyOutput("plot5",height = 800, width = 1600)
                                ),box(sliderInput("slider5", label = h3("Slider Range"), min = 1985, 
                                                  max = 2016, value = c(1, 80))), 
                                
                        )
                        
                      )
                    )
)


server <- function(input, output) {
#plot1----
  #setting up reactive years in generation tibble
  years1 <- reactive({
    seq(input$slider1[1], input$slider1[2], by = 1)
  })
  
  year1 <-reactive({
  generation_tibble %>%
    filter(year %in% years1())
})
  output$plot1 <- renderPlotly({ggplot(data=year1(),aes(year,suicide_capita,colour=sex)) +
      stat_summary(fun ="mean", size = 2, geom = "point") +
      stat_summary(aes(group=generation),fun="mean", geom="line",size=1, colour="purple") +
      labs(title = "Global Suicides By Generation", x="",y="Suicide Capita") +
      geom_smooth(method="loess",se=FALSE) +
      dark_theme_gray() +
      facet_grid(.~generation)
  })
#plot2----
  
  output$plot2 <- renderPlotly({ggplot(continent_age_tibble,aes(continent,suicide_capita,fill=continent)) +
    geom_bar(stat="summary_bin",fun="mean") +
    facet_grid(.~age) +
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
  })
#plot3----
  output$plot3 <- renderPlotly({ggplot(continent_sex_tibble,aes(continent,suicide_capita,fill=sex))+
    geom_bar(stat="identity",position = "dodge") +
    labs(title = "Suicides By Continent & Gender", x="Continents",y="Suicide Capita or GDP") +
    geom_hline(aes(yintercept = mean(suicide_capita)), color="white") +
    scale_fill_manual(values = c("#F8766D","#619CFF","purple")) +
    dark_theme_gray()
  })
#plot4----
  years4 <- reactive({
    seq(input$slider4[1], input$slider4[2], by = 1)
  })
  
  year4 <-reactive({
    age_tibble %>%
      filter(year %in% years4())
  })
  output$plot4 <- renderPlotly({ggplot(data=year4(),aes(year,suicide_capita,colour=age)) +
    geom_point() +
    geom_line() +
    labs(title = "Global Suicides By Age", x="Year",y="Suicide Capita") +
    theme(legend.title=element_blank()) +
    theme(legend.text = element_text(size = 1)) +
    dark_theme_gray()
  })
  
  
#plot5----
  years5 <- reactive({
    seq(input$slider5[1], input$slider5[2], by = 1)
  })
  
  year5 <-reactive({
    year_tibble %>%
      filter(year %in% years5())
  })
  
  output$plot5 <- renderPlotly({ggplot(data=year5(),aes(year,suicide_capita)) +
    geom_point() +
    geom_smooth(method = "loess") +
    geom_line() +
    labs(title = "Global Suicides By Year", x="Year",y="Suicide Capita") +
    theme(legend.title=element_blank()) +
    dark_theme_gray()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
