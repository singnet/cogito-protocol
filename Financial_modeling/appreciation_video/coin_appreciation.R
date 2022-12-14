# Library
require(ggplot2)
require(dplyr)
require(tidyr)
require(lubridate)
require(gganimate)
require(gifski)
require(av)
require(gapminder)
library(timetk)
library(datasets)
library(zoo)
library(reshape)

setwd(getwd())

df <- read.csv("currencies_v2.csv", header = T)

tk_tbl(df,preserve_index = F)

df <- transform(df, date = zoo::as.Date(Date, frac=0))
df <- as_tibble(df)

plotted_data <- data.frame(data.frame(df$Date,df$EUR,df$G.coin))
colnames(plotted_data) <- c("Date","EUR","G-coin")


plotted_data$date <- plotted_data$Date
plotted_data$Date <- 1:nrow(plotted_data)
plotted_data$date <- as.Date(plotted_data$date,"%d/%m/%Y")
plotted_data$Dollar <- 1

plotted_data <- plotted_data[!(names(plotted_data) %in% c("Date"))]


plotted_data <- melt(plotted_data,id.vars=c("date"),measured.vars=c("EUR","Dollar","G-coin"))

p <- ggplot(data = plotted_data, mapping = aes(x = date, y = value,group = variable, colour = variable)) +
  geom_line() +
  labs(x = "Year", y = "USD value", 
       title = "G-coin/ Euro appreciation vs US Dollar") +
  geom_text(aes(label = variable), vjust = 0) +
  scale_color_manual(values=c("#D1AC00", "#F47D20","#FFFFFF")) +
  theme(legend.position="none",panel.grid = element_line(colour = "#53625C",linewidth  = 0),
        plot.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"), 
        panel.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"),
        plot.title= element_text(size=15,color="#FFFFFF",face="bold",hjust = 0.5),
        axis.text.x=element_text(colour="#FFFFFF"),
        axis.text.y=element_text(colour="#FFFFFF"),
        axis.title.y = element_text(colour="#FFFFFF"),
        axis.title.x = element_text(colour="#FFFFFF"),
        legend.background = element_rect(fill = "#1d1e1e"),
        legend.title = element_text(size=15,color="#FFFFFF",face="bold"),
        legend.text = element_text(colour="#FFFFFF"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  transition_reveal(along = date) + view_follow()

p <- p +  guides(fill="none")# + guides(colour = guide_legend(""))
p


animate(p,nframes = 800, fps = 10)
anim_save("EUR_dollar.gif", animation = last_animation())


