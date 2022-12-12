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
  labs(x = "Date", y = "USD value", 
       title = "Appreciation of G-coin and Euro versus the dollar over time") +
  geom_text(aes(label = variable), vjust = 0) +
  scale_color_manual(values=c("#D1AC00", "#F47D20","#0C1618")) +
  theme(plot.background = element_rect(fill = "#53625C"), 
        panel.background = element_rect(fill = "#53625C", colour="#53625C"),
        plot.title= element_text(size=15,color="#FFFFFF",face="bold"),
        axis.text.x=element_text(colour="#FFFFFF"),
        axis.title.y = element_text(colour="#FFFFFF"),
        axis.title.x = element_text(colour="#FFFFFF"),
        legend.background = element_rect(fill = "#53625C"),
        legend.title = element_text(size=15,color="#FFFFFF",face="bold"),
        legend.text = element_text(colour="#FFFFFF")) +
  transition_reveal(along = date) + view_follow()

p <- p + guides(colour = guide_legend("Legend"))

p

animate(p,nframes = 800, fps = 10)
anim_save("EUR_dollar.gif", animation = last_animation())


