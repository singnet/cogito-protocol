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
  geom_line(size=2) +
  labs(x = "" ,y = "USD value", 
       title = stringr::str_wrap("G-coin/ Euro appreciation vs US Dollar",width = 40)) +
  geom_text(aes(label = variable),size=13, vjust = 0) +
  scale_color_manual(values=c("#D1AC00", "#F47D20","#FFFFFF")) +
  theme(legend.position="none",panel.grid = element_line(colour = "#53625C",linewidth  = 0),
        plot.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"), 
        panel.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"),
        plot.title= element_text(size=40,color="#FFFFFF",face="bold",hjust = 0.5),
        axis.text.x=element_text(size=22,colour="#FFFFFF"),
        axis.text.y=element_text(size=22,colour="#FFFFFF"),
        axis.title.y = element_text(size=22,colour="#FFFFFF"),
        axis.title.x = element_text(size=22,colour="#FFFFFF"),
        legend.background = element_rect(fill = "#1d1e1e"),
        legend.title = element_text(size=40,color="#FFFFFF",face="bold"),
        legend.text = element_text(colour="#FFFFFF"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  transition_reveal(along = date) + view_follow()

p <- p +  guides(fill="none")# + guides(colour = guide_legend(""))
p


animate(p,nframes = 2000, fps = 25, height = 720*2, width =1280*2, renderer = av_renderer())
anim_save("EUR_dollar.mp4", animation = last_animation())




### Other currencies:

### GBP:
plotted_data <- data.frame(data.frame(df$Date,df$EUR,df$G.coin))
colnames(plotted_data) <- c("Date","GBP","G-coin")


plotted_data$date <- plotted_data$Date
plotted_data$Date <- 1:nrow(plotted_data)
plotted_data$date <- as.Date(plotted_data$date,"%d/%m/%Y")
plotted_data$Dollar <- 1

plotted_data <- plotted_data[!(names(plotted_data) %in% c("Date"))]


plotted_data <- melt(plotted_data,id.vars=c("date"),measured.vars=c("GBP","Dollar","G-coin"))

p <- ggplot(data = plotted_data, mapping = aes(x = date, y = value,group = variable, colour = variable)) +
  geom_line(size=2) +
  labs(x = "", y = "USD value", 
       title = "G-coin/ British pound appreciation vs US Dollar",width = 40) +
  geom_text(aes(label = variable),size=13, vjust = 0) +
  scale_color_manual(values=c("#D1AC00", "#F47D20","#FFFFFF")) +
  theme(legend.position="none",panel.grid = element_line(colour = "#53625C",linewidth  = 0),
        plot.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"), 
        panel.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"),
        plot.title= element_text(size=40,color="#FFFFFF",face="bold",hjust = 0.5),
        axis.text.x=element_text(size=22,colour="#FFFFFF"),
        axis.text.y=element_text(size=22,colour="#FFFFFF"),
        axis.title.y = element_text(size=22,colour="#FFFFFF"),
        axis.title.x = element_text(size=22,colour="#FFFFFF"),
        legend.background = element_rect(fill = "#1d1e1e"),
        legend.title = element_text(size=40,color="#FFFFFF",face="bold"),
        legend.text = element_text(colour="#FFFFFF"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  transition_reveal(along = date) + view_follow()

p <- p +  guides(fill="none")# + guides(colour = guide_legend(""))
p


animate(p,nframes = 2000, fps = 25, height = 720*2, width =1280*2, renderer = av_renderer())
anim_save("GBP_dollar.mp4", animation = last_animation())



### JPY:
plotted_data <- data.frame(data.frame(df$Date,df$JPY,df$G.coin))
colnames(plotted_data) <- c("Date","JPY","G_coin")

scaleFactor <- plotted_data$JPY[1]/plotted_data$G_coin[1]

plotted_data$date <- plotted_data$Date
plotted_data$Date <- 1:nrow(plotted_data)
plotted_data$date <- as.Date(plotted_data$date,"%d/%m/%Y")
plotted_data$Dollar <- 1

plotted_data <- plotted_data[!(names(plotted_data) %in% c("Date"))]
### Scale factor for 2nd Y axis.
plotted_data$JPY <- plotted_data$JPY / scaleFactor

plotted_data <- melt(plotted_data,id.vars=c("date"),measured.vars=c("JPY","Dollar","G-coin"))



p <- ggplot(data = plotted_data, mapping = aes(x = date, y = value,group = variable, colour = variable)) +
  geom_line(size=2) +
  labs(x = "", y = "USD value", 
       title = "G-coin/ Japanese yen appreciation vs US Dollar",width = 40) +
  geom_text(aes(label = variable),size=13, vjust = 0) +
  scale_color_manual(values=c("#D1AC00", "#F47D20","#FFFFFF")) +
  scale_y_continuous(name="USD & G-coin value", sec.axis=sec_axis(~./scaleFactor, name="JPY value in dollars")) +
  theme(legend.position="none",panel.grid = element_line(colour = "#53625C",linewidth  = 0),
        plot.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"), 
        panel.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"),
        plot.title= element_text(size=40,color="#FFFFFF",face="bold",hjust = 0.5),
        axis.text.x=element_text(size=22,colour="#FFFFFF"),
        axis.text.y=element_text(size=22,colour="#FFFFFF"),
        axis.title.y = element_text(size=22,colour="#FFFFFF"),
        axis.title.x = element_text(size=22,colour="#FFFFFF"),
        legend.background = element_rect(fill = "#1d1e1e"),
        legend.title = element_text(size=40,color="#FFFFFF",face="bold"),
        legend.text = element_text(colour="#FFFFFF"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  transition_reveal(along = date) + view_follow()

p <- p +  guides(fill="none")# + guides(colour = guide_legend(""))
p



animate(p,nframes = 2000, fps = 25, height = 720*2, width =1280*2, renderer = av_renderer())
anim_save("JPY_dollar.mp4", animation = last_animation())


### CNY:
plotted_data <- data.frame(data.frame(df$Date,df$CNY,df$G.coin))
colnames(plotted_data) <- c("Date","CNY","G_coin")


plotted_data$date <- plotted_data$Date
plotted_data$Date <- 1:nrow(plotted_data)
plotted_data$date <- as.Date(plotted_data$date,"%d/%m/%Y")
plotted_data$Dollar <- 1

plotted_data <- plotted_data[!(names(plotted_data) %in% c("Date"))]
### Scale factor for 2nd Y axis.
scaleFactor <- plotted_data$CNY[1]/plotted_data$G_coin[1]
plotted_data$CNY <- plotted_data$CNY/scaleFactor
plotted_data <- melt(plotted_data,id.vars=c("date"),measured.vars=c("CNY","Dollar","G-coin"))



p <- ggplot(data = plotted_data, mapping = aes(x = date, y = value,group = variable, colour = variable)) +
  geom_line(size=2) +
  labs(x = "", y = "USD value", 
       title = "G-coin/ Chinese juan appreciation vs US Dollar",width = 40) +
  geom_text(aes(label = variable),size=13, vjust = 0) +
  scale_color_manual(values=c("#D1AC00", "#F47D20","#FFFFFF")) +
  scale_y_continuous(name="USD & G-coin value", sec.axis=sec_axis(~./scaleFactor, name="CNY value in dollars")) +
  #sec.axis = sec_axis(~.*CNY, name="Second Axis") +
  theme(legend.position="none",panel.grid = element_line(colour = "#53625C",linewidth  = 0),
        plot.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"), 
        panel.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"),
        plot.title= element_text(size=40,color="#FFFFFF",face="bold",hjust = 0.5),
        axis.text.x=element_text(size=22,colour="#FFFFFF"),
        axis.text.y=element_text(size=22,colour="#FFFFFF"),
        axis.title.y = element_text(size=22,colour="#FFFFFF"),
        axis.title.x = element_text(size=22,colour="#FFFFFF"),
        legend.background = element_rect(fill = "#1d1e1e"),
        legend.title = element_text(size=40,color="#FFFFFF",face="bold"),
        legend.text = element_text(colour="#FFFFFF"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  transition_reveal(along = date) + view_follow()

p <- p +  guides(fill="none")# + guides(colour = guide_legend(""))
p




animate(p,nframes = 2000, fps = 25, height = 720*2, width =1280*2, renderer = av_renderer())
anim_save("CNY_dollar.mp4", animation = last_animation())















#### And now the other way around (change titles):
### EUR:
plotted_data <- data.frame(data.frame(df$Date,df$EUR,df$G.coin))
colnames(plotted_data) <- c("Date","EUR","G_coin")


plotted_data$date <- plotted_data$Date
plotted_data$Date <- 1:nrow(plotted_data)
plotted_data$date <- as.Date(plotted_data$date,"%d/%m/%Y")
plotted_data$USD <- 1/plotted_data$EUR
plotted_data$G_coin <- plotted_data$G_coin/plotted_data$EUR


plotted_data$EUR <- 1

plotted_data <- plotted_data[!(names(plotted_data) %in% c("Date"))]


plotted_data <- melt(plotted_data,id.vars=c("date"),measured.vars=c("USD","EUR","G_coin"))

p <- ggplot(data = plotted_data, mapping = aes(x = date, y = value,group = variable, colour = variable)) +
  geom_line(size=2) +
  labs(x = "", y = "EUR value", 
       title = "G-coin/ USD appreciation vs EUR",width = 40) +
  geom_text(aes(label = variable),size=13, vjust = 0) +
  scale_color_manual(values=c("#D1AC00", "#F47D20","#FFFFFF")) +
  theme(legend.position="none",panel.grid = element_line(colour = "#53625C",linewidth  = 0),
        plot.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"), 
        panel.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"),
        plot.title= element_text(size=40,color="#FFFFFF",face="bold",hjust = 0.5),
        axis.text.x=element_text(size=22,colour="#FFFFFF"),
        axis.text.y=element_text(size=22,colour="#FFFFFF"),
        axis.title.y = element_text(size=22,colour="#FFFFFF"),
        axis.title.x = element_text(size=22,colour="#FFFFFF"),
        legend.background = element_rect(fill = "#1d1e1e"),
        legend.title = element_text(size=40,color="#FFFFFF",face="bold"),
        legend.text = element_text(colour="#FFFFFF"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  transition_reveal(along = date) + view_follow()

p <- p +  guides(fill="none")# + guides(colour = guide_legend(""))
p


animate(p,nframes = 2000, fps = 25, height = 720*2, width =1280*2, renderer = av_renderer())
anim_save("USD_EUR.mp4", animation = last_animation())






### GBP:
plotted_data <- data.frame(data.frame(df$Date,df$EUR,df$G.coin))
colnames(plotted_data) <- c("Date","GBP","G_coin")


plotted_data$date <- plotted_data$Date
plotted_data$Date <- 1:nrow(plotted_data)
plotted_data$date <- as.Date(plotted_data$date,"%d/%m/%Y")
plotted_data$USD <- 1/plotted_data$GBP
plotted_data$G_coin <- plotted_data$G_coin/plotted_data$GBP


plotted_data$GBP <- 1

plotted_data <- plotted_data[!(names(plotted_data) %in% c("Date"))]


plotted_data <- melt(plotted_data,id.vars=c("date"),measured.vars=c("USD","GBP","G_coin"))

p <- ggplot(data = plotted_data, mapping = aes(x = date, y = value,group = variable, colour = variable)) +
  geom_line(size=2) +
  labs(x = "", y = "GBP value", 
       title = "G-coin/ USD appreciation vs GBP",width = 40) +
  geom_text(aes(label = variable),size=13, vjust = 0) +
  scale_color_manual(values=c("#D1AC00", "#F47D20","#FFFFFF")) +
  theme(legend.position="none",panel.grid = element_line(colour = "#53625C",linewidth  = 0),
        plot.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"), 
        panel.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"),
        plot.title= element_text(size=40,color="#FFFFFF",face="bold",hjust = 0.5),
        axis.text.x=element_text(size=22,colour="#FFFFFF"),
        axis.text.y=element_text(size=22,colour="#FFFFFF"),
        axis.title.y = element_text(size=22,colour="#FFFFFF"),
        axis.title.x = element_text(size=22,colour="#FFFFFF"),
        legend.background = element_rect(fill = "#1d1e1e"),
        legend.title = element_text(size=40,color="#FFFFFF",face="bold"),
        legend.text = element_text(colour="#FFFFFF"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  transition_reveal(along = date) + view_follow()

p <- p +  guides(fill="none")# + guides(colour = guide_legend(""))
p


animate(p,nframes = 2000, fps = 25, height = 720*2, width =1280*2, renderer = av_renderer())
anim_save("USD_GBP.mp4", animation = last_animation())





### JPY:
plotted_data <- data.frame(data.frame(df$Date,df$JPY,df$G.coin))
colnames(plotted_data) <- c("Date","JPY","G_coin")

scaleFactor <- plotted_data$JPY[1]/plotted_data$G_coin[1]

plotted_data$date <- plotted_data$Date
plotted_data$Date <- 1:nrow(plotted_data)
plotted_data$date <- as.Date(plotted_data$date,"%d/%m/%Y")
plotted_data$USD <- 1/plotted_data$JPY
plotted_data$G_coin <- plotted_data$G_coin/plotted_data$JPY

#plotted_data$JPY <- 1

plotted_data <- plotted_data[!(names(plotted_data) %in% c("Date"))]
### Scale factor for 2nd Y axis.

#plotted_data$JPY <- 1
plotted_data <- plotted_data[,!names(plotted_data) %in% c("JPY")]

plotted_data <- melt(plotted_data,id.vars=c("date"),measured.vars=c("USD","G-coin"))


p <- ggplot(data = plotted_data, mapping = aes(x = date, y = value,group = variable, colour = variable)) +
  geom_line(size=2) +
  labs(x = "", y = "JPY value", 
       title = "G-coin/ USD appreciation vs Japanese Yen",width = 40) +
  geom_text(aes(label = variable),size=13, vjust = 0) +
  scale_color_manual(values=c("#D1AC00", "#F47D20")) +
  scale_y_continuous(name="JPY & G-coin value", sec.axis=sec_axis(~./scaleFactor, name="JPY value in dollars")) +
  theme(legend.position="none",panel.grid = element_line(colour = "#53625C",linewidth  = 0),
        plot.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"), 
        panel.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"),
        plot.title= element_text(size=40,color="#FFFFFF",face="bold",hjust = 0.5),
        axis.text.x=element_text(size=22,colour="#FFFFFF"),
        axis.text.y=element_text(size=22,colour="#FFFFFF"),
        axis.title.y = element_text(size=22,colour="#FFFFFF"),
        axis.title.x = element_text(size=22,colour="#FFFFFF"),
        legend.background = element_rect(fill = "#1d1e1e"),
        legend.title = element_text(size=40,color="#FFFFFF",face="bold"),
        legend.text = element_text(colour="#FFFFFF"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  transition_reveal(along = date) + view_follow()

p <- p +  guides(fill="none")# + guides(colour = guide_legend(""))
p


animate(p,nframes = 2000, fps = 25, height = 720*2, width =1280*2, renderer = av_renderer())
anim_save("USD_JPY.mp4", animation = last_animation())





### CNY:
plotted_data <- data.frame(data.frame(df$Date,df$JPY,df$G.coin))
colnames(plotted_data) <- c("Date","CNY","G_coin")

scaleFactor <- plotted_data$CNY[1]/plotted_data$G_coin[1]

plotted_data$date <- plotted_data$Date
plotted_data$Date <- 1:nrow(plotted_data)
plotted_data$date <- as.Date(plotted_data$date,"%d/%m/%Y")
plotted_data$USD <- 1/plotted_data$CNY
plotted_data$G_coin <- plotted_data$G_coin/plotted_data$CNY


plotted_data <- plotted_data[!(names(plotted_data) %in% c("Date"))]
### Scale factor for 2nd Y axis.

plotted_data <- plotted_data[,!names(plotted_data) %in% c("CNY")]

plotted_data <- melt(plotted_data,id.vars=c("date"),measured.vars=c("USD","G-coin"))



p <- ggplot(data = plotted_data, mapping = aes(x = date, y = value,group = variable, colour = variable)) +
  geom_line(size=2) +
  labs(x = "", y = "USD value", 
       title = "G-coin/ USD appreciation vs Chinese Juan",width = 40) +
  geom_text(aes(label = variable),size=13, vjust = 0) +
  scale_color_manual(values=c("#D1AC00", "#F47D20")) +
  scale_y_continuous(name="USD & G-coin value", sec.axis=sec_axis(~./scaleFactor, name="CNY value in dollars")) +
  #sec.axis = sec_axis(~.*CNY, name="Second Axis") +
  theme(legend.position="none",panel.grid = element_line(colour = "#53625C",linewidth  = 0),
        plot.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"), 
        panel.background = element_rect(fill = "#1d1e1e", colour="#1d1e1e"),
        plot.title= element_text(size=40,color="#FFFFFF",face="bold",hjust = 0.5),
        axis.text.x=element_text(size=22,colour="#FFFFFF"),
        axis.text.y=element_text(size=22,colour="#FFFFFF"),
        axis.title.y = element_text(size=22,colour="#FFFFFF"),
        axis.title.x = element_text(size=22,colour="#FFFFFF"),
        legend.background = element_rect(fill = "#1d1e1e"),
        legend.title = element_text(size=40,color="#FFFFFF",face="bold"),
        legend.text = element_text(colour="#FFFFFF"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  transition_reveal(along = date) + view_follow()

p <- p +  guides(fill="none")# + guides(colour = guide_legend(""))
p




animate(p,nframes = 2000, fps = 25, height = 720*2, width =1280*2, renderer = av_renderer())
anim_save("USD_CNY.mp4", animation = last_animation())














