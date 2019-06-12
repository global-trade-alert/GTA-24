rm(list=ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(gtalibrary)
library(ggplot2)

gta_setwd()

source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")
gta_colour_palette()

chapter.number = 0
chapter.title = 'Report opening plots'
output.path = paste(chapter.number,chapter.title,sep = ' - ')

d.path="0 report production/GTA 24/0 GTA database at production/master_plus.Rdata"
r.path="0 report production/GTA 24/0 GTA database at production/database replica - parts - base.Rdata"


## Share of world import affected, targeted vs non-targeted
gta_trade_coverage(
  gta.evaluation = c("red","amber"),
  intervention.ids = manually.removed.interventions,
  keep.interventions = F,
  data.path = d.path,
  replica.path =r.path
)

chart1=trade.coverage.estimates
chart1$version="All trade discrimination"
chart2=chart1

gta_trade_coverage(
  gta.evaluation = c("red","amber"),
  nr.exporters=c(1,1),
  intervention.ids = manually.removed.interventions,
  keep.interventions = F,
  data.path = d.path,
  replica.path =r.path
)
trade.coverage.estimates$version="Discrimination that singles out one exporter"
chart1=rbind(chart1, trade.coverage.estimates)


names(chart1)=gsub("Trade coverage estimate for ","",names(chart1))
chart1.plot <- gather(chart1[,c(4:15)], year, value, 1:11)
chart1.plot=rbind(chart1.plot, 
                  data.frame(version=unique(chart1.plot$version),
                             year=2008,
                             value=0,
                             stringsAsFactors = F))


chart1.plot$position=chart1.plot$value+.05

plot1=
  ggplot(chart1.plot, aes(x=year, y=value, colour=version, group=version))+  
  geom_line(size=1.2)+
  geom_text(aes(x=year, y=position, label=round(value,3)), colour="black", size=3, nudge_x = -.1)+
  scale_y_continuous(limits=c(0,1), sec.axis = dup_axis())+
  scale_colour_manual(values=c(gta_colour$qualitative[c(1,8)]))+
  gta_theme()+
  labs(x="",y="Share of world trade\nimplicated in a given year", colour="")+
  guides(colour=guide_legend(nrow=2))


  plot1

gta_plot_saver(plot=plot1,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".1", sep=""))





## Chart 2:
gta_trade_coverage(
  gta.evaluation = c("red","amber"),
  mast.chapters = c("L","P","TARIFF"),
  keep.mast=T,
  group.mast=F,
  intervention.ids = manually.removed.interventions,
  keep.interventions = F,
  data.path = d.path,
  replica.path =r.path
)

trade.coverage.estimates$version=NA
trade.coverage.estimates$version[trade.coverage.estimates$`MAST chapter ID`=="P"]="Export-related measures (including export subsidies)"
trade.coverage.estimates$version[trade.coverage.estimates$`MAST chapter ID`=="L"]="Subsidies (excluding export subsidies)"
trade.coverage.estimates$version[trade.coverage.estimates$`MAST chapter ID`=="TARIFF"]="Import tariff increases"

trade.coverage.estimates=trade.coverage.estimates[,names(chart2)]

chart2=rbind(chart2, subset(trade.coverage.estimates, is.na(version)==F))
names(chart2)=gsub("Trade coverage estimate for ","",names(chart2))
chart2.plot <- gather(chart2[,c(4:15)], year, value, 1:11)
chart2.plot=rbind(chart2.plot, 
                  data.frame(version=unique(chart2.plot$version),
                             year=2008,
                             value=0,
                             stringsAsFactors = F))

chart2.plot$position=chart2.plot$value+.05
chart2.plot$position[chart2$version=="Export-related measures (including export subsidies)"]=chart2.plot$value[chart2$version=="Export-related measures (including export subsidies)"]-.05
chart2.plot$position[chart2$version=="Import tariff increases"]=chart2.plot$value[chart2$version=="Import tariff increases"]+.03

plot2=
  ggplot(chart2.plot, aes(x=year, y=value, colour=version, group=version))+  
  geom_line(size=1.2)+
  geom_text(aes(x=year, y=position, label=round(value,3)), colour="black", size=3, nudge_x = .1)+
  scale_y_continuous(limits=c(0,1), sec.axis = dup_axis())+
  scale_colour_manual(values=c(gta_colour$qualitative[c(1,4,6,7)]))+
  gta_theme()+
  labs(x="",y="Share of world trade affected\nin a given year by discrimination in force", colour="")+
  guides(colour=guide_legend(nrow=2))


plot2

gta_plot_saver(plot=plot2,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".2", sep=""))

