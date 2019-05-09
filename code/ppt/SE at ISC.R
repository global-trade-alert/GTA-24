rm(list=ls())
library("splitstackshape")
library("xlsx")
library("foreign")
library("ggplot2")
library("scales")
library("gtable")
library("grid")
library("extrafontdb")
library("extrafont")
library("Rttf2pt1")
library("zoo")
library("gtalibrary")
library("lubridate")
library("data.table")
library("tidyverse")
library(gtalibrary)

loadfonts(device="postscript")
loadfonts(device="win")


setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
#setwd('C:/Users/Kamran/Dropbox/GTA cloud')
# setwd('D:/Dropbox/Dropbox/GTA cloud')
gta_colour_palette()
output.path="0 report production/GTA 24/tables & figures/ppt"
# Prepare simplified versions of figures 3.3 and 3.4. In one version please delete the detail of the number of interventions affecting trade. 
#   Please also prepare a version of 3.4 (but not for 3.3) where the number of hits detail is retained.

source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")

for(fig in c(3.3, 3.4)){
  
  fig3.3 <- xlsx::read.xlsx(file=paste("0 report production/GTA 24/tables & figures/3 - The Sino-US trade war - An update/Figure ",fig," - Data for Figure ",fig,".xlsx",sep=""), sheetIndex = 1)
  names(fig3.3) <- c("administration", "end.date","intervention.count", "value.total", "share.total", "value.1","value.2", "value.3", "value.4","value.5")
  fig3.3$end.date=NULL
  fig3.3.plot <- gather(fig3.3, variable, value, c(2:ncol(fig3.3)))
  
  if(fig==3.3){
    aj="China"
    aj.guy="Chinese"
    ij="US"
    ij.guy="US"
    y.max=500
  } else {
    ij="China"
    ij.guy="Chinese"
    aj="US"
    aj.guy="US"
    
    y.max=150
    
  }

  
  ## totals
  # y.max=round(max(fig3.3.plot$value)/100000000000 +.5,0)*100
  
  just.total <- ggplot()+
    geom_bar(data=subset(fig3.3.plot, variable == "value.total"), 
             aes(x=administration, y=value/1000000000), 
             stat = "identity", width=0.6,
             fill=gta_colour$blue[1]) +
    geom_line(data=subset(fig3.3.plot, variable == "share.total"), aes(x=administration, y=value*y.max, group=1),colour=gta_colour$qualitative[6],size=1.5)+
    geom_text(data=subset(fig3.3.plot, variable == "share.total"), aes(x=administration, y=value*y.max, label=round(value, digits = 3)), nudge_y = -20, size=3.5, colour="#FFFFFF")+
    scale_y_continuous(breaks=seq(0,y.max,50), limits = c(0,y.max),sec.axis = dup_axis())+
    scale_x_discrete(labels = c("Obama II", "2017", "2018", "2019"))+
    xlab("Period")+
    ylab(paste(aj.guy," exports affected\n(billion US Dollars)",sep=""))+
    gta_theme()+
    theme(axis.text.x.bottom = element_text(size=12),
          axis.title.y.left = element_text(size=12),
          axis.title.y.right = element_text(size=12)
    )
  
  just.total
  
  gta_plot_saver(plot=just.total,
                 path=output.path,
                 name=paste("Figure ",fig," - Total value of ",aj.guy," exports affected by ",ij.guy, sep=""),
                 eps=F)
  
  # ## by hit bracket, no trade share
  # just.hits <- ggplot(data=subset(fig3.3.plot, variable %in% c("value.1","value.2","value.3","value.4","value.5")), 
  #                     aes(x=administration, y=value/1000000000, fill=variable))+
  #   geom_bar(stat = "identity", width=0.6) +
  #   scale_y_continuous(breaks=seq(0,y.max,50), limits = c(0,y.max),sec.axis = dup_axis())+
  #   scale_x_discrete(labels = c("Obama II", "2017", "2018", "2019"))+
  #   scale_fill_manual(values = gta_colour$qualitative[c(5,4,3,2,1)], labels=c("1","2","3","4","5 or more"))+
  #   xlab("Period")+
  #   ylab(paste(aj.guy," exports affected\n(billion US Dollars)",sep=""))+
  #   guides(fill=guide_legend(title="Number of interventions \naffecting trade", ncol=3,title.position = "top"))+
  #   gta_theme()+
  #   theme(axis.text.x.bottom = element_text(size=12),
  #         axis.title.y.left = element_text(size=12),
  #         axis.title.y.right = element_text(size=12)
  #   )
  # 
  # just.hits
  # 
  # gta_plot_saver(plot=just.hits,
  #                path=output.path,
  #                name=paste("Figure ",fig," - Value of ",aj.guy," exports affected by ",ij.guy," x or more times", sep=""),
  #                eps=F)
  
  
}


# Please prepare a simplified version of figure 4.2.
## JF: nothing to simplify here.

# Please calculate for the latest year available the % of world trade associated with Sino US bilateral trade.
gta_trade_value_bilateral(trade.data = "2017")
total.trade=sum(trade.base.bilateral$trade.value)
chn.us=sum(subset(trade.base.bilateral, i.un==156 & a.un==840)$trade.value)
us.chn=sum(subset(trade.base.bilateral, i.un==840 & a.un==156)$trade.value)
(chn.us + us.chn)/total.trade

# Please calculate the % of the total value of exports affected by jumbo measures that are associated with the Sino-US trade war.
load("0 report production\GTA 24\data\4 - Covert jumbo protectionism is the norm\conservative jumbos 10bn.Rdata")



# It turns out there will be a Singaporean minister and a minister from Botswana on the panel.
# Starting with November 2008 as 0% please prepare a sequence of slides which show 
# (a) the % of Bostwana’s exports that confront transparent traditional trade barriers each year through to 2019, 
# (b) the % of Bostwana’s exports that confront any trade distortions each year through to 2019, 
# (c) the % of Singapore’s exports that confront any trade distortions each year through to 2019 
# (d) the % of World exports that confront any trade distortions each year through to 2019

source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")
traditional.barrier=int.mast.types$intervention.type[int.mast.types$is.murky==0]
gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                   exporters = "Botswana",
                   keep.exporters = T,
                   intervention.types = traditional.barrier,
                   keep.type = T)

trade.coverage=trade.coverage.estimates
xlsx::write.xlsx(trade.coverage, file=paste(output.path, "/Traditional barriers on Botswana exports.xlsx",sep=""), row.names = F)

## All barriers SGP & BOT
gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                   exporters = c("Botswana","Singapore"),
                   keep.exporters = T,
                   group.exporters = F)

xlsx::write.xlsx(trade.coverage.estimates, file=paste(output.path, "/All barriers on Botswana & Singaporean exports.xlsx",sep=""), row.names = F)
trade.coverage=trade.coverage.estimates
trade.coverage=trade.coverage[,c(2,4:ncol(trade.coverage))]
names(trade.coverage)=c("exporter",c(2009:2019))
tc.plot <- gather(trade.coverage, year, share, c(2:ncol(trade.coverage)))
tc.plot$year=as.numeric(tc.plot$year)
tc.plot=rbind(tc.plot,
              data.frame(exporter=c("Botswana","Singapore"),
                         year=2008,
                         share=0))

tc.2.cty= ggplot(tc.plot, aes(x=year, y=share, color=as.factor(exporter)))+
  geom_line(size=2)+
  scale_color_manual(values=c(gta_colour$qualitative[c(1,7)]))+
  scale_x_continuous(limit=c(2008,2019),breaks=seq(2008,2019,1), labels = c("Nov.\n2008",2009:2019))+
  scale_y_continuous(limit=c(0,1),breaks=seq(0,1,.2), sec.axis = dup_axis())+
  labs(x="", y="Share of total exports harmed", color="")+
  guides(fill=element_blank())+
  gta_theme(x.bottom.angle = 90)+
  theme(axis.text.x.bottom = element_text(vjust = 0.5, size=18),
          axis.text.y.left = element_text(size=18),
          axis.text.y.right = element_text(size=18),
          axis.title.y.left = element_text(size=20),
          axis.title.y.right = element_text(size=20),
          axis.title.x.bottom = element_text(size=20),
          legend.text = element_text(size=20))

  tc.2.cty
  
  gta_plot_saver(plot=tc.2.cty,
                 path=output.path,
                 name="Trade coverage - Botswana & Singapore - 2009 - 2019",
                 eps=F)


## All barriers WORLD
gta_trade_coverage(gta.evaluation = c("Red","Amber"))
xlsx::write.xlsx(trade.coverage.estimates, file=paste(output.path, "/All barriers on World exports.xlsx",sep=""), row.names = F)

trade.coverage.world=trade.coverage.estimates
trade.coverage.world=trade.coverage.world[,c(2,4:ncol(trade.coverage.world))]
names(trade.coverage.world)=c("exporter",c(2009:2019))
trade.coverage.world$exporter="World"
tcw.plot <- gather(trade.coverage.world, year, share, c(2:ncol(trade.coverage)))
tcw.plot$year=as.numeric(tcw.plot$year)
tcw.plot=rbind(tcw.plot,
              data.frame(exporter=c("World"),
                         year=2008,
                         share=0))

tc.2.cty= ggplot(tcw.plot, aes(x=year, y=share, color=as.factor(exporter)))+
  geom_line(size=2)+
  scale_color_manual(values=c(gta_colour$red[2]))+
  scale_x_continuous(limit=c(2008,2019),breaks=seq(2008,2019,1), labels = c("Nov.\n2008",2009:2019))+
  scale_y_continuous(limit=c(0,1),breaks=seq(0,1,.2), sec.axis = dup_axis())+
  labs(x="", y="Share of total exports harmed", color="")+
guides(fill=element_blank())+
  gta_theme(x.bottom.angle = 90)+
  theme(axis.text.x.bottom = element_text(vjust = 0.5, size=18),
        axis.text.y.left = element_text(size=18),
        axis.text.y.right = element_text(size=18),
        axis.title.y.left = element_text(size=20),
        axis.title.y.right = element_text(size=20),
        axis.title.x.bottom = element_text(size=20),
        legend.text = element_text(size=20))

tc.2.cty

gta_plot_saver(plot=tc.2.cty,
               path=output.path,
               name="Trade coverage - World - 2009 - 2019",
               eps=F)


## WORLD + 2 countries
total.plot=rbind(tc.plot, tcw.plot)

tc.2.cty= ggplot(total.plot, aes(x=year, y=share, color=as.factor(exporter)))+
  geom_line(size=2)+
  scale_color_manual(values=c(gta_colour$qualitative[c(1,7)],gta_colour$red[2]))+
  scale_x_continuous(limit=c(2008,2019),breaks=seq(2008,2019,1), labels = c("Nov.\n2008",2009:2019))+
  scale_y_continuous(limit=c(0,1),breaks=seq(0,1,.2), sec.axis = dup_axis())+
  labs(x="", y="Share of total exports harmed", color="")+
guides(fill=element_blank())+
  gta_theme(x.bottom.angle = 90)+
  theme(axis.text.x.bottom = element_text(vjust = 0.5, size=18),
        axis.text.y.left = element_text(size=18),
        axis.text.y.right = element_text(size=18),
        axis.title.y.left = element_text(size=20),
        axis.title.y.right = element_text(size=20),
        axis.title.x.bottom = element_text(size=20),
        legend.text = element_text(size=20))

tc.2.cty

gta_plot_saver(plot=tc.2.cty,
               path=output.path,
               name="Trade coverage - World & Botswana & Singapore - 2009 - 2019",
               eps=F)



# Then prepare the slide so that (a) appears first, then (a) and (b) together, then drop (a) so that (b) and (c) are on the slide together and then (b), (c), and (d) are on the same chart. Please be sure to make each line on these charts start explicitly at 0 in November 2008.

# Please prepare a staggered bar chart showing the scale of the Sino US trade war. 
# I have in mind a slide which on the far right has a column whose height reflects the total amount of bilateral Sino US exports in 2018. 

# With this chart I can discuss how large the tariff war really is.






# The fourth column from the left include add our estimates of the amounts of trade affected 
# by non-targeted-tariff increases for 2018 on top of the Chinese and US tariff increases in 2018.

gta_trade_coverage(coverage.period=c(2018,2018),
                   gta.evaluation = c("Red", "Amber"),
                   affected.flows = "inward",
                   implementers=c("China","United States of America"),
                   keep.implementer = T,
                   exporters = c("China","United States of America"),
                   keep.exporters = T,
                   group.exporters = F,
                   intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                   keep.type = T,
                   implementation.period = c("2018-01-01","2018-12-31"),
                   trade.data="2017",
                   trade.statistic = "value",
                   intra.year.duration = F,
                   nr.exporters = c(2,999))

other.stuff=sum(trade.coverage.estimates[,4])
us.stuff=trade.coverage.estimates[which(trade.coverage.estimates$`Exporting country`=="China"),4]

gta_trade_coverage(coverage.period=c(2018,2018),
                   gta.evaluation = c("Red", "Amber"),
                   affected.flows = "inward",
                   implementers=c("China","United States of America"),
                   keep.implementer = T,
                   exporters = c("China","United States of America"),
                   keep.exporters = T,
                   group.exporters = F,
                   intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                   keep.type = F,
                   implementation.period = c("2018-01-01","2018-12-31"),
                   trade.data="2017",
                   trade.statistic = "value",
                   intra.year.duration = F,
                   nr.exporters = c(2,999))

other.stuff=other.stuff+sum(trade.coverage.estimates[,4])
us.stuff=us.stuff+trade.coverage.estimates[which(trade.coverage.estimates$`Exporting country`=="China"),4]

gta_trade_coverage(coverage.period=c(2018,2018),
                   gta.evaluation = c("Red", "Amber"),
                   affected.flows = "inward",
                   implementers=c("China","United States of America"),
                   keep.implementer = T,
                   exporters = c("China","United States of America"),
                   keep.exporters = T,
                   group.exporters = F,
                   intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                   keep.type = F,
                   implementation.period = c("2018-01-01","2018-12-31"),
                   trade.data="2017",
                   trade.statistic = "value",
                   intra.year.duration = F,
                   nr.exporters = c(1,1))

other.stuff=other.stuff+sum(trade.coverage.estimates[,4])
us.stuff=us.stuff+trade.coverage.estimates[which(trade.coverage.estimates$`Exporting country`=="China"),4]

# Then the first left hand column would have the Trump tariff increases of 2018, 
gta_trade_coverage(coverage.period=c(2018,2018),
                   intervention.ids = trade.war.us,
                   keep.interventions = T,
                   exporters = "China",
                   keep.exporters = T,
                   trade.data="2017",
                   trade.statistic = "value",
                   intra.year.duration = F)

us.2018=trade.coverage.estimates[,4]



# second the next column on the left would add the Chinese tariff increases and 
gta_trade_coverage(coverage.period=c(2018,2018),
                   intervention.ids = trade.war.chn,
                   keep.interventions = T,
                   exporters = "United States of America",
                   keep.exporters = T,
                   trade.data="2017",
                   trade.statistic = "value",
                   intra.year.duration = F)

chn.2018=trade.coverage.estimates[,4]



# third column from the left would add Trump’s very recent tariff threats on top. 
# tweet.war=63051
# gta_trade_coverage(coverage.period=c(2018,2018),
#                    intervention.ids = tweet.war,
#                    keep.interventions = T,
#                    exporters = "China",
#                    keep.exporters = T,
#                    trade.data="2017",
#                    trade.statistic = "value",
#                    intra.year.duration = F)




## total bilateral trade

war.perspective=data.frame(act.title="US\ntariffs\n2018",
                           value=us.2018)

war.perspective=rbind(war.perspective,
                      data.frame(act.title="China\ntariffs\n2018",
                                 value=chn.2018))

war.perspective=rbind(war.perspective,
                      data.frame(act.title="Other\nUS/CHN\ninterventions\nin 2018",
                                 value=other.stuff)
)

war.perspective=rbind(war.perspective,
                      data.frame(act.title="Trump\nthreat\nMay\n2019",
                                 value=us.chn-us.2018))
war.perspective=rbind(war.perspective,
                      data.frame(act.title="Total\nbilateral\ntrade",
                           value=chn.us+us.chn))



## exciting waterfall version
## based on https://learnr.wordpress.com/2010/05/10/ggplot2-waterfall-charts/
war.terfall=war.perspective
war.terfall$value=round(war.terfall$value/1000000000,2)
war.terfall$act.title=factor(war.terfall$act.title, levels=war.terfall$act.title)
war.terfall$id=1:nrow(war.terfall)
war.terfall$end=cumsum(war.terfall$value)
war.terfall$end[nrow(war.terfall)]=war.terfall$value[nrow(war.terfall)]
war.terfall$end[war.terfall$act.title=="Trump\nthreat\nMay\n2019"]=war.terfall$end[war.terfall$act.title=="Trump\nthreat\nMay\n2019"]-us.stuff/1000000000
war.terfall$value[nrow(war.terfall)]=war.terfall$end[nrow(war.terfall)]-war.terfall$end[nrow(war.terfall)-1]
war.terfall$start <- c(0, head(war.terfall$end, -1))


war.terfall$start[war.terfall$act.title=="Trump\nthreat\nMay\n2019"]=war.terfall$start[war.terfall$act.title=="Trump\nthreat\nMay\n2019"]-us.stuff/1000000000
war.terfall=war.terfall[,c(3,1,5,4,2)]

war.terfall.xlsx=war.terfall
names(war.terfall.xlsx)=c("ID", "Action", "Starting Value", "Ending value","Change")
xlsx::write.xlsx(war.terfall.xlsx, file=paste(output.path, "/Trade war interventions in bilateral perspective.xlsx",sep=""), row.names=F)


#### classic waterfall
warter.shed=ggplot(war.terfall, aes(act.title, fill = act.title)) + 
  geom_rect(aes(x = act.title, 
                xmin = id - 0.45, xmax = id + 0.45, 
                ymin = end, ymax = start))+
  scale_fill_manual(values=c(gta_colour$qualitative[c(1,7,5,3,8)]))+
  scale_y_continuous(limit=c(0,700),breaks=seq(0,700,100), sec.axis = dup_axis())+
  labs(x="", y="USD billion", color="")+
  gta_theme(x.bottom.angle = 0)+
  theme(axis.text.x.bottom = element_text(vjust = 0.5, size=18),
        axis.text.y.left = element_text(size=18),
        axis.text.y.right = element_text(size=18),
        axis.title.y.left = element_text(size=16),
        axis.title.y.right = element_text(size=16),
        axis.title.x.bottom = element_text(size=18),
        legend.position="none")

gta_plot_saver(plot=warter.shed,
               path=output.path,
               name="Trade war in bilateral perspective - waterfall chart",
               eps=F)

### with full right-hand bar
war.terfall$start[nrow(war.terfall)]=0

warter.shed=ggplot(war.terfall, aes(act.title, fill = act.title)) + 
  geom_rect(aes(x = act.title, 
                xmin = id - 0.45, xmax = id + 0.45, 
                ymin = end, ymax = start))+
  scale_fill_manual(values=c(gta_colour$qualitative[c(1,7,5,3,8)]))+
  scale_y_continuous(limit=c(0,700),breaks=seq(0,700,100), sec.axis = dup_axis())+
  labs(x="", y="USD billion", color="")+
  gta_theme(x.bottom.angle = 0)+
  theme(axis.text.x.bottom = element_text(vjust = 0.5, size=18),
        axis.text.y.left = element_text(size=18),
        axis.text.y.right = element_text(size=18),
        axis.title.y.left = element_text(size=16),
        axis.title.y.right = element_text(size=16),
        axis.title.x.bottom = element_text(size=18),
        legend.position="none")

gta_plot_saver(plot=warter.shed,
               path=output.path,
               name="Trade war in bilateral perspective - waterfall chart with full rhs bar",
               eps=F)


## boring bar plot version
war.picture=ggplot(war.perspective, aes(x = factor(act.title), y = round(value/1000000000,0), fill=act.title)) +
  geom_col() + 
  geom_text(aes(y = round(value/1000000000,0) + 30, label = round(value/1000000000,0)), size=5,position = position_dodge(w = -0.5))+
  scale_fill_manual(values=c(gta_colour$qualitative[c(1,7,5,3,8)]))+
  scale_y_continuous(limit=c(0,750),breaks=seq(0,700,100), sec.axis = dup_axis())+
  labs(x="", y="USD billion", color="")+
  gta_theme(x.bottom.angle = 0)+
  theme(axis.text.x.bottom = element_text(vjust = 0.5, size=18),
        axis.text.y.left = element_text(size=18),
        axis.text.y.right = element_text(size=18),
        axis.title.y.left = element_text(size=16),
        axis.title.y.right = element_text(size=16),
        axis.title.x.bottom = element_text(size=18),
        legend.position="none")


gta_plot_saver(plot=war.picture,
               path=output.path,
               name="Trade war interventions in US-CHN bilateral perspective",
               eps=F)
