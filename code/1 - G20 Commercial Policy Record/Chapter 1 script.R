rm(list=ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(gtalibrary)
library(ggplot2)

#setwd(""C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
#setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
#setwd("/Users/piotrlukaszuk/Dropbox/GTA cloud")
#setwd('C:/Users/Kamran/Dropbox/GTA cloud')
setwd('D:/Dropbox/Dropbox/GTA cloud')

mast.descriptions = gtalibrary::int.mast.types

gta_colour_palette()

chapter.number = 1
chapter.title = 'G20 Commercial Policy Record'
output.path = paste(chapter.number,chapter.title,sep = ' - ')

gta.evaluation = c('Red', 'Amber')

year.list = list(year1  <- c(ymd('2014-12-01'), ymd('2015-04-15')),
          year2  <- c(ymd('2015-12-01'), ymd('2016-04-15')),
          year3  <- c(ymd('2016-12-01'), ymd('2017-04-15')),
          year4  <- c(ymd('2017-12-01'), ymd('2018-04-15')),
          year5  <- c(ymd('2018-12-01'), ymd('2019-04-15')))

periods = c('01/12/14-15/04/15','01/12/15-15/04/16', '01/12/16-15/04/17', 
                  '01/12/17-15/04/18', '01/12/18-15/04/19')

period.labels = c('01/12/14-\n15/04/15','01/12/15-\n15/04/16', '01/12/16-\n15/04/17', 
                  '01/12/17-\n15/04/18', '01/12/18-\n15/04/19')


# 1 -----------------------------------------------------------------------
# a -----------------------------------------------------------------------
# Simon's request: In each of these five years I am interested in the (a) total number of G20 amber and red implemented measures

total.implemented.harmful.measures = c()

for (year in 1:length(year.list)){
  
  r.period=c(year.list[[year]])
  r.period[1]="2008-11-01"
  
  gta_data_slicer(gta.evaluation= gta.evaluation,
                    implementing.country = 'G20',
                    keep.implementation.na = F,
                    implementation.period = c(year.list[[year]]),
                    reporting.period = r.period)
  
  total.implemented.harmful.measures[year] = length(unique(master.sliced$intervention.id))

}

plotting.data = data.frame(periods = 1:5, periods = periods, total.implemented.harmful.measures)

#US
US.implemented.harmful.measures = c()
for (year in 1:length(year.list)){
  
  r.period=c(year.list[[year]])
  r.period[1]="2008-11-01"
  
  gta_data_slicer(gta.evaluation= gta.evaluation,
                  implementing.country = 'United States of America',
                  keep.implementation.na = F,
                  implementation.period = c(year.list[[year]]),
                  reporting.period = r.period)
  
  US.implemented.harmful.measures[year] = length(unique(master.sliced$intervention.id))
  
}

plotting.data$US.implemented.harmful.measures = US.implemented.harmful.measures

#China 
china.implemented.harmful.measures = c()
for (year in 1:length(year.list)){
  
  r.period=c(year.list[[year]])
  r.period[1]="2008-11-01"
  
  gta_data_slicer(gta.evaluation= gta.evaluation,
                  implementing.country = 'China',
                  keep.implementation.na = F,
                  implementation.period = c(year.list[[year]]),
                  reporting.period = r.period)
  
  china.implemented.harmful.measures[year] = length(unique(master.sliced$intervention.id))
  
}

plotting.data$china.implemented.harmful.measures = china.implemented.harmful.measures

table.fig.1 = plotting.data[,c('periods.1','total.implemented.harmful.measures','US.implemented.harmful.measures','china.implemented.harmful.measures')]
names(table.fig.1) = c('Period', 'Total Implemented Harmful measures', 'US Implemented Harmful measures', 'China Implemented Harmful Measures')
xlsx::write.xlsx(table.fig.1, row.names=FALSE, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ",chapter.number,".1 - Data.xlsx", sep=""))



plot.6.2.a = ggplot(plotting.data,aes(x = periods, y = total.implemented.harmful.measures)) + 
  geom_line(colour=gta_colour$harmful[1], size=1.2) + 
  geom_point(colour= gta_colour$harmful[1], size=3) +
  ylab('Number of G20 implemented harmful interventions') +
  xlab('Period') + ylim(c(0, 300)) + 
  scale_x_continuous(breaks = plotting.data$periods,labels=period.labels) + gta_theme() +
  scale_y_continuous(sec.axis = dup_axis(), limits=c(0,300))

plot.6.2.a

gta_plot_saver(plot=plot.6.2.a,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name="Figure 1.1 - Number of harmful G20 implemented measures")




# b -----------------------------------------------------------------------
# Simon's request: In each of these five years I am interested in the  (b) the share of amber and red implemented measures by the G20 in all measures implemented by the G20

share.implemented.harmful.measures = c()

for (year in 1:length(year.list)){
  r.period=c(year.list[[year]])
  r.period[1]="2008-11-01"
  
  gta_data_slicer(gta.evaluation= gta.evaluation,
                  implementing.country = 'G20',
                  keep.implementation.na = F,
                  implementation.period = c(year.list[[year]]),
                  reporting.period = r.period)
  
  temp = master.sliced
  
  gta_data_slicer(gta.evaluation= c('Red', 'Amber', 'Green'),
                  implementing.country = 'G20',
                  keep.implementation.na = F,
                  implementation.period = c(year.list[[year]]),
                  reporting.period = r.period)
  
  share.implemented.harmful.measures[year] = length(unique(temp$intervention.id))/length(unique(master.sliced$intervention.id))

}

plotting.data$share.implemented.harmful.measures = share.implemented.harmful.measures

table.fig.2 = plotting.data[,c('periods','share.implemented.harmful.measures')]
names(table.fig.2) = c('Period', 'Share Implemented Harmful measures')
xlsx::write.xlsx(table.fig.2, row.names=FALSE, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ", chapter.number,".2 - Data.xlsx", sep=""))

# Simon's request: A line chart for (a) and (b) should be prepared.

plot.6.2.b = ggplot(plotting.data,aes(x=periods, y=share.implemented.harmful.measures*100)) + 
  geom_line(colour=gta_colour$harmful[1], size=1.2) + 
  geom_point(colour=gta_colour$harmful[1], size=3) +
  xlab('Period') +  ylab('Percentage of G20 implemented measures which are harmful') + 
  scale_x_continuous(breaks = plotting.data$periods,labels=period.labels) + 
  scale_y_continuous(breaks = seq(0,100,10),labels=paste0(seq(0,100,10),'%'), limits = (c(60,100)),sec.axis = dup_axis()) + gta_theme() 


plot.6.2.b


gta_plot_saver(plot=plot.6.2.b,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name="Figure 1.2 - Share of harmful G20 implemented measures")





# c -----------------------------------------------------------------------
# Simon's request: In each of these five years I am interested in the  (c) as the top five harmful policy instruments used by the G20 (and the other)

g20.members=c(32, 36, 76, 124, 156, 251, 276, 699, 360, 381, 392, 484, 410, 643, 682, 710, 792, 826, 840)
country.groups=list('G20' = g20.members)

official.source.base = read_csv("data/database replica/gta_measure.csv")
official.source.vector = subset(official.source.base, is_source_official == 1)$'title'


rnd=1
while(rnd<=2){
  g20.implemented.harmful.measures.policies = data.frame()
  master.sliced.titles = vector()
  # official.source.ratio = vector()
  #get most frequent policy instruments over the 5 years
  for (year in 1:length(year.list)){
    r.period=c(year.list[[year]])
    r.period[1]="2008-11-01"
    
    gta_data_slicer(gta.evaluation= gta.evaluation,
                    implementing.country = 'G20',
                    keep.implementer = rnd==1,
                    keep.implementation.na = F,
                    implementation.period = c(year.list[[year]]),
                    reporting.period = r.period)
    master.sliced.titles = c(master.sliced.titles,as.character(master.sliced$title))
    
    
    g20.policies = master.sliced[,colnames(master.sliced) %in% c('mast.chapter','intervention.id')]
    g20.policies = g20.policies[!duplicated(g20.policies),]
    g20.policies.chapters = g20.policies %>% dplyr::count(mast.chapter) 
    g20.policies.chapters$period = year
    
    g20.implemented.harmful.measures.policies = rbind(g20.implemented.harmful.measures.policies, g20.policies.chapters)
    
    
    
  }
  
  if (rnd == 1){
    assign('G20.official.source.ratio',(length(unique(master.sliced.titles))-length(setdiff(master.sliced.titles,official.source.vector)))/length(unique(master.sliced.titles)))
  } 
  
  if (rnd == 2) {
    assign('nonG20.official.source.ratio',(length(unique(master.sliced.titles))-length(setdiff(master.sliced.titles,official.source.vector)))/length(unique(master.sliced.titles)))
  }

  top5.frequent.policies = g20.implemented.harmful.measures.policies %>% group_by(mast.chapter) %>% summarise(most.frequent = sum(n)) %>% dplyr::top_n(5)
  top5.frequent.policies = top5.frequent.policies$mast.chapter
  
  g20.implemented.harmful.measures.policies = data.frame()
  
  for (year in 1:length(year.list)){
    r.period=c(year.list[[year]])
    r.period[1]="2008-11-01"
    
    
    gta_data_slicer(gta.evaluation= gta.evaluation,
                    implementing.country = 'G20',
                    keep.implementer = rnd==1,
                    keep.implementation.na = F,
                    implementation.period = c(year.list[[year]]),
                    reporting.period = r.period)
    
    g20.policies = master.sliced[,colnames(master.sliced) %in% c('mast.chapter','intervention.id')]
    g20.policies = g20.policies[!duplicated(g20.policies),]
    g20.policies.chapters = g20.policies %>% dplyr::count(mast.chapter) %>% filter(mast.chapter %in% top5.frequent.policies)
    g20.policies.others = g20.policies[!(g20.policies$mast.chapter %in% g20.policies.chapters$mast.chapter),]
    g20.policies.others = g20.policies.others[!duplicated(g20.policies.others),]
    g20.policies.chapters = rbind(g20.policies.chapters, data.frame(mast.chapter = 'Others', n = nrow(g20.policies.others)))
    g20.policies.chapters$period = year
    
    g20.implemented.harmful.measures.policies = rbind(g20.implemented.harmful.measures.policies, g20.policies.chapters)
    
  }
  
  
  
  setnames(g20.implemented.harmful.measures.policies, "mast.chapter", "mast.chapter.id")
  
  g20.implemented.harmful.measures.policies=merge(g20.implemented.harmful.measures.policies, 
                                                  unique(mast.descriptions[,c("mast.chapter.id","mast.chapter.name")]),
                                                  by="mast.chapter.id", all.x=T)
  
  g20.implemented.harmful.measures.policies$mast.chapter.name=as.character(g20.implemented.harmful.measures.policies$mast.chapter.name)
  g20.implemented.harmful.measures.policies$mast.chapter.name[is.na(g20.implemented.harmful.measures.policies$mast.chapter.name)]="Others"
  
  setnames(g20.implemented.harmful.measures.policies, "mast.chapter.id", "mast.chapter")
  
  fig3.xlsx=g20.implemented.harmful.measures.policies
  fig3.xlsx=reshape(fig3.xlsx, timevar = "period", idvar=c("mast.chapter","mast.chapter.name"), direction="wide")
  
  fig3.xlsx=fig3.xlsx[,c("mast.chapter","mast.chapter.name",paste("n.",1:5,sep=""))]
  names(fig3.xlsx)=c("mast.chapter","mast.chapter.name", period.labels)
  
  if(rnd==1){
  fig3.1.xlsx = fig3.xlsx
  data.plot1 = g20.implemented.harmful.measures.policies
  
  }
  ### producing output
  if(rnd==2){
    colors = gta_colour$qualitative[c(1,2,3,7,8,5,4)]
    names(colors) = unique(c(data.plot1$mast.chapter.name,g20.implemented.harmful.measures.policies$mast.chapter.name))
    
    xlsx::write.xlsx(fig3.1.xlsx, 
                     row.names=FALSE,
                     file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ", chapter.number,".3 - Data G20.xlsx", sep=""))
    
    
    colors.1 = colors[names(colors) %in% data.plot1$mast.chapter.name]
    colors.1 = colors.1[order(names(colors.1))]
    colors.1 = c(colors.1[!(names(colors.1) %in% c('Others'))],colors.1[(names(colors.1) %in% c('Others'))])
    colors.2 = colors[names(colors) %in% g20.implemented.harmful.measures.policies$mast.chapter.name]
    colors.2 = colors.2[order(names(colors.2))]
    colors.2 = c(colors.2[!(names(colors.2) %in% c('Others'))],colors.2[(names(colors.2) %in% c('Others'))])
    
    
    
    plot.6.2.c.1 = ggplot(data = data.plot1, aes(x=period, y = n, fill=mast.chapter.name)) + 
      geom_col(position='stack') + 
      scale_fill_manual(name='', values = colors.1,labels=names(colors.1)) + 
      xlab('Period') + 
      gta_theme() +
      ylab('Number of harmful policy instruments\nimplemented by G20') + 
      scale_x_continuous(breaks = 1:5,labels=period.labels) +
      theme(legend.text = element_text(size=8)) +
      scale_y_continuous(sec.axis = dup_axis())
    
    
    
    plot.6.2.c.1
    gta_plot_saver(plot=plot.6.2.c.1,
                   path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
                   name="Figure 1.3 - Top 5 harmful policy instruments implemented by G20")
  
    ## 2
    xlsx::write.xlsx(fig3.xlsx, 
                     row.names=FALSE,
                     file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ", chapter.number,".3 - Data non-G20.xlsx", sep=""))
    
    plot.6.2.c = ggplot(data = g20.implemented.harmful.measures.policies, aes(x=period, y = n, fill=mast.chapter.name)) + 
      geom_col(position='stack') + 
      scale_fill_manual(name='', values = colors.2,labels=names(colors.2))+ 
      xlab('Period') + 
      gta_theme() +
      ylab('Number of harmful policy instruments\nimplemented by non-G20') + 
      scale_x_continuous(breaks = 1:5,labels=period.labels) +
      theme(legend.text = element_text(size=8)) +
      scale_y_continuous(sec.axis = dup_axis())
    
    
    plot.6.2.c
    gta_plot_saver(plot=plot.6.2.c,
                   path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
                   name="Figure 1.3 - Top 5 harmful policy instruments implemented by non-G20")
    
    
  }
  
  
  rnd=rnd+1
  print(rnd)

  
}

official.source.ratio = data.frame('G20.official.source.ratio' = G20.official.source.ratio,
                                   'nonG20.official.source.ratio' = nonG20.official.source.ratio)

xlsx::write.xlsx(official.source.ratio,row.names = F, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Table 1.3 - Official source ratios.xlsx", sep=""))


# d -----------------------------------------------------------------------

# Simon's request: In each of these five years I am interested in the  
# (d) the total amount of trade covered by the G20 harmful measures.

value.total=data.frame()
value.firm.specific=data.frame()
value.per.intervention=data.frame()
gta.evaluation=c("Red","Amber")

for (year in 1:length(year.list)){
  
  
  
  r.year=as.character(min(year(year.list[[year]])))
  c.period=max(year(year.list[[year]]))
  if(r.year>2017){r.year="2017"}
  
  r.period=c(year.list[[year]])
  r.period[1]="2008-11-01"
  
  
  ## total
  # gta_trade_coverage(gta.evaluation = gta.evaluation,
  #                    implementers = 'G20',
  #                    keep.implementer = T,
  #                    reporting.period = r.period,
  #                    coverage.period = c(c.period,c.period),
  #                    implementation.period = year.list[[year]],
  #                    trade.statistic = "value",
  #                    trade.data = r.year, 
  #                    intervention.ids = c(70350, 18891, 16819, 71578, 58794, 18254, 13633, 15366, 13512, 18892), #The huge Indian intervention with budget of only 87 million USD
  #                    keep.interventions = F
  # )
  # 
  # value.total=rbind(value.total,
  #                   data.frame(period=paste(year.list[[year]], collapse=" - "),
  #                              trade.value=trade.coverage.estimates[,4]))
  
  ## firm-specific
  gta_trade_coverage(gta.evaluation = gta.evaluation,
                     implementers = 'G20',
                     keep.implementer = T,
                     reporting.period = r.period,
                     coverage.period = c(c.period,c.period),
                     implementation.period = year.list[[year]],
                     trade.statistic = "value",
                     trade.data = r.year, 
                     eligible.firms = "firm-specific", keep.firms = T,
                     intervention.ids = c(70350, 18891, 16819, 71578, 58794, 18254, 13633, 15366, 13512, 18892), #The huge Indian intervention with budget of only 87 million USD
                     keep.interventions = F
  )
  
  value.firm.specific=rbind(value.firm.specific,
                    data.frame(period=paste(year.list[[year]], collapse=" - "),
                               trade.value=trade.coverage.estimates[,4]))
  
  
  ## intervention-by-intervention
  # 
  # gta_data_slicer(gta.evaluation = gta.evaluation,
  #                 implementing.country = 'G20',
  #                 keep.implementer = T,
  #                 reporting.period = r.period,
  #                 implementation.period = year.list[[year]],
  #                 keep.implementation.na = F, 
  #                 intervention.ids = c(70350), #The huge Indian intervention with budget of only 87 million USD
  #                 keep.interventions = F
  # )
  # 
  # check.int=unique(master.sliced$intervention.id)
  # ms.base=master.sliced
  # 
  # for(int in check.int){
  #   tryCatch(gta_trade_coverage(intervention.ids = int,
  #                               keep.interventions = T,
  #                               coverage.period = c(c.period, c.period),
  #                               trade.statistic = "value",
  #                               trade.data = r.year))
  #   
  #   if(nrow(trade.coverage.estimates)==0){
  #     
  #     tv=0
  #     
  #   } else{
  #     tv=trade.coverage.estimates[,4]
  #     
  #   }
  #   
  #   value.per.intervention=rbind(value.per.intervention,
  #                                data.frame(period=paste(year.list[[year]], collapse=" - "),
  #                                           intervention.id=int,
  #                                           is.firm.specific=unique(ms.base$eligible.firms[ms.base$intervention.id==int])=="firm-specific",
  #                                           title=as.character(unique(subset(ms.base, intervention.id==int)$title)),
  #                                           trade.value=tv))
  #   
  #   trade.coverage.estimates=data.frame()
  #   
  #   print(int)
  #   
  # }
  # 
  # rm(check.int, master.sliced)
  
  print(paste(year.list[[year]], collapse=" - "))
}

#### Piotr's code
xlsx::write.xlsx(value.total, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure 1.4 - Trade value totals.xlsx", sep=""))

value.total$period.1 = 1:5

fig4 = ggplot(data=value.total, aes(x=period.1,y=trade.value)) + geom_col(fill=gta_colour$blue[1]) +
  xlab('Period') + gta_theme() +
  ylab('Value of trade billions USD harmed\nby G20 harmful measures') + 
  scale_x_continuous(breaks = 1:5,labels=period.labels) +
  scale_y_continuous(breaks = seq(0,3e12, 5e11), labels=paste(seq(0,3000,500),'bln'), sec.axis = dup_axis()) 
fig4

names(value.firm.specific) <- c("period", "firm.specific")
value.total <- merge(value.total, value.firm.specific, by="period", all = T)

# fig4.firm.spec = ggplot(data=value.total.long, aes(x=period.1,y=trade.value,fill=firm.spec.status)) + geom_col() +
#   xlab('Period') + gta_theme() +
#   ylab('Value of trade in billions USD\nharmed by G20 harmful measures') + 
#   scale_x_continuous(breaks = 1:5,labels=period.labels) +
#   scale_y_continuous(breaks = seq(0,3e12, 5e11), labels=paste(seq(0,3000,500),'bln'), sec.axis = dup_axis()) + 
#   scale_fill_manual(name='',values=gta_colour$blue[c(4,1)],labels=c('Firm specific','Non-firm specific'))
# fig4.firm.spec

gta_plot_saver(fig4,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name="Figure 1.4 - Value of trade harmed by G20 harmful measures")

# gta_plot_saver(fig4.firm.spec,
#                path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
#                name="Figure 1.4 - firm-specific Value of trade harmed by G20 harmful measures")





#### Previous code:
# 
# # value.total is just taking the first values of each period, code does not work
# xlsx::write.xlsx(value.total, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure 1.4 - Trade value totals.xlsx", sep=""))
# xlsx::write.xlsx(value.per.intervention, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure 1.4 - Trade value per intervention.xlsx", sep=""))
# 
# #value.total = xlsx::read.xlsx(paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure 1.4 - Trade value totals.xlsx", sep=""),sheetName = 'Sheet1')
# value.per.intervention = xlsx::read.xlsx(paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure 1.4 - Trade value per intervention.xlsx", sep=""),sheetName = 'Sheet1')
# 
# value.total = aggregate(trade.value~period,value.per.intervention,sum)
# value.total$period.1 = 1:5
# value.total$firm.spec = aggregate(trade.value~period,subset(value.per.intervention, is.firm.specific == T),sum)$trade.value
# value.total$non.firm.spec = value.total$trade.value-value.total$firm.spec
# 
# value.total.long = gather(value.total[,c('period.1','firm.spec','non.firm.spec')], firm.spec.status, trade.value,firm.spec:non.firm.spec, factor_key=TRUE)
# 
# fig4 = ggplot(data=value.total, aes(x=period.1,y=trade.value)) + geom_col(fill=gta_colour$blue[1]) +
#   xlab('Period') + gta_theme() +
#   ylab('Value of trade billions USD\nharmed by G20 harmful measures') + 
#   scale_x_continuous(breaks = 1:5,labels=period.labels) +
#   scale_y_continuous(breaks = seq(0,3e12, 5e11), labels=paste(seq(0,3000,500),'bln'), sec.axis = dup_axis()) 
# fig4
# 
# fig4.firm.spec = ggplot(data=value.total.long, aes(x=period.1,y=trade.value,fill=firm.spec.status)) + geom_col() +
#   xlab('Period') + gta_theme() +
#   ylab('Value of trade in billions USD\nharmed by G20 harmful measures') + 
#   scale_x_continuous(breaks = 1:5,labels=period.labels) +
#   scale_y_continuous(breaks = seq(0,3e12, 5e11), labels=paste(seq(0,3000,500),'bln'), sec.axis = dup_axis()) + 
#   scale_fill_manual(name='',values=gta_colour$blue[c(4,1)],labels=c('Firm specific','Non-firm specific'))
# fig4.firm.spec
# 
# gta_plot_saver(fig4,
#                path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
#                name="Figure 1.4 - Value of trade harmed by G20 harmful measures")
# 
# gta_plot_saver(fig4.firm.spec,
#                path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
#                name="Figure 1.4 - firm-specific Value of trade harmed by G20 harmful measures")
