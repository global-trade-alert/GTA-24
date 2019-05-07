rm(list=ls())

library(gtalibrary)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)
library(dplyr)

#setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
#setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
#setwd("/Users/piotrlukaszuk/Dropbox/GTA cloud")
setwd('C:/Users/Kamran/Dropbox/GTA cloud')
#setwd('D:/Dropbox/Dropbox/GTA cloud')

chapter.number = 4
chapter.title = 'Covert jumbo protectionism is the norm'

output.path = paste(chapter.number, chapter.title, sep=' - ')

load('data/master_plus.Rdata')
source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")
load("data/support tables/Final goods support table.Rdata")

jumbo.threshold.1 = 10e9
jumbo.threshold.2 = 100e9
approach = 'conservative'

# "EC: GSP for certain countries' interventions as one
# 30038 state.act.id for EC: GSP for certain countries sectors revoked for 2014-2016 period
ec.revoked.gsp.ids = unique(subset(master, state.act.id == '30038')$intervention.id)
# indian export incentive 2.3 trillion 
false.jumbos = c(70350, 18891, 16819, 71578, 58794, 18254, 13633, 15366, 13512, 18892) 
# coverage by intervention computations -----------------------------------

## importing trade data

trade=subset(final, Year %in% c(2005:2007))[,c("Reporter.un","Partner.un","Year","Tariff.line","Value")]
rm(final)
names(trade)=c("i.un","a.un","year","affected.product","trade.value")

trade=aggregate(trade.value ~ i.un + a.un + affected.product, trade, sum)
trade$trade.value= trade$trade.value/3 

# gtalibrary::elig.firms
## preparing GTA data
gta_data_slicer(gta.evaluation = c("red","amber"),
                implementation.period = c("2008-11-01",cutoff),
                keep.implementation.na = F)

# we need to implement sector-specific search in data slicer 
master.sliced = subset(master.sliced, eligible.firms %in% c('all','sector-specific'))

coverage.by.intervention=unique(master.sliced[,c("intervention.id","date.implemented","currently.in.force")])
coverage.by.intervention$year.implemented=year(coverage.by.intervention$date.implemented)
coverage.by.intervention$date.implemented=NULL
coverage.by.intervention$value.usd=NA
coverage.by.intervention$found.trade=T



master.temp=subset(master.sliced, intervention.id %in% coverage.by.intervention$intervention.id)

## Generating the base file 
gta_imp_exp_hs_tuples(master.path="master.temp",
                      master.data.frame = T)

master.tuple=merge(master.tuple, trade, by=c("i.un","a.un","affected.product"))

coverage.by.intervention$value.usd=NULL

coverage.by.intervention=merge(coverage.by.intervention, aggregate(trade.value ~ intervention.id, master.tuple, sum),by="intervention.id", all.x=T)
coverage.by.intervention$found.trade[is.na(coverage.by.intervention$trade.value)]=F
coverage.by.intervention$trade.value[is.na(coverage.by.intervention$trade.value)]=0

trade.coverage.base = subset(coverage.by.intervention, found.trade==T)

# remove ec.gsp.ids but keep highest 
trade.coverage.base = subset(trade.coverage.base, trade.value==max(subset(trade.coverage.base,intervention.id %in% ec.revoked.gsp.ids)$trade.value)| !(intervention.id %in% ec.revoked.gsp.ids))

# remove false jumbos
trade.coverage.base = subset(trade.coverage.base, !(intervention.id %in% false.jumbos))
## different subsets
ids.all=unique(master.sliced$intervention.id)
ids.conservative=unique(subset(master.sliced, implementation.level %in% c("national", "supranational") &
                                          eligible.firms %in% c("all", "sector-specific"))$intervention.id)

### XLSX specs
year.range = 2008:2019

gta_colour_palette()

# PDF/CDF plots -----------------------------------------------------------------

for(approach in c("all")){
  
  thresholds = c(0,1e7,1e8,1e9,1e10,1e11,1e12,max(trade.coverage.base$trade.value)+1)
  trade.thresholds.by.year = data.frame(Lower.threshold = thresholds[-length(thresholds)], Upper.threshold = thresholds[-1])
  
  if(approach=="all"){
    ids=ids.all
    
    cdf.file.name="All interventions - CDF of harmful intervention trade coverage"
    pdf.file.name="All interventions - PDF of harmful intervention trade coverage"
    table.path = paste0('0 report production/GTA 24/tables & figures/', output.path, '/',chapter.number,'.1 All interventions - Interventions by year and affected trade thresholds.xlsx')
    
  }
  
  if(approach=="conservative"){
    ids=ids.conservative
    
    cdf.file.name="Conservative interventions - CDF of harmful intervention trade coverage"
    pdf.file.name="Conservative interventions - PDF of harmful intervention trade coverage"
    table.path = paste0('0 report production/GTA 24/tables & figures/', output.path, '/',chapter.number,'.1 Conservative interventions - Interventions by year and affected trade thresholds.xlsx')
    
  }
  
  if(approach=="non-conservative"){
    ids=setdiff(ids.all,ids.conservative)
    
    cdf.file.name="Non-conservative interventions - CDF of harmful intervention trade coverage"
    pdf.file.name="Non-conservative interventions - PDF of harmful intervention trade coverage"
    table.path = paste0('0 report production/GTA 24/tables & figures/', output.path, '/',chapter.number,'.1 Non-conservative interventions - Interventions by year and affected trade thresholds.xlsx')
    
  }
  
  ## loop data
  loop.data=subset(trade.coverage.base, intervention.id %in% ids)
  
  ## ecdf 
  log10.cdf=ggplot(loop.data, aes(x=log10(trade.value))) + 
    stat_ecdf(geom = "step", position = "identity",size=1.1) + xlab('Trade value in USD') + ylab('Fraction of Data') + 
    ggtitle('Cumulative density function of the value of trade harmed \nby harmful interventions implemented 2008-2019') + 
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(xlim = c(5, max(log10(loop.data$trade.value)))+0.01) +
    scale_x_continuous(breaks = 5:12,labels= c('100\'000','1 million','10 million','100 million','1 billion', '10 billion', '100 billion', '1 trillion'))+ 
    gta_theme() +
    scale_y_continuous(sec.axis = dup_axis())
  
  log10.cdf

  gta_plot_saver(plot=log10.cdf,
                 path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
                 name=paste(chapter.number, ".1 ",cdf.file.name, sep=""))
  
    
  ## PDF (final version)
  trade.values.war.us <- trade.coverage.base[trade.coverage.base$intervention.id %in% trade.war.us,]$trade.value
  trade.values.war.chn <- trade.coverage.base[trade.coverage.base$intervention.id %in% trade.war.chn,]$trade.value
  trade.values.war.us = log10(trade.values.war.us)
  trade.values.war.chn = log10(trade.values.war.chn)
  
  log10.pdf = ggplot() + 
    geom_density(data=loop.data,aes(x=log10(trade.value)),trim=F, size=1) + xlab('Trade value in billions of USD') + ylab('Probability Density') + 
    ggtitle('Probability density function of the value of trade harmed \nby harmful interventions implemented 2008-2019') + 
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(xlim = c(9, max(log10(loop.data$trade.value)))+0.01) +
    scale_x_continuous(breaks = 9:12,labels=c('1','10','100', '1\'000'))+
    gta_theme() +
    theme(plot.title = element_text(size = 11)) +
    geom_vline(aes(xintercept = trade.values.war.chn, color = gta_colour$qualitative[3]), size=0.7,linetype='twodash', show.legend = TRUE) +
    geom_vline(aes(xintercept = trade.values.war.us, color = gta_colour$qualitative[6]), size=0.7,linetype='twodash', show.legend = TRUE) +
    scale_color_manual(name='',values=gta_colour$qualitative[c(7,2)],labels=c('China harmful interventions \nimplemented in 2018 trade war with the US','US harmful interventions \nimplemented in 2018 trade war with China')) +
    scale_y_continuous(sec.axis = dup_axis())
  
  
  log10.pdf
  
  gta_plot_saver(plot=log10.pdf,
                 path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
                 name=paste(chapter.number,".3 ",pdf.file.name, sep=""))
  
  ## XLSX
  # an XSLX with summary stats about how many interventions affected between x1 and x2 worth of trade for several brackets eg. less than 1bn, 1-2bn, 2-3bn or so
  # choose those brackets as they make sense
  
  for (i in 1:nrow(trade.thresholds.by.year)){
    for (year in 1:length(year.range)){
      
      trade.thresholds.by.year[i,year+2] = length(which(loop.data[loop.data$year.implemented==year.range[year],]$trade.value > trade.thresholds.by.year$Lower.threshold[i] & 
                                                          loop.data[loop.data$year.implemented==year.range[year],]$trade.value < trade.thresholds.by.year$Upper.threshold[i])) 
      
      names(trade.thresholds.by.year)[year+2] = paste(as.character(year.range[year]),'Number of interventions harming trade between threshold values' )
      
    }
  }
  
  colnames(trade.thresholds.by.year)[1:2] = c('Lower Threshold', 'Upper Threshold')
  
  # class(trade.thresholds.by.year$`Lower Threshold`) <- "scientific"
  # class(trade.thresholds.by.year$`Upper Threshold`) <- "scientific"
  
  loop.data=loop.data[order(loop.data$trade.value, decreasing=T),c('intervention.id','year.implemented','trade.value')]
  
  xlsx::write.xlsx(trade.thresholds.by.year, file=table.path, row.names = F, sheetName = "Harmed trade" )
  # xlsx::write.xlsx(trade.thresholds.by.year, file=table.path, row.names = F, sheetName = "Underlying data", append=T)
  
}

#  Task 1 ---------------------------------------------------------------------
# SE request: For the discriminatory measures imposed over the past 10 years, a bar chart showing the frequency of measures harming 10 billion USD 
# or more of trade would be produced. The trade war interventions (enumerated in chapter 2, stored in the GTA 24’s definitions file) 
# would be highlighted and compared to other jumbo measures.

trade.war.state.ids = c(china.us.trade.war.act.ids, eu.sg.steel.act.ids)
trade.war.intervention.ids = c(new.actions.intervention.ids ,china.us.trade.war.intervention.ids)

gta_data_slicer()

master.sliced = subset(master.sliced, eligible.firms %in% c('all','sector-specific'))

trade.war.intervention.ids = unique(subset(master.sliced, (gta.evaluation == 'Red')&((state.act.id %in% trade.war.state.ids)|(intervention.id %in% trade.war.intervention.ids)))$intervention.id)
trade.war.us = unique(subset(master.sliced,(implementing.jurisdiction == 'United States of America')&(affected.jurisdiction == 'China')&(gta.evaluation == 'Red')&((state.act.id %in% trade.war.state.ids)|(intervention.id %in% trade.war.intervention.ids)))$intervention.id)
trade.war.chn = unique(subset(master.sliced,(implementing.jurisdiction == 'China')&(affected.jurisdiction == 'United States of America')&(gta.evaluation == 'Red')&((state.act.id %in% trade.war.state.ids)|(intervention.id %in% trade.war.intervention.ids)))$intervention.id)

## plotting
## For the discriminatory measures imposed over the past 10 years, a bar chart showing the frequency of measures harming 10 billion USD or more of trade would be produced. 
## The trade war interventions (enumerated in chapter 2, stored in the GTA 24’s definitions file) would be highlighted and compared to other jumbo measures.

# for(approach in c("all", "conservative", "non-conservative")){
  
approach="conservative"
plot.name = "Number of interventions per year harming trade for over 10bn USD"

if(approach=="all"){
  ids=ids.all
  
  fig1.file.name= paste(chapter.number, ".2 All interventions - ",plot.name, sep="")
  table.path = paste0('0 report production/GTA 24/tables & figures/', output.path,'/',fig1.file.name,'.xlsx')
  
}

if(approach=="conservative"){
  ids=ids.conservative
  
  fig1.file.name= paste(chapter.number, ".2 Conservative interventions - ",plot.name, sep="")
  table.path = paste0('0 report production/GTA 24/tables & figures/', output.path,'/',fig1.file.name,'.xlsx')
  
}

if(approach=="non-conservative"){
  ids=setdiff(ids.all,ids.conservative)
  
  fig1.file.name= paste(chapter.number, ".2 Non-conservative interventions - ",plot.name, sep="")
  table.path = paste0('0 report production/GTA 24/tables & figures/', output.path,'/',fig1.file.name,'.xlsx')
  
}

## loop data
loop.data=subset(trade.coverage.base, intervention.id %in% ids)

threshold = jumbo.threshold.1

annual.jumbos=aggregate(intervention.id ~ year.implemented, subset(loop.data, trade.value>=threshold & !intervention.id %in% trade.war.intervention.ids), function(x) length(unique(x)))
annual.jumbos$intervention.status="2"
tw.jumbos = subset(loop.data, trade.value>=threshold & intervention.id %in% trade.war.intervention.ids)
if (nrow(tw.jumbos)>0){
tw.jumbos=aggregate(intervention.id ~ year.implemented, tw.jumbos , function(x) length(unique(x)))
tw.jumbos$intervention.status="1"
annual.jumbos=rbind(tw.jumbos,annual.jumbos)
color.values = c(gta_colour$qualitative[2:1])
} else {color.values=gta_colour$qualitative[1]}

if (approach == 'conservative'){annual.jumbos.over.200b = aggregate(intervention.id ~ year.implemented, subset(loop.data, trade.value>=200e9), function(x) length(unique(x)))}
if (approach == 'conservative'){annual.jumbos.over.500b = aggregate(intervention.id ~ year.implemented, subset(loop.data, trade.value>=500e9), function(x) length(unique(x)))}

fig.1 =ggplot(annual.jumbos, aes(x=year.implemented,y=intervention.id,fill=intervention.status)) + geom_col() + 
  scale_x_continuous(breaks=2008:2019,labels=2008:2019) + xlab('Year of implementation of the harmful intervention') +
  ylab(paste('Number of interventions harming trade for over 10 bln USD')) +
  scale_fill_manual(name='',values = color.values, labels=c('Trade war interventions','Non-trade war interventions')) +
  gta_theme() +
  scale_y_continuous(sec.axis = dup_axis())


fig.1

gta_plot_saver(plot=fig.1,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name=fig1.file.name)

annual.jumbos$intervention.status[annual.jumbos$intervention.status=="1"]="Trade war"
annual.jumbos$intervention.status[annual.jumbos$intervention.status=="2"]="Non-trade war"
xlsx::write.xlsx(annual.jumbos, row.names=F, file = table.path)

# }


names(annual.jumbos.over.200b) = c('year.implemented','number.of.interventions')
names(annual.jumbos.over.500b) = c('year.implemented','number.of.interventions')
annual.jumbos.over.200b = rbind(annual.jumbos.over.200b,data.frame(year.implemented = 'all',number.of.interventions = sum(annual.jumbos.over.200b$number.of.interventions)))
annual.jumbos.over.500b = rbind(annual.jumbos.over.500b,data.frame(year.implemented = 'all',number.of.interventions = sum(annual.jumbos.over.500b$number.of.interventions)))

## NOT IN FINAL REPORT
# xlsx::write.xlsx(annual.jumbos.over.200b, row.names=F, file=paste0('0 report production/GTA 24/tables & figures/',output.path,'/annual jumbos over 200b.xlsx'))
# xlsx::write.xlsx(annual.jumbos.over.500b, row.names=F, file=paste0('0 report production/GTA 24/tables & figures/',output.path,'/annual jumbos over 500b.xlsx'))


# Task 2 ------------------------------------------------------------------
# SE request: Please prepare a pie chart of the types of jumbo protectionist measures by MAST category.
# SE never specified a threshold and keeps going back and forward so i just did it for both thresholds of 10b and 100b

# for(approach in c("all", "conservative", "non-conservative")){
approach="conservative"  
  plot.name = "jumbo threshold MAST Chapter distribution harmful interventions"
  
  if(approach=="all"){
    ids=ids.all
    
    fig2.file.name.between.thresholds= paste(chapter.number,".4 All interventions - between ",paste0(as.character(jumbo.threshold.1/1e9), 'bn'),'-',paste0(as.character(jumbo.threshold.2/1e9), 'bn '),' ',plot.name, sep="")
    fig2.file.name.over.upper.threshold= paste(chapter.number,".4 All interventions - over ", paste0(as.character(jumbo.threshold.2/1e9), 'bn '),plot.name,' ', sep="")
    
    table.path.between.thresholds = paste0('0 report production/GTA 24/tables & figures/', output.path,'/',fig2.file.name.between.thresholds,'.xlsx')
    table.path.over.upper.threshold = paste0('0 report production/GTA 24/tables & figures/', output.path,'/',fig2.file.name.over.upper.threshold,'.xlsx')
    
  }
  
  if(approach=="conservative"){
    ids=ids.conservative
    
    fig2.file.name.between.thresholds= paste(chapter.number,".4 Conservative interventions - between ",paste0(as.character(jumbo.threshold.1/1e9), 'bn'),'-',paste0(as.character(jumbo.threshold.2/1e9), 'bn '),' ',plot.name, sep="")
    fig2.file.name.over.upper.threshold= paste(chapter.number,".4 Conservative interventions - over ", paste0(as.character(jumbo.threshold.2/1e9), 'bn '),plot.name,' ', sep="")
    
    table.path.between.thresholds = paste0('0 report production/GTA 24/tables & figures/', output.path,'/',fig2.file.name.between.thresholds,'.xlsx')
    table.path.over.upper.threshold = paste0('0 report production/GTA 24/tables & figures/', output.path,'/',fig2.file.name.over.upper.threshold,'.xlsx')
    
    
  }
  
  if(approach=="non-conservative"){
    ids=setdiff(ids.all,ids.conservative)
    
    fig2.file.name.between.thresholds= paste(chapter.number,".4 Non-conservative interventions - between ",paste0(as.character(jumbo.threshold.1/1e9), 'bn'),'-',paste0(as.character(jumbo.threshold.2/1e9), 'bn '),' ',plot.name, sep="")
    fig2.file.name.over.upper.threshold= paste(chapter.number,".4 Non-conservative interventions - over ", paste0(as.character(jumbo.threshold.2/1e9), 'bn '),plot.name,' ', sep="")
    
    table.path.between.thresholds = paste0('0 report production/GTA 24/tables & figures/', output.path,'/',fig2.file.name.between.thresholds,'.xlsx')
    table.path.over.upper.threshold = paste0('0 report production/GTA 24/tables & figures/', output.path,'/',fig2.file.name.over.upper.threshold,'.xlsx')
    
    
  }
  
  
  loop.data=subset(trade.coverage.base, intervention.id %in% ids)
  
  # SE: please do make a pie chart for the values between 10e9 and 100e9 
  ## between 10e9 - 100e9 threshold
  
  mast.trade.coverage.base=merge(subset(loop.data,trade.value >= jumbo.threshold.1 & trade.value < jumbo.threshold.2),
                                 unique(master.sliced[,c('intervention.id','mast.chapter')]), 
                                 by="intervention.id", all.x=T)
  fig.2.data=aggregate(intervention.id ~ mast.chapter, mast.trade.coverage.base, function(x) length(unique(x)))
  fig.2.data$perc.value=fig.2.data$intervention.id/sum(fig.2.data$intervention.id)*100
  fig.2.data = fig.2.data[order(fig.2.data$perc.value,decreasing=T),]
  fig.2.data$col = colorRampPalette(gta_colour$qualitative)(nrow(fig.2.data))
  
  #changing names to fit your ggplot code.
  setnames(fig.2.data, "intervention.id","value")
  setnames(fig.2.data, "mast.chapter","group")
  levels = fig.2.data$group
  fig.2.data$group = factor(fig.2.data$group,levels=levels)
  cols = fig.2.data[,c('group','col')]
  
  blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=14, face="bold")
    )
  
  
  fig.2  <- ggplot(fig.2.data, aes(x="", y=perc.value, fill=group)) + 
    geom_bar(width = 1, stat = "identity") + coord_polar("y", start=4) + blank_theme + gta_theme() + 
    scale_fill_manual(name='',values=fig.2.data$col) + xlab('') + ylab('') +
    theme(axis.ticks=element_blank(),panel.border = element_blank(),panel.grid=element_blank()) +   theme(axis.text.x=element_blank()) +
    geom_text(aes(x=1.7,y = perc.value,label = ifelse(perc.value>=1,scales::percent(perc.value/100),'')), size=3,position = position_stack(vjust = 0.5)) +
    theme(legend.spacing.x = unit (.5, 'cm'))
  
  
  fig.2
  
  gta_plot_saver(plot=fig.2,
                 path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
                 name= fig2.file.name.between.thresholds)
  
  fig.2.data = fig.2.data[,!(colnames(fig.2.data) %in% c('col'))]
  names(fig.2.data) = c('Mast Chapter','Count','Percentage') 
  xlsx::write.xlsx(fig.2.data, row.names=F, file = table.path.between.thresholds)
  
  ## over upper threshold
  mast.trade.coverage.base=merge(subset(loop.data,trade.value >= jumbo.threshold.2),
                               unique(master.sliced[,c('intervention.id','mast.chapter')]), 
                               by="intervention.id", all.x=T)
  fig.2.data=aggregate(intervention.id ~ mast.chapter, mast.trade.coverage.base, function(x) length(unique(x)))
  fig.2.data$perc.value=fig.2.data$intervention.id/sum(fig.2.data$intervention.id)*100
  fig.2.data = fig.2.data[order(fig.2.data$perc.value,decreasing=T),]
  fig.2.data$col = colorRampPalette(gta_colour$qualitative)(nrow(fig.2.data))
  
  
  #changing names to fit your ggplot code.
  setnames(fig.2.data, "intervention.id","value")
  setnames(fig.2.data, "mast.chapter","group")
  fig.2.data$group = factor(fig.2.data$group,levels=levels)
  
  blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=14, face="bold")
    )


  fig.2  <- ggplot(fig.2.data, aes(x="", y=perc.value, fill=group)) + 
    geom_bar(width = 1, stat = "identity") + coord_polar("y", start=4) + blank_theme + gta_theme() + 
    scale_fill_manual(name='',values=cols[cols$group %in% fig.2.data$group,]$col) + xlab('') + ylab('') +
    theme(axis.ticks=element_blank(),panel.border = element_blank(),panel.grid=element_blank()) +   theme(axis.text.x=element_blank()) +
    geom_text(aes(x=1.7,y = perc.value,label = ifelse(perc.value>0,scales::percent(perc.value/100),'')), size=3,position = position_stack(vjust = 0.5)) +
    theme(legend.spacing.x = unit (.5, 'cm'))
  
    
  
  fig.2

  gta_plot_saver(plot=fig.2,
                 path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
                 name= fig2.file.name.over.upper.threshold)
  
  fig.2.data = fig.2.data[,!(colnames(fig.2.data) %in% c('col'))]
  names(fig.2.data) = c('Mast Chapter','Count','Percentage') 
  xlsx::write.xlsx(fig.2.data, row.names=F, file = table.path.over.upper.threshold)



# }

# Task 3 ------------------------------------------------------------------

# SE request: Please compute the number of jumbo protectionist measures that affect only one trading partner.

gta_data_slicer(gta.evaluation=c('Red','Amber'),
                keep.implementation.na=F,
                nr.affected=c(1,1)
                )
master.sliced = subset(master.sliced, eligible.firms %in% c('all','sector-specific'))


master.sliced=master.sliced[!is.na(master.sliced$affected.jurisdiction),]
unique.affected.partner.interventions = unique(master.sliced$intervention.id)

unique.affected.partner.jumbo = trade.coverage.base[trade.coverage.base$intervention.id %in% unique.affected.partner.interventions,]

total.unique.affected.partner.jumbo = data.frame(thresholds = c('10b','100b'), 
                                                 'Number of remaining protectionist interventions affecting one partner' = 
                                                   c(length(which(unique.affected.partner.jumbo$trade.value > jumbo.threshold.1 & unique.affected.partner.jumbo$trade.value <= jumbo.threshold.2)),
                                                     length(which(unique.affected.partner.jumbo$trade.value > jumbo.threshold.2))))  

  
xlsx::write.xlsx(total.unique.affected.partner.jumbo, row.names=F, file = paste("0 report production/GTA 24/tables & figures/",output.path,"/",chapter.number,".3 Number of jumbo protectionist interventions affecting one trading partner.xlsx",sep=''))


#  Task 4 -----------------------------------------------------------------
# Request: Please send SE the number of jumbo protectionist measures affecting 10 billion USD of trade. 
# Add the value of trade affected to that list. If the number is not too large (SE will decide) then please produce a table of all of the jumbo protectionist
# measures listed in descending order of trade coverage today, indicating the implementing jurisdiction, the name of the measure, MAST chapter,
# date of implementation, whether the measure is still in force, and whether the measure affects only 1 trading partner 
# (if so, identify which trading partner.)
##
# Modified his request to 100 billion USD
gta_data_slicer()

master.sliced = subset(master.sliced, eligible.firms %in% c('all','sector-specific'))

conservative.trade.coverage.base=subset(trade.coverage.base, intervention.id %in% ids.conservative)

trade.coverage.base.10b.threshold = conservative.trade.coverage.base[conservative.trade.coverage.base$trade.value > jumbo.threshold.1,]

trade.coverage.base.10b.threshold = merge(trade.coverage.base.10b.threshold, 
                                           master.sliced[,c('intervention.id','mast.chapter','implementing.jurisdiction','title','date.implemented')], 
                                           by ='intervention.id')

trade.coverage.base.10b.threshold = trade.coverage.base.10b.threshold[,c('intervention.id','implementing.jurisdiction','title','mast.chapter','date.implemented','currently.in.force','trade.value')]

trade.coverage.base.10b.threshold$affects.one.partner = 'FALSE'
trade.coverage.base.10b.threshold[trade.coverage.base.10b.threshold$intervention.id %in% unique.affected.partner.interventions,]$affects.one.partner = 'TRUE'
trade.coverage.base.10b.threshold = trade.coverage.base.10b.threshold[order(trade.coverage.base.10b.threshold$trade.value, decreasing=T),]


add.unique.affected.partner = subset(master.sliced, intervention.id %in% unique.affected.partner.interventions)[,c('intervention.id','affected.jurisdiction')]

trade.coverage.base.10b.threshold = merge(trade.coverage.base.10b.threshold, add.unique.affected.partner, by='intervention.id', all.x=T)
trade.coverage.base.10b.threshold = trade.coverage.base.10b.threshold[!duplicated(trade.coverage.base.10b.threshold),]
names(trade.coverage.base.10b.threshold) = c('Intervention ID','Implementing Jurisdiction','Title','Mast Chapter','Implemented Date','Currently in Force','Trade Value','Affects unique partner','Unique affected partner')                                                     

#  NOT IN FINAL REPORT
# xlsx::write.xlsx(trade.coverage.base.10b.threshold, row.names=F, file = paste("0 report production/GTA 24/tables & figures/",output.path,"/",chapter.number,".4 Table of (10b threshold) jumbo protectionist measures.xlsx",sep=''))


conservative.jumbo.threshold.1.ids  = unique(subset(conservative.trade.coverage.base,trade.value>jumbo.threshold.1)$intervention.id)
conservative.jumbo.threshold.2.ids  = unique(subset(conservative.trade.coverage.base,trade.value>jumbo.threshold.2)$intervention.id)
# number computations for text --------------------------------------------

gta_data_slicer(gta.evaluation = c("red","amber"),
                implementation.period = c("2008-11-01",cutoff),
                keep.implementation.na = F)

coverage.base=unique(master.sliced[,c("intervention.id","date.implemented","currently.in.force","mast.id")])
coverage.base$year.implemented=year(coverage.base$date.implemented)
coverage.base$date.implemented=NULL
coverage.base$value.usd=NA
coverage.base$found.trade=T


master.temp=subset(master.sliced, intervention.id %in% coverage.base$intervention.id)

## Generating the base file 
gta_imp_exp_hs_tuples(master.path="master.temp",
                      master.data.frame = T)

master.tuple=merge(master.tuple, trade, by=c("i.un","a.un","affected.product"))

coverage.base$value.usd=NULL

coverage.base=merge(coverage.base, aggregate(trade.value ~ intervention.id+i.un+a.un+affected.product, master.tuple, sum),by="intervention.id", all.x=T)
coverage.base$found.trade[is.na(coverage.base$trade.value)]=F
coverage.base$trade.value[is.na(coverage.base$trade.value)]=0

coverage.base = subset(coverage.base, found.trade==T)

# removal jumbo measures (threshold 10b)
removal.df = data.frame(removal.jumbo.10b = sum(unique(subset(coverage.base, intervention.id %in% conservative.jumbo.threshold.1.ids, select=c('i.un','a.un','affected.product','trade.value')))$trade.value))
# removal jumbo measures (threshold 100b)
removal.df$removal.jumbo.100b = sum(unique(subset(coverage.base, intervention.id %in% conservative.jumbo.threshold.2.ids, select=c('i.un','a.un','affected.product','trade.value')))$trade.value)
# removal P7
removal.df$removal.P7 = sum(unique(subset(coverage.base, mast.id == 'P7', select=c('i.un','a.un','affected.product','trade.value')))$trade.value)
# removal P8
removal.df$removal.P8 = sum(unique(subset(coverage.base, mast.id == 'P8', select=c('i.un','a.un','affected.product','trade.value')))$trade.value)
# removal P7&P8
removal.df$removal.P7.P8 = sum(unique(subset(coverage.base, mast.id %in% c('P7','P8'), select=c('i.un','a.un','affected.product','trade.value')))$trade.value)
# removal L
removal.df$removal.L = sum(unique(subset(coverage.base, mast.id == 'L', select=c('i.un','a.un','affected.product','trade.value')))$trade.value)
# removal TARIFF
removal.df$removal.TARIFF = sum(unique(subset(coverage.base, mast.id == 'TARIFF', select=c('i.un','a.un','affected.product','trade.value')))$trade.value)
# removal p7 & p8 & L & tariff
removal.df$removal.p7.p8.L.TARIFF = sum(unique(subset(coverage.base, mast.id %in% c('P7','P8','L','TARIFF'), select=c('i.un','a.un','affected.product','trade.value')))$trade.value)

## NOT IN FINAL REPORT
# xlsx::write.xlsx(removal.df, row.names=F, file = paste("0 report production/GTA 24/tables & figures/",output.path,"/Table trade affected with removal Jumbo p7 p8 L TARIFF.xlsx",sep=''))




