rm(list=ls())

library(gtalibrary)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)
library(dplyr)
library(splitstackshape)

#setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
#setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
#setwd("/Users/piotrlukaszuk/Dropbox/GTA cloud")
setwd('C:/Users/Kamran/Dropbox/GTA cloud')
#setwd('D:/Dropbox/Dropbox/GTA cloud')

chapter.number = 4
chapter.title = 'Covert jumbo protectionism is the norm'

output.path = paste(chapter.number, chapter.title, sep=' - ')

source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")

## Check 1:
# please find out how many jumbos are left if we drop interventions 
# that have a known budget and 
# where the trade affected estimate is 100+ times that budget.
## levels: 1 = percent, 2 = total budget (USD) 

# jumbo intervention.ids
load("0 report production/GTA 24/data/4 - Covert jumbo protectionism is the norm/trade per jumbo(10bn) intervention.Rdata")

# trade coverage base
load(paste0("0 report production/GTA 24/data/", output.path,"/coverage base.Rdata"))

## gta database replica
load("data/database replica/database replica - parts - base.Rdata")

names(gta_intervention)=gsub("_","\\.",names(gta_intervention))
jumbo.budgets = merge(trade.jumbo.intervention, 
              subset(gta_intervention, intervention.id %in% jumbo.ids & (is.na(prior.level)==F|is.na(new.level)==F),select=c('intervention.id','prior.level','new.level','level.unit')),
              by = 'intervention.id', all = T)

# Does null just mean no budget allocation? 
ratio.known.budgets.jumbo = 1-length(which(is.na(jumbo.budgets$level.unit) | jumbo.budgets$level.unit == "NULL"))/nrow(jumbo.budgets)
ratio.total.budgets.jumbo = length(which(jumbo.budgets$level.unit == 2))/nrow(jumbo.budgets)
budget.stats = data.frame(ratio.known.budgets.jumbo = ratio.known.budgets.jumbo, 
                          ratio.total.budgets.jumbo = ratio.total.budgets.jumbo)
names(budget.stats) = c('Share of jumbo budgets known', 'Share of jumbo budgets known in total(USD) format ')

total.jumbo.budgets = subset(jumbo.budgets, level.unit == 2, select = c('intervention.id', 'new.level', 'trade.value'))
total.jumbo.budgets$trade.exceeds.100x.budget = 'No'
condition = which(total.jumbo.budgets$trade.value > 100*as.numeric(as.character(total.jumbo.budgets$new.level)))
total.jumbo.budgets$trade.exceeds.100x.budget[condition] = 'Yes'
jumbo.budget.outliers = subset(total.jumbo.budgets, trade.exceeds.100x.budget == 'Yes')$intervention.id
total.jumbo.budgets$observed.multiplier=round(total.jumbo.budgets$trade.value/as.numeric(as.character(total.jumbo.budgets$new.level)),2)
total.jumbo.budgets$intervention.id=paste("https://www.globaltradealert.org/intervention/", total.jumbo.budgets$intervention.id, sep="")
names(total.jumbo.budgets) = c('Intervention URL', 'Total(USD) Budget', 'Trade affected', 'Does trade affected exceed 100 x total budget?','Observed multiplier (trade/budget)')
## NOT IN REPORT
xlsx::write.xlsx(budget.stats, row.names=F, sheetName = 'known budgets', file = paste("0 report production/GTA 24/tables & figures/",output.path,"/removal jumbos affected trade exceeding 100 times budget.xlsx",sep=''))
xlsx::write.xlsx(total.jumbo.budgets, row.names=F, sheetName = 'trade values & budgets',file = paste("0 report production/GTA 24/tables & figures/",output.path,"/removal jumbos affected trade exceeding 100 times budget.xlsx",sep=''), append=T)

## Check 2:
# Please find out how many jumbos are left if measures affecting a single sector are dropped
gta_data_slicer()
#
ids.single.sector = subset(aggregate(affected.sector~intervention.id, subset(cSplit(master.sliced,which(colnames(master.sliced)=='affected.sector'), direction = "long", sep = ",",drop=TRUE), select=c('intervention.id','affected.sector')),
                                     function(x) length(unique(x))), affected.sector == 1)$intervention.id
#
non.sgl.sect.jumbo = jumbo.ids[!jumbo.ids %in% ids.single.sector]

jumbo.counts = data.frame(count.jumbos.10bn = length(jumbo.ids), 
                          count.non.single.sector.jumbos.10bn = length(non.sgl.sect.jumbo))
colnames(jumbo.counts) = c('Number of jumbos', 'Jumbos affecting more than one sector')

## NOT IN REPORT
xlsx::write.xlsx(jumbo.counts, row.names=F, sheetName = "Number of jumbos",file = paste("0 report production/GTA 24/tables & figures/",output.path,"/Non-single sector jumbos.xlsx",sep=''))
xlsx::write.xlsx(data.frame(single.sector.jumbos = intersect(jumbo.ids,ids.single.sector), Intervention.URL = paste0("https://www.globaltradealert.org/intervention/", intersect(jumbo.ids, ids.single.sector))), sheetName = "Single sector jumbos", row.names=F, file = paste("0 report production/GTA 24/tables & figures/",output.path,"/Non-single sector jumbos.xlsx",sep=''), append = T)

## Check 3: Find values of world exports covered by different jumbos
#World exports "covered" by the 348 jumbo measures
trade.jumbos = sum(unique(subset(coverage.base, intervention.id %in% jumbo.ids & currently.in.force == 'Yes', 
                                 select=c('i.un','a.un','affected.product','trade.value')))$trade.value)
#World exports covered by the jumbos that are not single sector
trade.non.sgl.jumbos = sum(unique(subset(coverage.base, intervention.id %in% non.sgl.sect.jumbo & currently.in.force == 'Yes', 
                                         select=c('i.un','a.un','affected.product','trade.value')))$trade.value)
#World exports covered by the jumbos excluding the small number (I think 12) 
#where the budget is known to be less than 1% of trade affected
trade.non.outlier.jumbos = sum(unique(subset(coverage.base, intervention.id %in% setdiff(jumbo.ids, jumbo.budget.outliers) & currently.in.force == 'Yes',
                                             select=c('i.un','a.un','affected.product','trade.value')))$trade.value)
#World exports covered by the jumbos that are not single sector 
#and where the budget is too small (see above.)
trade.non.outlier.sgl.jumbos = sum(unique(subset(coverage.base, intervention.id %in% setdiff(non.sgl.sect.jumbo, jumbo.budget.outliers)&currently.in.force == 'Yes',
                                                 select=c('i.un','a.un','affected.product','trade.value')))$trade.value)
trade.affected = data.frame(trade.jumbos = trade.jumbos, 
                            trade.non.sgl.jumbos = trade.non.sgl.jumbos, 
                            trade.non.outlier.jumbos = trade.non.outlier.jumbos,
                            trade.non.outlier.sgl.jumbos = trade.non.outlier.sgl.jumbos)
names(trade.affected) = c('Exports covered by jumbos', 
                          'Exports covered by jumbos affecting more than one sector',
                          'Exports covered without jumbos of budget lower than 1% of trade affected', 
                          'Exports covered without jumbos of budget lower than 1% of trade affected and with jumbos affecting more than one sector')
xlsx::write.xlsx(trade.affected, row.names=F, sheetName = "Exports Affected",file = paste("0 report production/GTA 24/tables & figures/",output.path,"/Exports affected by non-single sector and without outlier jumbos.xlsx",sep=''))
