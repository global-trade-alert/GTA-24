rm(list=ls())

library(gtalibrary)

#setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
setwd('D:/Dropbox/Dropbox/GTA cloud')

chapter.number = 4
chapter.title = 'Covert jumbo protectionism is the norm'

data.path = paste(chapter.number, chapter.title, sep=' - ')

source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")
load(paste0("0 report production/GTA 24/data/",data.path,"/conservative jumbos 10bn.Rdata"))

# "EC: GSP for certain countries' interventions as one
# 30038 state.act.id for EC: GSP for certain countries sectors revoked for 2014-2016 period
gta_data_slicer()
ec.revoked.gsp.ids = unique(subset(master.sliced, state.act.id == '30038')$intervention.id)
# indian export incentive 2.3 trillion 
false.jumbos = c(70350, 18891, 16819, 71578, 58794, 18254, 13633, 15366, 13512, 18892) 

trade.coverage.base.10b.threshold = subset(trade.coverage.base.10b.threshold, !intervention.id %in% c(ec.revoked.gsp.ids,false.jumbos))

jumbos.war.sino.us = unique(subset(trade.coverage.base.10b.threshold, sino.us.war == T)$intervention.id)
jumbos.2018 = unique(subset(trade.coverage.base.10b.threshold, year(date.implemented) == 2018)$intervention.id)
jumbos.2018 = jumbos.2018[!(jumbos.2018 %in% c(63143, 63174))]
jumbos.trade.war = unique(subset(trade.coverage.base.10b.threshold, intervention.id %in% trade.war.intervention.ids)$intervention.id)

## numerator 
gta_trade_coverage(intervention.ids = jumbos.war.sino.us, 
                   keep.interventions = T, 
                   trade.data = '2017',
                   trade.statistic = 'value',
                   intra.year.duration = F, 
                   coverage.period = c(2018,2018) 
                   )

numerator = trade.coverage.estimates[,4:ncol(trade.coverage.estimates)]
## denominator Option 1 all jumbos implemented in 2018
gta_trade_coverage(intervention.ids = setdiff(jumbos.2018, 60244), 
                   keep.interventions = T,  
                   trade.data = '2017',
                   in.force.today = T,
                   trade.statistic = 'value',
                   intra.year.duration = F, 
                   revocation.period = c("2019-01-01","2222-01-01"),
                   keep.revocation.na = T,
                   coverage.period = c(2018,2018) 
)
denominator=trade.coverage.estimates[,4:ncol(trade.coverage.estimates)]
numerator/denominator

## all jumbos in force 2018, 12,31
odd.ones=c(18029, 18563, 58791, 17058, 18030, 20073, 58853, 12860, 19949, 19178, 63250, 19966, 13570, 16786, 19965, 19962, 19947, 59224, 20036, 16737, 58813, 19770, 58793, 20050, 17409, 18926, 15327, 20091, 20016, 18769, 19941, 19944, 20062, 57058, 69449, 15482, 59226, 20038, 58751, 19959, 70021, 15064, 19190, 59059, 58050, 15424, 60343, 58685, 18590, 16977, 18591, 16561, 14697, 58227, 60414, 18426, 20035, 13530, 63143, 58752, 16400, 20052, 20010, 13775, 18076, 13267, 14854, 17814, 16623, 13193, 5720, 19446, 19156, 14011, 20046, 20658, 19619, 14084, 17312, 60244, 17330, 15479, 61371, 70475, 13387, 19476, 68968, 58800, 13038, 13383, 19474, 18048, 19475, 16606, 18662, 20013, 13356, 11841, 15883, 57791, 17416, 15325, 16034, 17835, 16539, 13385, 16022, 14013, 17410, 19648, 15301, 71266, 64853, 15728, 19217, 18650, 63174, 57733, 19282, 12745, 19319, 18633, 18437, 13912, 62969, 20362, 19676, 16610, 60802, 12805, 63102, 15476, 70037, 15755, 17415)
gta_trade_coverage(intervention.ids = setdiff(unique(trade.coverage.base.10b.threshold$intervention.id), odd.ones), 
                   keep.interventions = T,  
                   trade.data = '2017',
                   in.force.today = T,
                   trade.statistic = 'value',
                   intra.year.duration = F, 
                   revocation.period = c("2019-01-01","2222-01-01"),
                   keep.revocation.na = T,
                   coverage.period = c(2018,2018) 
)
denominator.2=trade.coverage.estimates[,4:ncol(trade.coverage.estimates)]
numerator/denominator
numerator/denominator.2

## denominator Option 2 the value of the light blue bloc
gta_trade_coverage(intervention.ids = jumbos.trade.war, 
                   keep.interventions = T, 
                   trade.data = '2017',
                   trade.statistic = 'value',
                   intra.year.duration = F, 
                   coverage.period = c(2018,2018) 
)
numerator.2=trade.coverage.estimates[,4:ncol(trade.coverage.estimates)]
numerator.2/denominator

denominator.trade.war = trade.coverage.estimates$`Trade coverage estimate for 2019`

table.output = data.frame(num.over.denom.2018 = numerator$`Trade coverage estimate for 2018`[1]/denominator.2018[1], 
                  num.over.denom.trade.war = numerator$`Trade coverage estimate for 2019`[1]/denominator.trade.war[1])

xlsx::write.xlsx(table.output, row.names=F, file = paste("0 report production/GTA 24/tables & figures/ppt/sino us jumbos % total value exports.xlsx",sep=''))
