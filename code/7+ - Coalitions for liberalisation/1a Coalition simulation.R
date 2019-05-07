library(gtalibrary)
library(splitstackshape)
rm(list=ls())
setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
source("0 report production/GTA 24/code/7 - Coalitions for liberalisation/ch7 functions.R")

chapter.number = 7
chapter.title = 'Coalitions for liberalisation'
output.path = paste(chapter.number,chapter.title,sep = ' - ')

## Choosing trade data
gta_trade_value_bilateral(trade.data="2017",df.name="trade")
setnames(trade, "hs6","affected.product")

total.imports=aggregate(trade.value ~ i.un + affected.product, trade, sum)

## EU/EEU members
eu.members=country.correspondence$un_code[country.correspondence$name=="EU-28"]
eeu.members=country.correspondence$un_code[country.correspondence$name=="Eurasian Economic Union"]

ti.eu=aggregate(trade.value ~ affected.product, subset(trade, i.un %in% eu.members), sum)
ti.eu$i.un=10007
ti.eeu=aggregate(trade.value ~ affected.product, subset(trade, i.un %in% eeu.members), sum)
ti.eeu$i.un=10008

total.imports=rbind(total.imports, ti.eu, ti.eeu)
setnames(total.imports, "trade.value","total.imports")

## What tariff lines are part of the prize? i.e currently discriminated against

specification="all interventions"

if(specification=="all interventions") {
  ## all barriers
  gta_data_slicer(gta.evaluation = c("Red","Amber"),
                  in.force.today = T,
                  mast.chapters = c("D"),
                  keep.mast = F)
}


if(specification=="inward only") {
  ## inward barriers
  gta_data_slicer(gta.evaluation = c("Red","Amber"),
                  in.force.today = T,
                  mast.chapters = c("D"),
                  affected.flow="inward",
                  keep.mast = F)
  
}

if(specification=="inward without subsidies") {
  
  ## inward barriers w/o subsidies
  gta_data_slicer(gta.evaluation = c("Red","Amber"),
                  in.force.today = T,
                  mast.chapters = c("D","L"),
                  affected.flow="inward",
                  keep.mast = F)
}




liberalisation.options=unique(subset(master.sliced, is.na(affected.product)==F)[,c("intervention.id","i.un","affected.product")])
liberalisation.options=cSplit(liberalisation.options, which(names(liberalisation.options)=="affected.product"), sep=",", direction="long")
liberalisation.options=aggregate(intervention.id ~., liberalisation.options, function(x) length(unique(x)))
barrier.count=unique(liberalisation.options[,c("i.un","affected.product","intervention.id")])
setnames(barrier.count, "intervention.id","nr.of.hits")

liberalisation.options=merge(merge(liberalisation.options, trade, by=c("i.un","affected.product")), total.imports, by=c("i.un","affected.product"))
liberalisation.options$market.share=liberalisation.options$trade.value/liberalisation.options$total.imports

prize.allocation=unique(liberalisation.options[,c("i.un","affected.product","a.un","market.share")])

## loop growth from above; remove areas where no nucleus; proceed with those that have
# growth.rates=c(seq(.5,.1,-.05),seq(.09,.01,-.01))
growth.rates=1

## Export/import weights
participation.threshold=0
# exporter.weight=1
# domestic.producer.weight=1
# consumer.weight=0
# relative.import.utility=(consumer.weight-domestic.producer.weight)/exporter.weight

import.weights=seq(-1,0,.05)

## define areas of cooperation
# areas.of.cooperation=data.frame(cpc=unique(cpc.to.hs$cpc[cpc.to.hs$hs %in% unique(liberalisation.options$affected.product)]),
#                                 level=3)
areas.of.cooperation=data.frame(cpc=unique(as.numeric(substr(sprintf(fmt = "%03i", unique(cpc.to.hs$cpc[cpc.to.hs$hs %in% unique(liberalisation.options$affected.product)])),1,2))),
                                level=2)



## initialise records
coalition.members=data.frame()
coalition.stats=data.frame(coalition.id=numeric(),
                           sector.scope=character(),
                           sector.level=numeric(),
                           sector.name=character(),
                           import.utility.weight=numeric(),
                           member.size=numeric(),
                           members.liberalising=numeric(),
                           freerider.count=numeric(),
                           bystander.count=numeric(),
                           coalition.total.trade=numeric(),
                           coalition.liberalised.trade=numeric(),
                           intra.coalition.liberalised.trade=numeric(),
                           share.world.imports=numeric(),
                           share.world.imports.liberalised=numeric())


step=1
for(growth in growth.rates){
  for(i in 1:nrow(areas.of.cooperation)){
    
    if(areas.of.cooperation$level[i]==3){
      area=areas.of.cooperation$cpc[i]
      
      s.name=paste(unique(as.character(cpc.names$cpc.name[cpc.names$cpc.digit.level==3 & cpc.names$cpc %in% area])), collapse="")
      s.scope=paste(area, collapse = ";")
      
    } else {
      
      area=gta_cpc_code_expand(areas.of.cooperation$cpc[i])
      
      s.name=paste(unique(as.character(cpc.names$cpc.name[cpc.names$cpc.digit.level==2 & cpc.names$cpc %in% areas.of.cooperation$cpc[i]])), collapse="")
      s.scope=areas.of.cooperation$cpc[i]
    }
    
    
    print(paste("STARTING AREA",area))
    area.codes=intersect(subset(cpc.to.hs, cpc %in% area)$hs,unique(liberalisation.options$affected.product))
    exporters=unique(trade$a.un[trade$affected.product %in% area.codes])
    
    prize.allocation.area=subset(prize.allocation, affected.product %in% area.codes)
    area.world.imports=sum(subset(total.imports, affected.product %in% area.codes & 
                                    i.un<10000)$total.imports)
    
    for(i.weight in import.weights){
      
      ## initialise: only countries with an upside will join the coalition i.e. exporters
      print(paste("STARTING import weight",i.weight))
      
      gains=gain_from_agreement(area.codes,
                                exporters,
                                exporters,
                                "barrier.count", 
                                i.weight, 
                                participation.threshold,
                                "total.imports",
                                "area.world.imports",
                                "prize.allocation.area",
                                growth)
      
      coalition=gains$coalition
      free.riders=gains$free.riders
      net.income=gains$net.income
      by.stander=gains$by.stander
      
      ## storing result
      c.id=length(unique((coalition.stats$coalition.id)))+1
      if(length(coalition)>0){
        c.ms=data.frame(coalition.id=c.id,
                        i.un=net.income$i.un[net.income$i.un %in% coalition],
                        type="member")
        
        c.ms=merge(c.ms, subset(net.income, i.un %in% coalition)[,c("i.un","result")], all.x=T)
        c.ms[is.na(c.ms)]=0
        data.table::setnames(c.ms, "result","net.gain")
        coalition.members=rbind(coalition.members, 
                                c.ms)
        
      }
      
      if(nrow(free.riders)>0){
        
        
        c.ms=free.riders
        c.ms$coalition.id=c.id
        c.ms$type="freerider"
        data.table::setnames(c.ms, "result","net.gain")
        
        coalition.members=rbind(coalition.members, c.ms)
        
      }
      
      
      if(length(by.stander)>0){
        
        coalition.members=rbind(coalition.members, 
                                data.frame(i.un=by.stander,
                                           coalition.id=c.id,
                                           type="bystander",
                                           net.gain=0))
        
      }
      
      
      coalition.stats=rbind(coalition.stats,
                            data.frame(coalition.id=c.id,
                                       sector.scope=s.scope,
                                       sector.level=areas.of.cooperation$level[i],
                                       sector.name=s.name,
                                       import.utility.weight=i.weight,
                                       member.size=gains$m.count,
                                       members.liberalising=gains$lib.count,
                                       freerider.count=gains$f.count,
                                       bystander.count=gains$b.count,
                                       coalition.total.trade=gains$c.t.trade,
                                       coalition.liberalised.trade=gains$c.l.trade,
                                       intra.coalition.liberalised.trade=gains$intra.c.l.trade,
                                       share.world.imports=gains$imp.share,
                                       share.world.imports.liberalised=gains$imp.share.liberalised))
      
      
      rm(gains, net.income, coalition, free.riders)

      
      
    }
    
    print(paste("FINISHED AREA",area,"(",step,"out of",nrow(areas.of.cooperation),")."))
    step=step+1
    print(paste("FINISHED AREA",area))
    
  }
  
  
}

c.s.xlsx=coalition.stats

names(c.s.xlsx)=c("Coalition ID", "Sectoral scope (CPC)","CPC level","Sector name","Import utility weight", 
                  "Nr of coalition members",  "Nr of members which liberalise", "Nr of freeriding exporters","Nr of bystanding exporters",
                  "Total imports by coalition", "Total liberalised imports by coalition", "Intra-coalition liberalised imports",
                  "Share of coalition's imports in sectoral world trade", "Share of liberalised imports in sectoral world trade")



if(specification=="all interventions") {
  ## all barriers
  xlsx::write.xlsx(c.s.xlsx, file="0 report production/GTA 24/tables & figures/7 - Coalitions for liberalisation/Coalition size by relative import weight - all GTA interventions (excl Trade defense).xlsx", row.names=F)
  save(coalition.stats, coalition.members, file="0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results.Rdata")  
}


if(specification=="inward only") {
  ## inward barriers
  xlsx::write.xlsx(c.s.xlsx, file="0 report production/GTA 24/tables & figures/7 - Coalitions for liberalisation/Coalition size by relative import weight - inward only (excl Trade defense).xlsx", row.names=F)
  save(coalition.stats, coalition.members, file="0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results - inward only (excl Trade defense).Rdata")
  
}

if(specification=="inward without subsidies") {
  
  ## inward barriers w/o subsidies
  xlsx::write.xlsx(c.s.xlsx, file="0 report production/GTA 24/tables & figures/7 - Coalitions for liberalisation/Coalition size by relative import weight - inward only (excl Subsidies  & trade defense).xlsx", row.names=F)
  save(coalition.stats, coalition.members, file="0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results - inward only (excl Subsidies & trade defense).Rdata")
}


