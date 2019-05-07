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
gta_data_slicer(gta.evaluation = c("Red","Amber"),
                in.force.today = T,
                mast.chapters = "D",
                affected.flow="inward",
                keep.mast = F)

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

import.weights=c(seq(-.75,0,.25))

## define areas of cooperation
# areas.of.cooperation=data.frame(cpc=unique(cpc.to.hs$cpc[cpc.to.hs$hs %in% unique(liberalisation.options$affected.product)]),
#                                 level=3)

total.sec=aggregate(trade.value ~ affected.product, trade, sum)
setnames(total.sec, "trade.value","total.imports")

hs2cpc=cpc.to.hs
names(hs2cpc)=c("cpc","affected.product")

total.sec=merge(total.sec, hs2cpc, by="affected.product", all.x=T)
total.sec$cpc2=as.numeric(substr(sprintf(fmt = "%03i",total.sec$cpc),1,2))

sectoral.imports=aggregate(total.imports ~ cpc2, total.sec, sum)

sectoral.imports=sectoral.imports[order(-sectoral.imports$total.imports),]

areas.of.cooperation=data.frame(cpc=unique(as.numeric(substr(sprintf(fmt = "%03i", unique(cpc.to.hs$cpc[cpc.to.hs$hs %in% unique(liberalisation.options$affected.product)])),1,2))),
                                level=2)

areas.of.cooperation=subset(areas.of.cooperation, cpc %in% sectoral.imports$cpc2[1:10])


for(i in 2:10){
 
  combos=as.data.frame(combn(sectoral.imports$cpc2[1:10],i))
    
    for(j in 1:ncol(combos)){
      areas.of.cooperation=rbind(areas.of.cooperation,
                                 data.frame(cpc=paste(combos[,j], collapse=","), 
                                            level="4"))
    }
  print(i)
  }

areas.of.cooperation=subset(areas.of.cooperation, grepl(",", cpc))


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
      
    } 
    
    if(areas.of.cooperation$level[i]==2){
      
      area=gta_cpc_code_expand(areas.of.cooperation$cpc[i])
      
      s.name=paste(unique(as.character(cpc.names$cpc.name[cpc.names$cpc.digit.level==2 & cpc.names$cpc %in% areas.of.cooperation$cpc[i]])), collapse="")
      s.scope=areas.of.cooperation$cpc[i]
    }
    
    if(areas.of.cooperation$level[i]==4){
      
      area=gta_cpc_code_expand(as.numeric(unlist(strsplit(areas.of.cooperation$cpc[i],","))))
      
      s.name="Top 10 combo"
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
    
    if(step%%100==0){
      c.s.xlsx=coalition.stats
      
      names(c.s.xlsx)=c("Coalition ID", "Sectoral scope (CPC)","CPC level","Sector name","Import utility weight", 
                        "Nr of coalition members",  "Nr of members which liberalise", "Nr of freeriding exporters","Nr of bystanding exporters",
                        "Total imports by coalition", "Total liberalised imports by coalition", "Intra-coalition liberalised imports",
                        "Share of coalition's imports in sectoral world trade", "Share of liberalised imports in sectoral world trade")
      
      xlsx::write.xlsx(c.s.xlsx, file="0 report production/GTA 24/tables & figures/7 - Coalitions for liberalisation/Coalition size by relative import weight - intra-Top 10 - inward only (excl trade defense).xlsx", row.names=F)
      save(coalition.stats, coalition.members, file="0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results - intra-Top 10 - inward only (excl trade defense).Rdata")
      
    }
    
  }
  
  
}

c.s.xlsx=coalition.stats

names(c.s.xlsx)=c("Coalition ID", "Sectoral scope (CPC)","CPC level","Sector name","Import utility weight", 
                  "Nr of coalition members",  "Nr of members which liberalise", "Nr of freeriding exporters","Nr of bystanding exporters",
                  "Total imports by coalition", "Total liberalised imports by coalition", "Intra-coalition liberalised imports",
                  "Share of coalition's imports in sectoral world trade", "Share of liberalised imports in sectoral world trade")

xlsx::write.xlsx(c.s.xlsx, file="0 report production/GTA 24/tables & figures/7 - Coalitions for liberalisation/Coalition size by relative import weight - intra-Top 10 - inward only (excl trade defense).xlsx", row.names=F)
save(coalition.stats, coalition.members, file="0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results - intra-Top 10 - inward only (excl trade defense).Rdata")


