library(gtalibrary)
library(ggplot2)
library(data.table)
library(splitstackshape)
library(stringr)
rm(list=ls())
setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")

gta_colour_palette()
chapter.number = 10
chapter.title = 'A subsidy accord'
output.path = paste(chapter.number,chapter.title,sep = ' - ')
output.path=paste("0 report production/GTA 24/tables & figures/",output.path, sep="")

## key members
eu=10007
usa=840
chn=156
jpn=country.names$un_code[country.names$name=="Japan"]

## restrict to non-agriculture?
non.agri=T

## update coverage figures?
coverage.update=F

## load simulation
if(non.agri){
  sec.name="Industrial sectors (excl. agriculture)"
  
  load("0 report production/GTA 24/data/10 - A subsidy accord/Coalition results - inward subsidies only1-11 - no agriculture - excl ETS.Rdata")
  
  cs.no.ets=coalition.stats
  cm.no.ets=coalition.members
  
  load("0 report production/GTA 24/data/10 - A subsidy accord/Coalition results - inward subsidies only1-11 - no agriculture - incl ETS.Rdata")
  
  cs.ets=coalition.stats
  cm.ets=coalition.members
} else {
  
  sec.name="All sectors (incl. agriculture)"
  
  load("0 report production/GTA 24/data/10 - A subsidy accord/Coalition results - inward subsidies only1-11 - excl ETS.Rdata")
  
  cs.no.ets=coalition.stats
  cm.no.ets=coalition.members
  
  load("0 report production/GTA 24/data/10 - A subsidy accord/Coalition results - inward subsidies only1-11 - incl ETS.Rdata")
  
  cs.ets=coalition.stats
  cm.ets=coalition.members
}


## add nr of instruments
cs.ets$instruments.nr=str_count(cs.ets$instruments.incl,";")+1
cs.no.ets$instruments.nr=str_count(cs.no.ets$instruments.incl,";")+1
max.inst=max(cs.no.ets$instruments.nr)

## create table for all or n-1 combinations
by.instrument.ets=subset(cs.ets, instruments.nr>=(max.inst-1))
by.instrument.no.ets=subset(cs.no.ets, instruments.nr>=(max.inst-1))

# add missing instrument
all.instruments=unlist(str_split(unique(by.instrument.ets$instruments.incl[by.instrument.ets$instruments.nr==max.inst]), ";"))
by.instrument.ets$instrument.missing="none"
by.instrument.no.ets$instrument.missing="none"

rownames(by.instrument.ets)=NULL
rownames(by.instrument.no.ets)=NULL
for(i in 1:nrow(by.instrument.ets)){
  if(by.instrument.ets$instruments.nr[i]!=max.inst){
    by.instrument.ets$instrument.missing[i]=all.instruments[!all.instruments %in% unlist(str_split(unique(by.instrument.ets$instruments.incl[i]), ";"))]
    
  }
  if(by.instrument.no.ets$instruments.nr[i]!=max.inst){
    by.instrument.no.ets$instrument.missing[i]=all.instruments[!all.instruments %in% unlist(str_split(unique(by.instrument.no.ets$instruments.incl[i]), ";"))]
  }
}

## specify sectors covered
by.instrument.ets$sector.name=sec.name
by.instrument.no.ets$sector.name=sec.name


# big-4 presence
member.jpn.ets=cm.ets$coalition.id[cm.ets$i.un==jpn & cm.ets$type=="member"]
member.chn.ets=cm.ets$coalition.id[cm.ets$i.un==chn & cm.ets$type=="member"]
member.eu.ets=cm.ets$coalition.id[cm.ets$i.un==eu & cm.ets$type=="member"]
member.usa.ets=cm.ets$coalition.id[cm.ets$i.un==usa & cm.ets$type=="member"]

member.jpn.no.ets=cm.no.ets$coalition.id[cm.no.ets$i.un==jpn & cm.no.ets$type=="member"]
member.chn.no.ets=cm.no.ets$coalition.id[cm.no.ets$i.un==chn & cm.no.ets$type=="member"]
member.eu.no.ets=cm.no.ets$coalition.id[cm.no.ets$i.un==eu & cm.no.ets$type=="member"]
member.usa.no.ets=cm.no.ets$coalition.id[cm.no.ets$i.un==usa & cm.no.ets$type=="member"]

by.instrument.ets$incl.jpn=by.instrument.ets$coalition.id %in% member.jpn.ets
by.instrument.ets$incl.chn=by.instrument.ets$coalition.id %in% member.chn.ets
by.instrument.ets$incl.eu=by.instrument.ets$coalition.id %in% member.eu.ets
by.instrument.ets$incl.usa=by.instrument.ets$coalition.id %in% member.usa.ets
by.instrument.ets$incl.all=by.instrument.ets$coalition.id %in% intersect(member.jpn.ets, 
                                                                         intersect(member.chn.ets, 
                                                                                                   intersect(member.usa.ets, member.eu.ets)))

by.instrument.no.ets$incl.jpn=by.instrument.no.ets$coalition.id %in% member.jpn.no.ets
by.instrument.no.ets$incl.chn=by.instrument.no.ets$coalition.id %in% member.chn.no.ets
by.instrument.no.ets$incl.eu=by.instrument.no.ets$coalition.id %in% member.eu.no.ets
by.instrument.no.ets$incl.usa=by.instrument.no.ets$coalition.id %in% member.usa.no.ets
by.instrument.no.ets$incl.all=by.instrument.no.ets$coalition.id %in% intersect(member.jpn.no.ets, 
                                                                               intersect(member.chn.no.ets, 
                                                                                         intersect(member.usa.no.ets, member.eu.no.ets)))


good.order=c("coalition.id","instruments.incl","instrument.missing", 
             "sector.name", "import.utility.weight",
             "member.size","members.liberalising",
             "freerider.count","bystander.count",
             "coalition.total.trade","share.world.imports",
             "coalition.liberalised.trade", "share.world.imports.liberalised",
             "incl.all","incl.chn","incl.eu","incl.jpn","incl.usa")

good.names=c("Coalition ID","Instruments included","Instrument missing", 
             "Sectors included", "Import aversion",
             "Nr of coalition members","Nr of coalition members which liberalise",
             "Nr of freeriders","Nr of bystanders",
             "Total sectoral coalition imports","Share of coalition in sectoral world trade",
             "Total sectoral coalition imports liberalised", "Share of liberalised coalition imports in sectoral world trade",
             "includes BIG 4","includes China","includes EU","includes JPN","includes USA")

by.instrument.ets=by.instrument.ets[,good.order]
by.instrument.no.ets=by.instrument.no.ets[,good.order]

by.instrument.ets.xlsx=by.instrument.ets
by.instrument.no.ets.xlsx=by.instrument.no.ets

names(by.instrument.ets.xlsx)=good.names
names(by.instrument.no.ets.xlsx)=good.names

## store XSLX
xlsx::write.xlsx(by.instrument.ets.xlsx, file=paste(output.path,"/Table ",chapter.number,".1 - Subsidy agreements by instrument - ",sec.name,".xlsx", sep=""), row.names = F, sheetName = "incl ETS in data")
xlsx::write.xlsx(by.instrument.no.ets.xlsx, file=paste(output.path,"/Table ",chapter.number,".1 - Subsidy agreements by instrument - ",sec.name,".xlsx", sep=""), row.names = F, sheetName = "excl ETS in data", append=T)



## add trade coverage numbers
if(coverage.update){
  ets.interventions=c(63250,63254)
  gta_trade_coverage(gta.evaluation = c("red","amber"),
                     affected.flows = "inward",
                     coverage.period = c(2019,2019),
                     intervention.types = all.instruments,
                     keep.type = T,
                     group.type = F)
  xlsx::write.xlsx(trade.coverage.estimates, file=paste(output.path,"/Trade coverage by intervention type.xlsx", sep=""), row.names = F, sheetName = "incl ETS in data")
  
  gta_trade_coverage(gta.evaluation = c("red","amber"),
                     affected.flows = "inward",
                     coverage.period = c(2019,2019),
                     intervention.types = all.instruments,
                     keep.type = T,
                     intervention.ids = ets.interventions,
                     keep.interventions = F,
                     group.type = F)
  xlsx::write.xlsx(trade.coverage.estimates, file=paste(output.path,"/Trade coverage by intervention type.xlsx", sep=""), row.names = F, sheetName = "excl ETS in data", append=T)  
}

