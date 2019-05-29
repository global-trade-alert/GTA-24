library(gtalibrary)
library(splitstackshape)
rm(list=ls())
setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
source("0 report production/GTA 24/code/7+ - Coalitions for liberalisation/ch7 functions v2.R")


for(ets in c(T,F)){
  for(agri in c(T,F)){
    
    
    ## chapter 7 & 8
    chapter.number =7
    chapter.title = 'Coalitions for liberalisation'
    output.path = paste(chapter.number,chapter.title,sep = ' - ')
    
    gta_coalition_search(excl.agri=agri,
                         sector.ranks=c(1,"max"),
                         sector.tuple.min=1,
                         sector.tuple.max=1,
                         incl.bb=T,
                         incl.atb=T,
                         incl.subs=T,
                         incl.td=F,
                         instrument.tuple.min="max",
                         instrument.tuple.max="max",
                         excl.ets=ets,
                         excl.targeted=T,
                         incl.flows="inward",
                         growth.rates=1,
                         participation.threshold=0,
                         import.weights=seq(0,-1,-.05),
                         trade.data.year=2017,
                         store.folder=paste("0 report production/GTA 24/data/",output.path,sep=""),
                         create.xlsx=F,
                         store.folder.xlsx=paste("0 report production/GTA 24/tables & figures/",output.path,sep=""))
    
    
    ## chapter 9
    chapter.number =9
    chapter.title = 'Gains from horse trade'
    output.path = paste(chapter.number,chapter.title,sep = ' - ')
    
    gta_coalition_search(excl.agri=agri,
                         sector.ranks=c(1,10),
                         sector.tuple.min=2,
                         sector.tuple.max=10,
                         incl.bb=T,
                         incl.atb=T,
                         incl.subs=T,
                         incl.td=F,
                         instrument.tuple.min="max",
                         instrument.tuple.max="max",
                         excl.ets=ets,
                         excl.targeted=T,
                         incl.flows="inward",
                         growth.rates=1,
                         participation.threshold=0,
                         import.weights=seq(0,-1,-.05),
                         trade.data.year=2017,
                         store.folder=paste("0 report production/GTA 24/data/",output.path,sep=""),
                         create.xlsx=F,
                         store.folder.xlsx=paste("0 report production/GTA 24/tables & figures/",output.path,sep=""))
    
    
    ## chapter 10
    chapter.number =10
    chapter.title = 'A subsidy accord'
    output.path = paste(chapter.number,chapter.title,sep = ' - ')
    
    gta_coalition_search(excl.agri=agri,
                         sector.ranks=c(1,"max"),
                         sector.tuple.min="max",
                         sector.tuple.max="max",
                         incl.bb=F,
                         incl.atb=F,
                         incl.subs=T,
                         incl.td=F,
                         instrument.tuple.min=9,
                         instrument.tuple.max="max",
                         excl.ets=ets,
                         excl.targeted=T,
                         incl.flows="inward",
                         growth.rates=1,
                         participation.threshold=0,
                         import.weights=seq(0,-1,-.05),
                         trade.data.year=2017,
                         store.folder=paste("0 report production/GTA 24/data/",output.path,sep=""),
                         create.xlsx=F,
                         store.folder.xlsx=paste("0 report production/GTA 24/tables & figures/",output.path,sep=""))
    
    
    
    
  }
}