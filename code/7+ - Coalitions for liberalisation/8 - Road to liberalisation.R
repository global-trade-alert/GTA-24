library(gtalibrary)
library(ggplot2)
library(gtable)
library(grid)
library(data.table)
library(gridExtra)
rm(list=ls())

# font_import()
loadfonts(device="postscript")
loadfonts(device="win")

setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")


## general settings
incl.agriculture=F
incl.ets=F



chapter.number = 8
chapter.title = 'Road to liberalisation'
output.path = paste(chapter.number,chapter.title,sep = ' - ')
output.path=paste("0 report production/GTA 24/tables & figures/",output.path, sep="")



## definitions
ets.interventions=c(63250,63254)
agri.sectors=gta_cpc_code_expand(c(1,2,3,4,21,22,23))
agri.products=cpc.to.hs$hs[cpc.to.hs$cpc %in% agri.sectors]


gta_colour_palette()

## Choosing trade data
gta_trade_value_bilateral(trade.data="2017",df.name="trade")
setnames(trade, "hs6","affected.product")
total.imports=aggregate(trade.value ~ affected.product, trade, sum)
setnames(total.imports, "trade.value","total.imports")

hs2cpc=cpc.to.hs
names(hs2cpc)=c("cpc","affected.product")

total.imports=merge(total.imports, hs2cpc, by="affected.product", all.x=T)
total.imports$cpc2=as.numeric(substr(sprintf(fmt = "%03i",total.imports$cpc),1,2))

## removing agriculture
if(incl.agriculture==F){
  total.imports=subset(total.imports, ! cpc %in% agri.sectors)
}


sectoral.imports=aggregate(total.imports ~ cpc2, total.imports, sum)
sectoral.imports$share=sectoral.imports$total.imports/sum(trade$trade.value)
cpc.shorts=read.csv("0 report production/GTA 24/help files/cpc names shortened.csv", sep=";")
setnames(sectoral.imports, "cpc2", "cpc")

sectoral.imports=merge(sectoral.imports, cpc.shorts[,c("cpc","cpc.short")])
sectoral.imports$cpc.short=gsub("\\\\n"," ",sectoral.imports$cpc.short)
sectoral.imports=sectoral.imports[,c(1,4,2,3)]

####### CHOOSE SPECIFICATION
nr.sectors=length(unique(sectoral.imports$cpc))


## Road to liberalisation

# load data
if(incl.agriculture){
  if(incl.ets){
    load("0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results - at & behind-the border interventions (excl trade defence) - incl ETS.Rdata")
    specification="Industrial & agricultural goods - inward subsidies only (excl trade defence) - incl ETS"
  } else {
    load("0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results - at & behind-the border interventions (excl trade defence) - excl ETS.Rdata")
    specification="Industrial & agricultural goods - inward subsidies only (excl trade defence) - excl ETS"
  }
} else {
  if(incl.ets){
    load("0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results - at & behind-the border interventions (excl trade defence) - no agriculture - incl ETS.Rdata")
    specification="Industrial goods (excl agriculture) - inward subsidies only (excl trade defence) - incl ETS"
  } else {
    load("0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results - at & behind-the border interventions (excl trade defence) - no agriculture - excl ETS.Rdata")
    specification="Industrial goods (excl agriculture) - inward subsidies only (excl trade defence) - excl ETS"
  }
}


cpc.shorts=read.csv("0 report production/GTA 24/help files/cpc names shortened.csv", sep=";")
setnames(cpc.shorts, "cpc", "sector.scope")

cpc2=subset(coalition.stats, grepl(",",sector.scope)==F)[c("coalition.id","sector.scope","import.utility.weight","member.size","coalition.liberalised.trade","share.world.imports", "share.world.imports.liberalised")]
cpc2=merge(cpc2, cpc.shorts[,c("sector.scope", "cpc.short","cpc.order")], by="sector.scope", all.x=T)
cpc2$sector.name=gsub("\\\\n"," ",cpc2$cpc.short)
# cpc2$sector.name=paste(gsub("\\\\n"," ",cpc2$cpc.short), " (CPC ",cpc2$sector.scope,")", sep="")

## top 20
sectoral.imports=sectoral.imports[order(-sectoral.imports$total.imports),]

setnames(sectoral.imports, "cpc","sector.scope")
cpc2=merge(cpc2, sectoral.imports[,c("sector.scope","share")], by="sector.scope", all.x=T)
setnames(sectoral.imports,"sector.scope", "cpc")

cpc2$sector.name=factor(cpc2$sector.name, levels=unique(cpc2$sector.name)[order(unique(cpc2$share))])


cpc2.20=subset(cpc2, sector.scope %in% sectoral.imports$cpc[1:nr.sectors])



sectoral.world.market.share=aggregate(trade.value ~ i.un + affected.product, trade, sum)
setnames(sectoral.world.market.share, "trade.value","country.imports")
sectoral.world.market.share=merge(sectoral.world.market.share, hs2cpc, by="affected.product", all.x=T)
sectoral.world.market.share$cpc2=as.numeric(substr(sprintf(fmt = "%03i",sectoral.world.market.share$cpc),1,2))

sectoral.world.market.share=aggregate(country.imports ~ i.un + cpc2, sectoral.world.market.share, sum)

## accounting EU/EEU members
eu.members=country.correspondence$un_code[country.correspondence$name=="EU-28"]
eeu.members=country.correspondence$un_code[country.correspondence$name=="Eurasian Economic Union"]

ti.eu=aggregate(country.imports ~ cpc2, subset(sectoral.world.market.share, i.un %in% eu.members), sum)
ti.eu$i.un=10007
ti.eeu=aggregate(country.imports ~ cpc2, subset(sectoral.world.market.share, i.un %in% eeu.members), sum)
ti.eeu$i.un=10008

sectoral.world.market.share=rbind(subset(sectoral.world.market.share, !i.un %in% c(eu.members,eeu.members)), ti.eu, ti.eeu)
setnames(sectoral.world.market.share, "cpc2","cpc")
sectoral.world.market.share=merge(sectoral.world.market.share, sectoral.imports[,c("cpc","total.imports")], by="cpc", all.x=T)
sectoral.world.market.share$share=sectoral.world.market.share$country.imports/sectoral.world.market.share$total.imports

if(sum(aggregate(share ~ cpc, sectoral.world.market.share, sum)$share)%%1!=0){
  stop("Some issue with your market shares")
}

## iso codes
conversion=read.csv("R help files/country_iso_un.csv",sep=";",stringsAsFactors = F)
conversion=unique(conversion[,c("ISO","UN")])
names(conversion)=c("iso","i.un")

## plotting
serious.margin=.03

for(i.weight in c(-.5,-.25)){
  c.id=unique(subset(coalition.stats, sector.scope %in% cpc2.20$sector.scope & import.utility.weight==i.weight)$coalition.id)
  road.plot=subset(coalition.members, coalition.id %in% c.id & type!="bystander")
  
  road.plot=merge(road.plot, unique(coalition.stats[,c("coalition.id","sector.scope")]), by="coalition.id")
  setnames(road.plot, "sector.scope","cpc")
  
  road.plot=merge(road.plot, sectoral.world.market.share[,c("i.un", "cpc","share")], by=c("i.un", "cpc"), all.x=T)
  road.plot$share[is.na(road.plot$share)]=0
  
  if(nrow(subset(road.plot, type=="member"))>0){
    other.in=merge(aggregate(net.gain ~ cpc + coalition.id + type, subset(road.plot, type=="member" & share<serious.margin), sum),
                   aggregate(share ~ cpc + coalition.id + type, subset(road.plot, type=="member" & share<serious.margin), sum),
                   by=c("cpc","coalition.id","type"))
    other.in$i.un="1001"
    
    road.plot=rbind(subset(road.plot, (share>=serious.margin & type=="member")|type!="member"),
                    other.in)
    
  }
  
  if(nrow(subset(road.plot, type=="freerider"))>0){
    
    other.out=merge(aggregate(net.gain ~ cpc + coalition.id + type, subset(road.plot, type=="freerider" & share<serious.margin), sum),
                    aggregate(share ~ cpc + coalition.id + type, subset(road.plot, type=="freerider" & share<serious.margin), sum),
                    by=c("cpc","coalition.id","type"))
    other.out$i.un="1002"
    
    
    road.plot=rbind(subset(road.plot, (share>=serious.margin & type=="freerider")|type!="freerider"),
                    other.out)
    
    
  }
  
  
  
  
  
  road.plot=merge(road.plot, conversion, by="i.un", all.x=T)
  road.plot$iso[road.plot$i.un=="10007"]="EU"
  road.plot$iso[road.plot$i.un=="10008"]="EEU"
  road.plot$iso[road.plot$i.un=="1001"]="min"
  road.plot$iso[road.plot$i.un=="1002"]="min"
  
  road.plot$intensity=NA
  road.plot$position=NA
  for(i in unique(road.plot$coalition.id)){
    if(sum(subset(road.plot, coalition.id==i)$share)<1){
      road.plot=rbind(road.plot,
                      data.frame(i.un=1003,
                                 cpc=unique(subset(road.plot, coalition.id==i)$cpc),
                                 net.gain=0,
                                 coalition.id=i,
                                 type="bystander",
                                 share=1-sum(subset(road.plot, coalition.id==i)$share),
                                 iso="bys",
                                 intensity=NA,
                                 position=NA))
    }
    
    road.plot$intensity[road.plot$coalition.id==i]=road.plot$net.gain[road.plot$coalition.id==i]/max(abs(road.plot$net.gain[road.plot$coalition.id==i]))
    
    cumul=subset(road.plot, coalition.id==i)
    cumul=cumul[order(-cumul$intensity),]
    cumul$position[1]=cumul$share[1]/2
    for(j in 2:nrow(cumul)){
      cumul$position[j]=cumul$share[j]/2+sum(cumul$share[1:(j-1)])
    }
    
    road.plot=rbind(subset(road.plot, coalition.id!=i), cumul)
    
  }
  
  setnames(cpc.shorts, "sector.scope","cpc")
  road.plot=merge(road.plot, cpc.shorts[,c("cpc","cpc.short")], by="cpc", all.x=T)
  road.plot$cpc.short=gsub("\\\\n"," ",road.plot$cpc.short)
  setnames(cpc.shorts, new="sector.scope","cpc")
  
  setnames(sectoral.imports, "share","sec.size")
  road.plot=merge(road.plot, sectoral.imports[,c("cpc","sec.size")], by="cpc", all.x=T)
  setnames(sectoral.imports,"sec.size", "share")
  
  road.plot$cpc.short =factor(road.plot$cpc.short, levels=unique(road.plot$cpc.short)[order(unique(road.plot$sec.size))])
  
  road.plot=road.plot[order(-road.plot$net.gain),]
  
  if(min(road.plot$intensity)<0){
    
    plot=ggplot()+
      geom_bar(data=road.plot, 
               aes(x=cpc.short, y=share, fill=intensity),
               colour="#848484", 
               stat = "identity", width=.8)+
      geom_text(data=road.plot, 
                aes(x= as.factor(cpc.short), y = position, label = iso, family=""), 
                size = 2, colour="black", nudge_x = .1) +
      scale_fill_gradientn(colours = c(gta_colour$harmful[1], "#ececec", gta_colour$liberalising[1]),
                           breaks=c(-1,0,1),
                           na.value="white",
                           name="favorability\ntowards\nagreement")+
      scale_y_continuous(breaks=seq(0,1,.2))+
      coord_flip()+
      geom_hline(yintercept = .8, linetype="dashed")+
      gta_theme()+
      labs(x="", y="Cumulative world trade share of given sector")+
      theme(panel.background = element_blank(), 
            panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
            legend.position="bottom",
            axis.text.x.bottom = element_text(hjust = 0.5))
  } else {
    
    plot=ggplot()+
      geom_bar(data=road.plot, 
               aes(x=cpc.short, y=share, fill=intensity),
               colour="#848484", 
               stat = "identity", width=.8)+
      geom_text(data=road.plot, 
                aes(x= as.factor(cpc.short), y = position, label = iso, family=""), 
                size = 2, colour="black", nudge_x = .1) +
      scale_fill_gradient(low="#ececec", 
                          high=gta_colour$liberalising[1],
                          na.value="white",
                          name="favorability\ntowards\nagreement",
                          breaks=c(0,1))+
      scale_y_continuous(breaks=seq(0,1,.2))+
      coord_flip()+
      geom_hline(yintercept = .8, linetype="dashed")+
      gta_theme()+
      labs(x="", y="Cumulative world trade share of given sector")+
      theme(panel.background = element_blank(), 
            panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
            legend.position="bottom",
            axis.text.x.bottom = element_text(hjust = 0.5))
  }
  
  
  plot
  
  gta_plot_saver(plot = plot,
                 path = output.path,
                 name = paste0("Figure ", chapter.number, ".1 - Critical coalition members - ",nr.sectors," sectors - IW ",i.weight))
  
  
}



