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
coverage.update=T

## load simulation
if(non.agri){
  sec.name="Industrial sectors (excl. agriculture)"
  
  load("0 report production/GTA 24/data/10 - A subsidy accord/Coalition results - inward subsidies only - no agriculture - excl ETS.Rdata")
  
  cs.no.ets=coalition.stats
  cm.no.ets=coalition.members
  
  load("0 report production/GTA 24/data/10 - A subsidy accord/Coalition results - inward subsidies only - no agriculture - incl ETS.Rdata")
  
  cs.ets=coalition.stats
  cm.ets=coalition.members
} else {
  
  sec.name="All sectors (incl. agriculture)"
  
  load("0 report production/GTA 24/data/10 - A subsidy accord/Coalition results - inward subsidies only - excl ETS.Rdata")
  
  cs.no.ets=coalition.stats
  cm.no.ets=coalition.members
  
  load("0 report production/GTA 24/data/10 - A subsidy accord/Coalition results - inward subsidies only - incl ETS.Rdata")
  
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
xlsx::write.xlsx(by.instrument.ets.xlsx, file=paste(output.path,"/Coalition results - Subsidy agreements by instrument - ",sec.name,".xlsx", sep=""), row.names = F, sheetName = "incl ETS in data")
xlsx::write.xlsx(by.instrument.no.ets.xlsx, file=paste(output.path,"/Coalition results - Subsidy agreements by instrument - ",sec.name,".xlsx", sep=""), row.names = F, sheetName = "excl ETS in data", append=T)



## add trade coverage numbers
ets.interventions=c(63250,63254)

if(non.agri){
  agri.sectors=gta_cpc_code_expand(c(1,2,3,4,21,22,23))
  agri.products=cpc.to.hs$hs[cpc.to.hs$cpc %in% agri.sectors]
  
  gta_trade_coverage(gta.evaluation = c("red","amber"),
                     affected.flows = "inward",
                     coverage.period = c(2019,2019),
                     intervention.types = all.instruments,
                     keep.type = T,
                     group.type = F,
                     hs.codes = agri.products,
                     keep.hs = F)
  xlsx::write.xlsx(trade.coverage.estimates, file=paste(output.path,"/Figure ",chapter.number,".1 - Trade coverage by intervention type - Industrial goods trade only.xlsx", sep=""), row.names = F, sheetName = "incl ETS in data")
  tc.ets=trade.coverage.estimates
  rm(trade.coverage.estimates)
  
  gta_trade_coverage(gta.evaluation = c("red","amber"),
                     affected.flows = "inward",
                     coverage.period = c(2019,2019),
                     intervention.types = all.instruments,
                     keep.type = T,
                     intervention.ids = ets.interventions,
                     keep.interventions = F,
                     group.type = F,
                     hs.codes = agri.products,
                     keep.hs = F)
  xlsx::write.xlsx(trade.coverage.estimates, file=paste(output.path,"/Figure ",chapter.number,".1 - Trade coverage by intervention type - Industrial goods trade only.xlsx", sep=""), row.names = F, sheetName = "excl ETS in data", append=T)  
  tc.no.ets=trade.coverage.estimates
  rm(trade.coverage.estimates)
  
} else {
  
  
  
  gta_trade_coverage(gta.evaluation = c("red","amber"),
                     affected.flows = "inward",
                     coverage.period = c(2019,2019),
                     intervention.types = all.instruments,
                     keep.type = T,
                     group.type = F)
  xlsx::write.xlsx(trade.coverage.estimates, file=paste(output.path,"/Figure ",chapter.number,".1 - Trade coverage by intervention type - All sectors incl agriculture.xlsx", sep=""), row.names = F, sheetName = "incl ETS in data")
  tc.ets=trade.coverage.estimates
  rm(trade.coverage.estimates)
  
  gta_trade_coverage(gta.evaluation = c("red","amber"),
                     affected.flows = "inward",
                     coverage.period = c(2019,2019),
                     intervention.types = all.instruments,
                     keep.type = T,
                     intervention.ids = ets.interventions,
                     keep.interventions = F,
                     group.type = F)
  xlsx::write.xlsx(trade.coverage.estimates, file=paste(output.path,"/Figure ",chapter.number,".1 - Trade coverage by intervention type - All sectors incl agriculture.xlsx", sep=""), row.names = F, sheetName = "excl ETS in data", append=T)  
  tc.no.ets=trade.coverage.estimates
  rm(trade.coverage.estimates)
}

# stacked bar
bar.chart=tc.ets[,c("Intervention type","Trade coverage estimate for 2019")]
bar.chart=merge(bar.chart, tc.no.ets[,c("Intervention type","Trade coverage estimate for 2019")], by="Intervention type")
names(bar.chart)=c("intervention.type","trade.share.incl","trade.share.excl")
bar.chart$ets.effect=bar.chart$trade.share.incl-bar.chart$trade.share.excl

bc=bar.chart[,c("intervention.type","trade.share.excl")]
bc$ets="Without EU ETS"
names(bc)=c("intervention.type","trade.share","ets.status")

bc.e=bar.chart[,c("intervention.type","ets.effect")]
bc.e$ets="Including EU ETS"
names(bc.e)=c("intervention.type","trade.share","ets.status")

bar.chart=rbind(bc, bc.e)

inst.order=tc.ets[,c("Intervention type","Trade coverage estimate for 2019")]
inst.order$`Intervention type`[inst.order$`Intervention type`=="Capital injection and equity stakes (including bailouts)"]="Capital injection and\nequity stakes\n(including bailouts)"
inst.order=inst.order[order(-inst.order$`Trade coverage estimate for 2019`),]$`Intervention type`

bar.chart$intervention.type[bar.chart$intervention.type=="Capital injection and equity stakes (including bailouts)"]="Capital injection and\nequity stakes\n(including bailouts)"
bar.chart$intervention.type=factor(bar.chart$intervention.type, levels=inst.order)
bar.chart=bar.chart[order(bar.chart$intervention.type),]
bar.chart$position=bar.chart$trade.share/2
bar.chart$position[bar.chart$position<.008]=.008
bar.chart$position[bar.chart$intervention.type=="Tax or social insurance relief" & bar.chart$ets.status=="Including EU ETS"]=.10
bar.chart$position[bar.chart$intervention.type=="All included instruments" & bar.chart$ets.status=="Including EU ETS"]=.215

p1=
ggplot()+
  geom_bar(data=bar.chart, aes(x=intervention.type, y=trade.share, fill=ets.status), stat = "identity", width=.65)+
  geom_text(data=subset(bar.chart, trade.share>0), aes(x= intervention.type, y = position, label = scales::percent(round(trade.share,3)), family=""), size = 3) +
  labs(x="Subsidy instrument" , y="Share of world non-agricultural goods trade\naffected by interventions currently in force",
       fill="")+
  coord_flip()+
  scale_fill_manual(values=c(gta_colour$brown[2], gta_colour$blue[2]))+
  guides(guide_legend(title = NULL))+
  gta_theme()+
  theme(axis.text.x.bottom = element_text(hjust=0.9 , size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.y.right = element_text(size=10),
        legend.text = element_text(size=8),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color="transparent"),
        legend.position = c(.9,.9),
        legend.background = element_rect(fill="transparent"),
        legend.margin = margin(t=-10,b=0,r=30,l=0))

p1

gta_plot_saver(plot=p1,
               path = output.path,
               name = paste0("Figure ", chapter.number, ".1 - Trade affected by subsidies - Industrial sectors only")
                 )



## Map of exposure to subsidies

gta_trade_coverage(gta.evaluation = c("red","amber"),
                   affected.flows = "inward",
                   coverage.period = c(2019,2019),
                   intervention.types = all.instruments,
                   keep.type = T,
                   group.type = T,
                   group.exporters = F,
                   hs.codes = agri.products,
                   keep.hs = F)


l.map=trade.coverage.estimates[,c("Exporting country","Trade coverage estimate for 2019")]
names(l.map)=c("name","value")
l.map=merge(l.map, country.names[,c("un_code","name")], by="name", all.x=T)
setnames(l.map, "un_code","UN")

world <- gtalibrary::world.geo

world = merge(world, l.map[,c("UN","value")], by="UN", all.x=T)

###### IMPORTANT, sort for X (id) again
world <-  world[with(world, order(X)),]
world$value[is.na(world$value) == T] <- 0
min.w=min(world$value, na.rm = T)
max.w=max(world$value, na.rm = T)

map.breaks=seq(min.w,max.w,.2)
map.colors=gta_colour$red.shades(length(map.breaks))[length(map.breaks):1]
map.labels=c("less than 20%","20 - 40%","40 - 60%","60 - 80%","80% or more")

map1=
  ggplot() +
  geom_polygon(data= subset(world, country != "Antarctica"), 
               aes(x = long, y = lat, group = group, fill = value), size = 0.15, color = "white") +
  geom_polygon(data=subset(world, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#dadada", size = 0.15, colour = "white") +
  coord_fixed() + # Important to fix world map proportions
  scale_y_continuous(limits=c(-55,85))+
  scale_x_continuous(limits=c(-169,191))+
  labs(x="", y="") +
  scale_fill_gradientn(colours = map.colors, 
                       
                       breaks=map.breaks, 
                       position="bottom", 
                       labels=map.labels) + # Set color gradient
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(family = "", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
        legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10)),
        legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=0, margin = margin(r=10)),
        legend.text.align = 0
  ) +
  guides(fill=guide_legend(title="Share of exports\ncompeting with\nsubsidised local rival", label.position = "top"),
         ymax=guide_legend(title="size"))

map1

gta_plot_saver(plot=map1,
               path=output.path,
               name=paste("Figure ", chapter.number, ".2 - Map for export exposure to chapter L - Industrial sectors only", sep=""),
               width = 21,
               height = 12)


xlsx::write.xlsx(trade.coverage.estimates, file=paste(output.path,paste("/Figure ", chapter.number, ".2 - Map for export exposure to chapter L - Industrial sectors only.xlsx", sep=""), sep=""), row.names = F)

## Import and export exposure of big 4
gta_trade_value_bilateral(trade.data=2017)
total.trade=subset(trade.base.bilateral, ! hs6 %in% agri.products)
rm(trade.base.bilateral)

subsidy.exposure=data.frame()


for(biggie in c(chn, eu, jpn, usa)){

  if(biggie==10007){
    big.countries=country.names$un_code[country.names$is.eu]
  } else {
    big.countries= biggie
  }
  
  print(biggie)    
  ## export side  
  if(sum(as.numeric(big.countries %in% country.names$un_code[country.names$is.eu]))==0){
    
    gta_trade_coverage(gta.evaluation = c("red","amber"),
                       affected.flows = "inward",
                       coverage.period = c(2019,2019),
                       intervention.types = all.instruments,
                       keep.type = T,
                       group.type = T,
                       exporters = big.countries,
                       keep.exporters = T,
                       hs.codes = agri.products,
                       keep.hs = F,
                       trade.data=2017)
    
  } else {
    
    gta_trade_coverage(gta.evaluation = c("red","amber"),
                       affected.flows = "inward",
                       coverage.period = c(2019,2019),
                       intervention.types = all.instruments,
                       keep.type = T,
                       group.type = T,
                       importers = big.countries,
                       keep.importers = F,
                       exporters = big.countries,
                       keep.exporters = T,
                       hs.codes = agri.products,
                       keep.hs = F,
                       trade.data=2017)
    
  }
  
  subsidy.exposure=rbind(subsidy.exposure,
                         data.frame(actor=paste(big.countries, collapse=","),
                                    flow="exports",
                                    trade.share=trade.coverage.estimates$`Trade coverage estimate for 2019`,
                                    stringsAsFactors = F))
  rm(trade.coverage.estimates)
  
  ## import side
  if(sum(as.numeric(big.countries %in% country.names$un_code[country.names$is.eu]))==0){
    
    gta_trade_coverage(gta.evaluation = c("red","amber"),
                       affected.flows = "inward",
                       coverage.period = c(2019,2019),
                       intervention.types = all.instruments,
                       keep.type = T,
                       group.type = T,
                       importers = big.countries,
                       keep.importers = T,
                       hs.codes = agri.products,
                       keep.hs = F,
                       trade.data=2017,
                       trade.statistic="value")
    
    
    
  } else {
    
    gta_trade_coverage(gta.evaluation = c("red","amber"),
                       affected.flows = "inward",
                       coverage.period = c(2019,2019),
                       intervention.types = all.instruments,
                       keep.type = T,
                       group.type = T,
                       importers = big.countries,
                       keep.importers = T,
                       exporters = big.countries,
                       keep.exporters = F,
                       hs.codes = agri.products,
                       keep.hs = F,
                       trade.data=2017,
                       trade.statistic="value")
    
  }
  
  tt=sum(subset(total.trade, i.un %in% big.countries & ! a.un %in% big.countries)$trade.value) ## there is no intra-US trade here, so this is fine.
  
  subsidy.exposure=rbind(subsidy.exposure,
                         data.frame(actor=paste(big.countries, collapse=","),
                                    flow="imports",
                                    trade.share=trade.coverage.estimates$`Trade coverage estimate for 2019`/tt,
                                    stringsAsFactors = F))
  rm(trade.coverage.estimates)
  
}

subsidy.exposure$trade.share=scales::percent(round(subsidy.exposure$trade.share,3))
subsidy.exposure.xlsx=reshape(subsidy.exposure, idvar = "actor",timevar = "flow", direction="wide")
names(subsidy.exposure.xlsx)=c("WTO member","Export percentage affected by interventions of trading partners","Import percentage affected by own interventions")
subsidy.exposure.xlsx=subsidy.exposure.xlsx[,c(1,3,2)]
xlsx::write.xlsx(subsidy.exposure.xlsx, file=paste(output.path,paste("/Table ", chapter.number, ".1 - Relative import and export exposure.xlsx", sep=""), sep=""),row.names = F)


