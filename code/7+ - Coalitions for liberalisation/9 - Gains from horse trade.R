library(gtalibrary)
library(ggplot2)
library(data.table)
library(splitstackshape)
library(stringr)
rm(list=ls())
gta_setwd()

gta_colour_palette()
chapter.number = 9
chapter.title = 'Gains from horse trade'
output.path = paste(chapter.number,chapter.title,sep = ' - ')
output.path=paste("0 report production/GTA 24/tables & figures/",output.path, sep="")


## general settings
incl.agriculture=F
incl.ets=F
threshold.share=.8
single.iuw.different=F


## load top10 simulation
# load data
if(incl.agriculture){
  if(incl.ets){
    load("0 report production/GTA 24/data/9 - Gains from horse trade/Coalition results - at & behind-the border interventions (excl trade defence) - incl ETS.Rdata")
    specification="Industrial & agricultural goods - inward subsidies only (excl trade defence) - incl ETS"
  } else {
    load("0 report production/GTA 24/data/9 - Gains from horse trade/Coalition results - at & behind-the border interventions (excl trade defence) - excl ETS.Rdata")
    specification="Industrial & agricultural goods - inward subsidies only (excl trade defence) - excl ETS"
  }
} else {
  if(incl.ets){
    load("0 report production/GTA 24/data/9 - Gains from horse trade/Coalition results - at & behind-the border interventions (excl trade defence) - no agriculture - incl ETS.Rdata")
    specification="Industrial goods (excl agriculture) - inward subsidies only (excl trade defence) - incl ETS"
  } else {
    load("0 report production/GTA 24/data/9 - Gains from horse trade/Coalition results - at & behind-the border interventions (excl trade defence) - no agriculture - excl ETS.Rdata")
    specification="Industrial goods (excl agriculture) - inward subsidies only (excl trade defence) - excl ETS"
  }
}


cs.multi=coalition.stats
cm.multi=subset(coalition.members, coalition.id %in% cs.multi$coalition.id)

## load coalition data, restricting to import weights used in top10 simulation


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

cs.single=coalition.stats
cs.single$sector.scope=as.numeric(as.character(cs.single$sector.scope))
cm.single=subset(coalition.members, coalition.id %in% cs.single$coalition.id)

rm(coalition.members, coalition.stats)




### summary stats

# SE:  
# A table with the following columns: 
# import hostility parameter, 
# number of sectors in the best deal, 
# CPC codes of those sectors, 
# number of WTO members inside the coalition, and 
# share of world trade liberalised.
# I suppose it would be good to know how many possible multi-sector deals are possible for each value of the import hostility parameter. 
cs.sectors=cSplit(cs.multi[,c("coalition.id","import.utility.weight","sector.scope")], 3, direction="long", sep=",")
cs.sectors=aggregate(sector.scope ~., cs.sectors, function(x) length(unique(x)))
setnames(cs.sectors, "sector.scope","nr.of.sectors")
cs.multi=merge(cs.multi, cs.sectors, by=c("coalition.id","import.utility.weight"), all.x=T)


# unconditional
sum.stats=data.frame(import.utility.weight=unique(cs.multi$import.utility.weight), 
                     stringsAsFactors = F)
sum.stats=merge(sum.stats,
                aggregate(coalition.id ~ import.utility.weight, subset(cs.multi, member.size>0), function(x) length(unique(x))), 
                by="import.utility.weight", all=T)
setnames(sum.stats, "coalition.id","nr.of.deals")
sum.stats=merge(sum.stats,
                aggregate(member.size ~ import.utility.weight, subset(cs.multi, member.size>0), function(x) round(mean(x),2)), 
                by="import.utility.weight", all=T)
sum.stats=merge(sum.stats,
                aggregate(nr.of.sectors ~ import.utility.weight, subset(cs.multi, member.size>0), function(x) round(mean(x),2)), 
                by="import.utility.weight", all=T)

sum.stats=merge(sum.stats,
                aggregate(coalition.total.trade ~ import.utility.weight, subset(cs.multi, member.size>0), function(x) round(mean(x),2)), 
                by="import.utility.weight", all=T)


sum.stats=merge(sum.stats,
                aggregate(share.world.imports ~ import.utility.weight, subset(cs.multi, member.size>0), function(x) round(mean(x),2)), 
                by="import.utility.weight", all=T)

sum.stats=merge(sum.stats,
                aggregate(coalition.liberalised.trade ~ import.utility.weight, subset(cs.multi, member.size>0), function(x) round(mean(x),2)), 
                by="import.utility.weight", all=T)


sum.stats=merge(sum.stats,
                aggregate(share.world.imports.liberalised ~ import.utility.weight, subset(cs.multi, member.size>0), function(x) round(mean(x),2)), 
                by="import.utility.weight", all=T)

## adding maximal liberaliser

## joining it all

sum.stats[is.na(sum.stats)]=0
names(sum.stats)=c("Import aversion","Number of deals",
                   "Average member size",
                   "Average number of sectors included (CPC 2-digit level)", 
                   "Average total coalition sectoral import value",
                   "Average total coalition sectoral import share",
                   "Average liberalised coalition sectoral import value",
                   "Average liberalised coalition sectoral import share")

xlsx::write.xlsx(sum.stats, file=paste(output.path,"/Table ",chapter.number,".1 - Summary statistics of deals per given import aversion.xlsx", sep=""), row.names = F, sheetName ="No threshold")


### adding maximum liberaliser
max.stats=data.frame(import.utility.weight=unique(cs.multi$import.utility.weight), 
                     stringsAsFactors = F)

find.max=aggregate(coalition.liberalised.trade ~import.utility.weight,subset(cs.multi, member.size>0), max)

my.max=c()
for(i in 1:nrow(find.max)){
  if(find.max$coalition.liberalised.trade[i]>0){
    my.max=c(my.max, 
             unique(subset(cs.multi, 
                           member.size>0 & 
                             import.utility.weight==find.max$import.utility.weight[i] &
                             coalition.liberalised.trade==find.max$coalition.liberalised.trade[i])$coalition.id)[1])
  }
}



max.stats=merge(max.stats,
                aggregate(sector.scope ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), function(x) paste(unique(x), collapse=",")), 
                by="import.utility.weight", all=T)

max.stats=merge(max.stats,
                aggregate(member.size ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), sum), 
                by="import.utility.weight", all=T)
max.stats=merge(max.stats,
                aggregate(nr.of.sectors ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), sum), 
                by="import.utility.weight", all=T)

max.stats=merge(max.stats,
                aggregate(coalition.total.trade ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), sum), 
                by="import.utility.weight", all=T)


max.stats=merge(max.stats,
                aggregate(share.world.imports ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), sum), 
                by="import.utility.weight", all=T)

max.stats=merge(max.stats,
                aggregate(coalition.liberalised.trade ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), sum), 
                by="import.utility.weight", all=T)


max.stats=merge(max.stats,
                aggregate(share.world.imports.liberalised ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), sum), 
                by="import.utility.weight", all=T)


max.stats[is.na(max.stats)]=0
names(max.stats)=c("Import aversion",
                   "Sectors included in agreement",
                   "Member size",
                   "Number of sectors included (CPC 2-digit level)", 
                   "Total coalition sectoral import value",
                   "Total coalition sectoral import share",
                   "Liberalised coalition sectoral import value",
                   "Liberalised coalition sectoral import share")

xlsx::write.xlsx(max.stats, file=paste(output.path,"/Table ",chapter.number,".1 - Summary statistics of deals per given import aversion.xlsx", sep=""), row.names = F, sheetName ="No threshold - MAX", append=T)





# conditional on passing .8 threshold
sum.stats=data.frame(import.utility.weight=unique(cs.multi$import.utility.weight), 
                     stringsAsFactors = F)
sum.stats=merge(sum.stats,
                aggregate(coalition.id ~ import.utility.weight, subset(cs.multi, member.size>0 & share.world.imports>=threshold.share), function(x) length(unique(x))), 
                by="import.utility.weight", all=T)
setnames(sum.stats, "coalition.id","nr.of.deals")
sum.stats=merge(sum.stats,
                aggregate(member.size ~ import.utility.weight, subset(cs.multi, member.size>0 & share.world.imports>=threshold.share), function(x) round(mean(x),2)), 
                by="import.utility.weight", all=T)
sum.stats=merge(sum.stats,
                aggregate(nr.of.sectors ~ import.utility.weight, subset(cs.multi, member.size>0 & share.world.imports>=threshold.share), function(x) round(mean(x),2)), 
                by="import.utility.weight", all=T)

sum.stats=merge(sum.stats,
                aggregate(coalition.total.trade ~ import.utility.weight, subset(cs.multi, member.size>0 & share.world.imports>=threshold.share), function(x) round(mean(x),2)), 
                by="import.utility.weight", all=T)


sum.stats=merge(sum.stats,
                aggregate(share.world.imports ~ import.utility.weight, subset(cs.multi, member.size>0 & share.world.imports>=threshold.share), function(x) round(mean(x),2)), 
                by="import.utility.weight", all=T)

sum.stats=merge(sum.stats,
                aggregate(coalition.liberalised.trade ~ import.utility.weight, subset(cs.multi, member.size>0 & share.world.imports>=threshold.share), function(x) round(mean(x),2)), 
                by="import.utility.weight", all=T)


sum.stats=merge(sum.stats,
                aggregate(share.world.imports.liberalised ~ import.utility.weight, subset(cs.multi, member.size>0 & share.world.imports>=threshold.share), function(x) round(mean(x),2)), 
                by="import.utility.weight", all=T)

sum.stats[is.na(sum.stats)]=0
names(sum.stats)=c("Import aversion","Number of deals",
                   "Average member size",
                   "Average number of sectors included (CPC 2-digit level)", 
                   "Average total coalition sectoral import value",
                   "Average total coalition sectoral import share",
                   "Average liberalised coalition sectoral import value",
                   "Average liberalised coalition sectoral import share")

xlsx::write.xlsx(sum.stats, file=paste(output.path,"/Table ",chapter.number,".1 - Summary statistics of deals per given import aversion.xlsx", sep=""), row.names = F, sheetName = paste("threshold", threshold.share), append=T)



### adding maximum liberaliser
max.stats=data.frame(import.utility.weight=unique(cs.multi$import.utility.weight), 
                     stringsAsFactors = F)

find.max=aggregate(coalition.liberalised.trade ~import.utility.weight,subset(cs.multi, member.size>0 & share.world.imports>=threshold.share), max)

my.max=c()
for(i in 1:nrow(find.max)){
  if(find.max$coalition.liberalised.trade[i]>0){
    my.max=c(my.max, 
             unique(subset(cs.multi, 
                           member.size>0 & 
                             import.utility.weight==find.max$import.utility.weight[i] &
                             coalition.liberalised.trade==find.max$coalition.liberalised.trade[i] & 
                             share.world.imports>=threshold.share)$coalition.id)[1])
  }
}



max.stats=merge(max.stats,
                aggregate(sector.scope ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), function(x) paste(unique(x), collapse=",")), 
                by="import.utility.weight", all=T)

max.stats=merge(max.stats,
                aggregate(member.size ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), sum), 
                by="import.utility.weight", all=T)
max.stats=merge(max.stats,
                aggregate(nr.of.sectors ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), sum), 
                by="import.utility.weight", all=T)

max.stats=merge(max.stats,
                aggregate(coalition.total.trade ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), sum), 
                by="import.utility.weight", all=T)


max.stats=merge(max.stats,
                aggregate(share.world.imports ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), sum), 
                by="import.utility.weight", all=T)

max.stats=merge(max.stats,
                aggregate(coalition.liberalised.trade ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), sum), 
                by="import.utility.weight", all=T)


max.stats=merge(max.stats,
                aggregate(share.world.imports.liberalised ~ import.utility.weight, subset(cs.multi, member.size>0 & coalition.id %in% my.max), sum), 
                by="import.utility.weight", all=T)


max.stats[is.na(max.stats)]=0
names(max.stats)=c("Import aversion",
                   "Sectors included in agreement",
                   "Member size",
                   "Number of sectors included (CPC 2-digit level)", 
                   "Total coalition sectoral import value",
                   "Total coalition sectoral import share",
                   "Liberalised coalition sectoral import value",
                   "Liberalised coalition sectoral import share")

xlsx::write.xlsx(max.stats, file=paste(output.path,"/Table ",chapter.number,".1 - Summary statistics of deals per given import aversion.xlsx", sep=""), row.names = F, sheetName =paste("threshold", threshold.share,"- MAX"), append=T)












## Are there cases where the liberalisation amount for a given i.weight exceeds the individual agreements?
# 
# gains.from.horse.trade=data.frame()
# 
# ## restricting to cases where there is a mulit-sectoral coalition
# rnd=1
# 
# gain.cases=subset(cs.multi, member.size>0 & share.world.imports>=threshold.share)$coalition.id
# for(c.id in gain.cases){
#   iuw=cs.multi$import.utility.weight[cs.multi$coalition.id== c.id]
# 
#   multi.sectors=cSplit(subset(cs.multi, coalition.id==c.id), which(names(cs.multi)=="sector.scope"), sep=",", direction="long")[,c("coalition.id", "sector.scope")]
# 
#   horse=merge(multi.sectors,
#               subset(cs.single, import.utility.weight==iuw & share.world.imports>=threshold.share)[,c("sector.scope","coalition.liberalised.trade")],
#               by="sector.scope", all.x=T)
# 
#   gains.from.horse.trade=rbind(gains.from.horse.trade,
#                                data.frame(coalition.id=c.id,
#                                           sector.scope=cs.multi$sector.scope[cs.multi$coalition.id== c.id],
#                                           import.utility.weight=iuw,
#                                           horse.gain=cs.multi$coalition.liberalised.trade[cs.multi$coalition.id== c.id],
#                                           individual.gain=sum(horse$coalition.liberalised.trade),
#                                           us.membership=as.numeric(840 %in% subset(cm.multi, coalition.id==c.id)$i.un),
#                                           stringsAsFactors = F)
#                                )
#   print(rnd/length(gain.cases))
#   rnd=rnd+1
# 
# }
# 
# gains.from.horse.trade$individual.gain[is.na(gains.from.horse.trade$individual.gain)]=0
# gains.from.horse.trade$abs.gain=gains.from.horse.trade$horse.gain-gains.from.horse.trade$individual.gain
# 
# gains.from.horse.trade=gains.from.horse.trade[order(-gains.from.horse.trade$abs.gain),]
# 
# gains.from.horse.trade$two.sector=NA
# for(i in 1:nrow(gains.from.horse.trade)){
#   gains.from.horse.trade$two.sector[i]=length(unlist(str_extract_all(as.character(gains.from.horse.trade$sector.scope[i]),",")))==1
# }
# 
# save(gains.from.horse.trade, file="0 report production/GTA 24/data/9 - Gains from horse trade/gfht.Rdata")
# 


## dump
# gfht.xlsx=gains.from.horse.trade
# names(gfht.xlsx)=c("Coalition ID","Sectors included","Import aversion","Sum of liberalised imports, multi-sector", "Sum of liberalised imports, single-sector", "Gain from multi-sector agreement", "USA is a member","Includes only 2 sectors")
# 
# xlsx::write.xlsx(gfht.xlsx, file=paste(output.path,"/Figure ",chapter.number,".1 - Change in liberalised world imports.xlsx", sep=""), row.names = F)
load("0 report production/GTA 24/data/9 - Gains from horse trade/gfht.Rdata")



## overview plot: economic mass
my.weights=c(0,-.25, -.4,-.5)
horse.overview=
  ggplot(subset(gains.from.horse.trade, import.utility.weight %in% my.weights), aes(x=abs.gain/1000000000, fill=as.factor(import.utility.weight), alpha=as.factor(import.utility.weight)))+
  geom_histogram(bins=200, position="identity")+
  scale_fill_manual(values=c(gta_colour$qualitative[c(1,3,5,7)]))+
  scale_alpha_manual(values=c(1,.6,.6,.6))+
  labs(x="Additional imports reformed by a multi-sectoral accord as opposed to single-sector accords in same sectors,\nbillions USD", 
       y="Number of agreements",
       fill="Degree of import aversion")+
  guides(alpha=F)+
  scale_x_continuous(limits=c(-100,4200), breaks=seq(-500,4000,500), labels=prettyNum(seq(-500,4000,500), big.mark = "'"))+
  coord_cartesian(ylim = c(0, 30))+
  gta_theme() 

horse.overview

gta_plot_saver(plot=horse.overview,
               path=output.path,
               name= paste("Figure ",chapter.number,".2 - Change in liberalised world imports", sep=""),
               eps = F,
               cairo_ps = T)


horse.overview=
  ggplot(subset(gains.from.horse.trade, import.utility.weight %in% my.weights & two.sector==T), aes(x=abs.gain/1000000000, fill=as.factor(import.utility.weight), alpha=as.factor(import.utility.weight)))+
  geom_histogram(bins=200, position="identity")+
  scale_fill_manual(values=c(gta_colour$qualitative[c(1,3,5,7)]))+
  scale_alpha_manual(values=c(1,.6,.6,.6))+
  labs(x="Additional imports reformed by a multi-sectoral accord as opposed to single-sector accords in same sectors,\nbillions USD", 
       y="Number of agreements",
       fill="Degree of import aversion")+
  guides(alpha=F)+
  scale_x_continuous(breaks=seq(0,7000,500), labels=prettyNum(seq(0,7000,500), big.mark = "'"))+
  coord_cartesian(ylim = c(0, 15))+
  gta_theme()

horse.overview

gta_plot_saver(plot=horse.overview,
               path=output.path,
               name= paste("Figure ",chapter.number,".1 - Change in liberalised world imports - 2-sector agreements only", sep=""),
               eps = F,
               cairo_ps = T)



## Critical masses of multi-sector agreements
load("0 report production/GTA 24/data/9 - Gains from horse trade/gfht - no threshold.Rdata")
mass.multi=subset(cs.multi, coalition.total.trade>0 & coalition.id %in% subset(gains.from.horse.trade.no.t, abs.gain>=0)$coalition.id)[,c("coalition.id","sector.scope","share.world.imports")]
mass.multi$type="multi"
mass.single=subset(cs.single, coalition.total.trade>0)[,c("coalition.id","sector.scope","share.world.imports")]
mass.single$type="single"

mass.comparison=rbind(mass.single, mass.multi)
mc.density=data.frame()

density.step=.05
for(i in seq(density.step,1,density.step)){
  mc.density=rbind(mc.density,
                   data.frame(bin=i-density.step/2,
                              type="multi-sector",
                              share=nrow(subset(mass.multi, share.world.imports>(i-density.step) & share.world.imports<=i))/nrow(mass.multi)))
  
  mc.density=rbind(mc.density,
                   data.frame(bin=i-density.step/2,
                              type="single-sector",
                              share=nrow(subset(mass.single, share.world.imports>(i-density.step) & share.world.imports<=i))/nrow(mass.single)))
}


critical.mass=
  ggplot(data=mc.density,aes(x=bin, y=share, fill=as.factor(type)))+
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_manual(name='Type of sectoral accord',values=c(gta_colour$qualitative[c(1,3)]))+
  scale_x_continuous(limits=c(.7,1), breaks=seq(.7,1,.1))+
  scale_y_continuous(limits=c(0,.6), sec.axis =  dup_axis(name=NULL))+
  labs(x="Share of sectoral world imports",
       y="Share of possible sectoral deals", 
       fill="Coalition   \nscope")+
  gta_theme()

  
critical.mass

gta_plot_saver(plot=critical.mass,
               path=output.path,
               name= paste("Figure ",chapter.number,".3 - Critical masses across coaltions scope", sep=""))




### FOCUS agreements

## focus plot for each iuw; pos and negative

cm.threshold=.8
focus.coalition=subset(gains.from.horse.trade, abs.gain>0 &
                         coalition.id %in% subset(cs.multi, share.world.imports>=cm.threshold)$coalition.id)
focus.coalition=merge(focus.coalition, cs.multi[,c(1,7:15)], by=c("coalition.id"), all.x=T)

focus.coalition$two.sector=NA
for(i in 1:nrow(focus.coalition)){
  focus.coalition$two.sector[i]=length(unlist(str_extract_all(as.character(focus.coalition$sector.scope[i]),",")))==1
}

focus.largest.50=75
focus.largest.50.2s=19752
# focus.50=16585

focus.largest.25=7
focus.largest.25.2s=18592

## looking for african member counts
au=country.correspondence$un_code[country.correspondence$name=="African Union"]

cm.multi$is.africa=as.numeric(cm.multi$i.un %in% au)
incl.africa=aggregate(is.africa ~  coalition.id, subset(cm.multi, type=="member"),sum)

focus.coalition=merge(focus.coalition, incl.africa, by="coalition.id", all.x=T)

# e=subset(focus.coalition, import.utility.weight==-.5 & two.sector==F)
# 
# s.4349=subset(focus.coalition, grepl("43", sector.scope) & 
#                 grepl("49", sector.scope) & 
#                 str_count(sector.scope,",")<=2 &
#                 import.utility.weight %in% c(-.4,-.45,-.5))

focus.largest.50.3s=16539


largest.special.machine.2s=21117
largest.special.machine.3s=20067
largest.special.machine.manys=75

largest.version=c(focus.largest.50.2s, focus.largest.50.3s)

special.version=c(largest.special.machine.2s,largest.special.machine.3s,largest.special.machine.manys)

fig.dot=4
for(focus.id in largest.version){
  
  focus.scope=as.character(focus.coalition$sector.scope[focus.coalition$coalition.id==focus.id])
  iuw=focus.coalition$import.utility.weight[focus.coalition$coalition.id==focus.id]
  
  sec.check=unlist( strsplit(focus.scope,","))
  
  if(single.iuw.different){
    
    single.coalitions=c()
    for(sc in sec.check){
      sc.stats=subset(cs.single, sector.scope %in% sc &
                        member.size>0)
      if(nrow(sc.stats)>0){
        
        single.coalitions=c(single.coalitions,sc.stats$coalition.id[sc.stats$import.utility.weight==min(sc.stats$import.utility.weight)])
        
      } else {
        single.coalitions=c(single.coalitions,
                            subset(cs.single, sector.scope %in% sc &
                                     import.utility.weight==iuw)$coalition.id)
      }
    }
    
  } else {
    
    single.coalitions=subset(cs.single, sector.scope %in% sec.check &
                               import.utility.weight==iuw)$coalition.id
  }
  
  
  focus.stats=subset(cs.multi,coalition.id==focus.id)[,c("coalition.id","sector.scope","member.size","coalition.total.trade","coalition.liberalised.trade", "share.world.imports","share.world.imports.liberalised" )]
  focus.stats$type="multi"
  focus.stats$sector.scope=as.character(focus.stats$sector.scope)
  fs=subset(cs.single,coalition.id %in% single.coalitions)[,c("coalition.id","sector.scope","member.size","coalition.total.trade","coalition.liberalised.trade", "share.world.imports","share.world.imports.liberalised" )]
  fs$type="single"
  focus.stats=rbind(focus.stats, fs)
  
  
  ## participation frequency
  participation=data.frame()
  for(i in single.coalitions){
    prcp=subset(cm.single, coalition.id==i)
    if(nrow(prcp)==0){
      prcp=data.frame()
    } else {
      
      prcp$member=as.numeric(prcp$type=="member")
      prcp$type="single"
      prcp=prcp[,c("coalition.id","type","i.un","member")]
    }
    
    participation=rbind(participation, prcp)
  }
  
  # participation=subset(participation, i.un %in% subset(participation, member==1)$i.un)
  participation=aggregate(member ~ i.un, participation, sum)
  
  participation.m=subset(cm.multi, coalition.id==focus.id)
  participation.m$member=as.numeric(participation.m$type=="member")*length(single.coalitions)
  
  participation=merge(participation, participation.m[,c("i.un", "member")], by="i.un", all=T)
  participation[is.na(participation)]=0
  participation$gain=participation$member.y-participation$member.x
  
  pre.max=max(participation$gain, na.rm = T)
  
  participation$gain[participation$gain==0 & participation$member.x==max(participation$gain)]=max(participation$gain)+1
  participation=participation[,c("i.un", "gain")]
  
  post.max=max(participation$gain, na.rm = T)
  
  has.full.participation=post.max>pre.max
  
  participation=rbind(subset(participation, i.un!=10007),
                      data.frame(i.un=country.correspondence$un_code[country.correspondence$name=="EU-28"],
                                 gain=participation$gain[participation$i.un==10007]))
  
  participation=rbind(subset(participation, i.un!=10008),
                      data.frame(i.un=country.correspondence$un_code[country.correspondence$name=="Eurasian Economic Union"],
                                 gain=participation$gain[participation$i.un==10008]))
  
  
  world <- gtalibrary::world.geo
  names(participation)=c("UN", "value")
  
  world = merge(world, participation[,c("UN","value")], by="UN", all.x=T)
  
  ###### IMPORTANT, sort for X (id) again
  world <-  world[with(world, order(X)),]
  world$value[is.na(world$value) == T] <- 0
  min.w=min(world$value, na.rm = T)
  max.w=max(world$value, na.rm = T)
  

  
  
  map.breaks=unique(world$value)[order(unique(world$value))]
  values.lost=map.breaks[map.breaks<0]
  values.won=map.breaks[map.breaks>0]
  
  if(min.w>=0){
    
    if(has.full.participation){
      
      map.colours=c("#dadada",
                    gta_colour$green.shades(length(values.won))[length(values.won):1],
                    gta_colour$blue[1])
      map.labels=c("full absence",values.won, "full participation\nin both scenarios")
      
    } else {
      
      map.colours=c("#dadada",
                    gta_colour$green.shades(length(values.won))[length(values.won):1])
      map.labels=c("full absence",values.won)
      
    }
    
  }else{
    
    if(has.full.participation){
      
      map.colours=c(gta_colour$red.shades(length(values.lost)),
                    "#dadada",
                    gta_colour$green.shades(length(values.won))[length(values.won):1],
                    gta_colour$blue[1])
      
      map.labels=c(values.lost, "full absence",values.won, "full participation\nin both scenarios")
      
    } else {
      
      map.colours=c(gta_colour$red.shades(length(values.lost)),
                    "#dadada",
                    gta_colour$green.shades(length(values.won))[length(values.won):1])
      map.labels=c(values.lost, "full absence",values.won)
      
    }
    

   
    
  }
  
  if(length(map.labels)==2){
    map.labels=c("Non-member", "Member (net gainer)")
    map.legend.title="Membership status"
  } else {
    
    map.legend.title="Change in the\nnumber of sectors\ncovered by the\nmulti-sectoral\nagreement"
  }
  
  
 map1=
  ggplot() +
    geom_polygon(data= subset(world, country != "Antarctica"), 
                 aes(x = long, y = lat, group = group, fill = as.factor(value)), size = 0.15, color = "white") +
    geom_polygon(data=subset(world, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#dadada", size = 0.15, colour = "white") +
    coord_fixed() + # Important to fix world map proportions
    scale_y_continuous(limits=c(-55,85))+
    scale_x_continuous(limits=c(-169,191))+
    labs(x="", y="") +
    scale_fill_manual(values = map.colours, 
                      breaks=map.breaks, 
                      position="bottom", 
                      labels=map.labels
    ) + # Set color gradient
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
    guides(fill=guide_legend(title=map.legend.title, label.position = "top"),
           ymax=guide_legend(title="size"))
  
  
  map1
  
  gta_plot_saver(plot=map1,
                 path=output.path,
                 name=paste("Figure ", chapter.number, ".",fig.dot," - Focus agreement - IW ",iuw," - sectors ",focus.scope," - participation map", sep=""),
                 width = 21,
                 height = 12)
  
  setnames(participation, "UN","un_code")
  participation=merge(participation, country.names[,c("un_code","name")], by="un_code", all.x=T)
  participation=participation[,c("name","value")]
  participation$value[participation$value==max(participation$value)]="full in both scenarios"
  
  names(participation)=c("Country", "Difference in agreements")
  xlsx::write.xlsx(participation, file=paste(output.path,"/Figure ",chapter.number,".",fig.dot," - Focus agreement - IW ",iuw," - sectors ",focus.scope," - participation map data.xlsx", sep=""), row.names = F)
  
  ## focus stats
  
  fs.xlsx=focus.stats
  fs.xlsx$type=NULL
  names(fs.xlsx)=c("Coaliton ID","Sectoral scope","Number of members","Total imports of coalition","Total liberalised imports","Coalition's share in sectoral world imports","Share of world sectoral imports liberalised")
  
  xlsx::write.xlsx(fs.xlsx, file=paste(output.path,"/Figure ",chapter.number,".",fig.dot," - Focus agreement - IW ",iuw," - sectors ",focus.scope," - Membership statistics.xlsx", sep=""), row.names = F)
  
  fig.dot=fig.dot+1
  
  
  
}

## 9.4
focus.coalition$sec.count=str_count(focus.coalition$sector.scope, ",")+1
big4=c(10007,840,156,392)
big4.in=as.data.frame(table(subset(cm.multi, i.un %in% big4 & type=="member")$coalition.id))

big4.in$all.in=big4.in$Freq==4
names(big4.in)=c("coalition.id","nr.of.4","all.4.in")
focus.coalition=merge(focus.coalition, big4.in, by="coalition.id", all.x=T)

focus.coalition$share.lib.intra=focus.coalition$intra.coalition.liberalised.trade/focus.coalition$coalition.liberalised.trade

plot=
  ggplot(subset(focus.coalition, sec.count>=3 & import.utility.weight<=-.5), aes(x=coalition.liberalised.trade/1000000000, y=share.lib.intra, size=sec.count))+
  geom_point(color=gta_colour$blue[1])+
  gta_theme()+
  scale_y_continuous(sec.axis = dup_axis(name=NULL))+
  labs(x="Total value of trade reformed by accord, billions USD", y="Share of trade reformed that\nbenefits members of the accord",
       size="Number of\nsectors")+
  theme(panel.background = element_blank(), 
        panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
        legend.position="bottom",
        axis.text.x.bottom = element_text(hjust = 0.5))



plot

gta_plot_saver(plot = plot,
               path = output.path,
               name = paste0("Figure ", chapter.number, ".6 - Agreement size & liberalised trade")
               )





