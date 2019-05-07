library(gtalibrary)
library(ggplot2)
library(data.table)
library(splitstackshape)
rm(list=ls())
setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")

gta_colour_palette()
chapter.number = 9
chapter.title = 'Gains from horse trade'
output.path = paste(chapter.number,chapter.title,sep = ' - ')
output.path=paste("0 report production/GTA 24/tables & figures/",output.path, sep="")

## load top10 simulation
load("0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results - intra-Top 10 - inward only (excl trade defense).Rdata")
cm.multi=coalition.members
cs.multi=coalition.stats

## load coalition data, restricting to import weights used in top10 simulation
load("0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results - inward only (excl Subsidies & trade defense).Rdata")
cs.single=subset(coalition.stats, import.utility.weight %in% unique(cs.multi$import.utility.weight))
cm.single=subset(coalition.members, coalition.id %in% cs.single$coalition.id)
rm(coalition.members, coalition.stats)


## Are there cases where the liberalisation amount for a given i.weight exceeds the individual agreements?

gains.from.horse.trade=data.frame()

for(c.id in cs.multi$coalition.id){
  iuw=cs.multi$import.utility.weight[cs.multi$coalition.id== c.id]
  horse=merge(cSplit(subset(cs.multi, coalition.id==c.id), which(names(cs.multi)=="sector.scope"), sep=",", direction="long")[,c("coalition.id", "sector.scope")],
              subset(cs.single, import.utility.weight==iuw)[,c("sector.scope","coalition.liberalised.trade")],
              by="sector.scope", all.x=T)
  
  gains.from.horse.trade=rbind(gains.from.horse.trade,
                               data.frame(coalition.id=c.id,
                                          sector.scope=cs.multi$sector.scope[cs.multi$coalition.id== c.id],
                                          import.utility.weight=iuw,
                                          horse.gain=cs.multi$coalition.liberalised.trade[cs.multi$coalition.id== c.id],
                                          individual.gain=sum(horse$coalition.liberalised.trade)))
  
  
}

gains.from.horse.trade$abs.gain=gains.from.horse.trade$horse.gain-gains.from.horse.trade$individual.gain

gains.from.horse.trade=gains.from.horse.trade[order(gains.from.horse.trade$abs.gain)]

## overview plot: economic mass
horse.overview= 
  ggplot(gains.from.horse.trade, aes(x=abs.gain/1000000000, fill=as.factor(import.utility.weight)))+
  geom_histogram(bins=200, position="identity", alpha=.7)+
  scale_fill_manual(values=c(gta_colour$qualitative[c(1,3,5,7)]))+
  labs(x="Difference of liberalised import value in multi-sector agreement\ncompared to the sum of the single-sector agreements,\nbillion USD ", 
       y="Nr of agreements",
       fill="Import aversion")+
  scale_x_continuous(breaks=seq(-1000,2000,200), labels=prettyNum(seq(-1000,2000,200), big.mark = "'"))+
  coord_cartesian(ylim = c(0, 100))+
  gta_theme()

horse.overview

gta_plot_saver(plot=horse.overview,
               path=output.path,
               name= paste("Figure ",chapter.number,".1 - Change in liberalised world imports", sep=""))

gfht.xlsx=gains.from.horse.trade
names(gfht.xlsx)=c("Coalition ID","Sectors included","Import aversion","Sum of liberalised imports, multi-sector", "Sum of liberalised imports, single-sector", "Gain from multi-sector agreement")

xlsx::write.xlsx(gfht.xlsx, file=paste(output.path,"/Figure ",chapter.number,".1 - Change in liberalised world imports.xlsx", sep=""), row.names = F)


## Critical masses of multi-sector agreements
mass.multi=subset(cs.multi, coalition.total.trade>0 & coalition.id %in% subset(gains.from.horse.trade, abs.gain>=0)$coalition.id)[,c("coalition.id","sector.scope","share.world.imports")]
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
  scale_fill_manual(values=c(gta_colour$qualitative[c(1,3)]))+
  labs(x="Share of sectoral world imports",
       y="Share of observed coalitions", 
       fill="Coalition   \nscope")+
  gta_theme()

  
critical.mass

gta_plot_saver(plot=critical.mass,
               path=output.path,
               name= paste("Figure ",chapter.number,".2 - Critical masses across coaltions scope", sep=""))




## focus plot for each iuw; pos and negative
cm.threshold=.8
focus.coalition=subset(gains.from.horse.trade, abs.gain>0 &
                         coalition.id %in% subset(cs.multi, share.world.imports>=cm.threshold)$coalition.id)
focus.coalition=merge(focus.coalition, cs.multi[,c(1,6:14)], by=c("coalition.id"), all.x=T)

focus.l.50=1462
focus.50=1490
focus.25=151

focus.id=focus.50
focus.scope=as.character(focus.coalition$sector.scope[focus.coalition$coalition.id==focus.id])
iuw=focus.coalition$import.utility.weight[focus.coalition$coalition.id==focus.id]

single.coalitions=subset(cs.single, sector.scope %in% unlist( strsplit(focus.scope,",")) &
                           import.utility.weight==iuw)$coalition.id

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
  prcp$member=as.numeric(prcp$type=="member")
  prcp$type="single"
  prcp=prcp[,c("coalition.id","type","i.un","member")]
  
  participation=rbind(participation, prcp)
}

# participation=subset(participation, i.un %in% subset(participation, member==1)$i.un)
participation=aggregate(member ~ i.un, participation, sum)

participation.m=subset(cm.multi, coalition.id==focus.id)
participation.m$member=as.numeric(participation.m$type=="member")*length(single.coalitions)

participation=merge(participation, participation.m[,c("i.un", "member")], by="i.un", all.x=T)
participation$gain=participation$member.y-participation$member.x

participation$gain[participation$gain==0 & participation$member.x==participation$member.y]=max(participation$gain)+1
participation=participation[,c("i.un", "gain")]

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



map1=
  ggplot() +
  geom_polygon(data= subset(world, country != "Antarctica"), 
               aes(x = long, y = lat, group = group, fill = value), size = 0.15, color = "white") +
  geom_polygon(data=subset(world, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#dadada", size = 0.15, colour = "white") +
  coord_fixed() + # Important to fix world map proportions
  scale_y_continuous(limits=c(-55,85))+
  scale_x_continuous(limits=c(-169,191))+
  labs(x="", y="") +
  scale_fill_gradientn(colours = c(gta_colour$red.shades(length(seq(min(world$value),-1,1))),
                                                          "#dadada",
                                   gta_colour$green.shades(length(seq(1,max(world$value)-1,1)))[length(seq(1,max(world$value)-1,1)):1],
                                   gta_colour$blue[1]), 
                    
                       breaks=c(seq(min(world$value),max(world$value),1)), 
                       position="bottom", 
                       labels=c(seq(min(world$value),max(world$value)-1,1), "full participation\nin both scenarios")) + # Set color gradient
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
  guides(fill=guide_legend(title=paste("Change in the\nnumber of sectors\ncovered by the agreement", sep=""), label.position = "top"),
         ymax=guide_legend(titel="size"))


map1

gta_plot_saver(plot=map1,
               path=output.path,
               name=paste("Figure ", chapter.number, ".3 - Focus agreement - participation map", sep=""),
               width = 21,
               height = 12)

setnames(participation, "UN","un_code")
participation=merge(participation, country.names[,c("un_code","name")], by="un_code", all.x=T)
participation=participation[,c("name","value")]
participation$value[participation$value==max(participation$value)]="full in both scenarios"

names(participation)=c("Country", "Difference in agreements")
xlsx::write.xlsx(participation, file=paste(output.path,"/Figure ",chapter.number,".3 - Focus agreement - participation map data.xlsx", sep=""), row.names = F)

## focus stats

fs.xlsx=focus.stats
fs.xlsx$type=NULL
names(fs.xlsx)=c("Coaliton ID","Sectoral scope","Number of members","Total imports of coalition","Total liberalised imports","Coalition's share in sectoral world imports","Share of world sectoral imports liberalised")

xlsx::write.xlsx(fs.xlsx, file=paste(output.path,"/Figure ",chapter.number,".3 - Focus agreement - Membership statistics.xlsx", sep=""), row.names = F)

