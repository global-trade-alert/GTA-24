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

gta_setwd()

## general settings
incl.agriculture=F
incl.ets=F
update.sec.imports=F


gta_colour_palette()
red.amber.green=colorRampPalette(c(gta_colour$harmful[1], gta_colour$amber[1], gta_colour$liberalising[1]))


chapter.number = 7
chapter.title = 'Sectoral coalitions for liberalisation'
output.path = paste(chapter.number,chapter.title,sep = ' - ')
output.path=paste("0 report production/GTA 24/tables & figures/",output.path, sep="")

## definitions
ets.interventions=c(63250,63254)
agri.sectors=gta_cpc_code_expand(c(1,2,3,4,21,22,23))
agri.products=cpc.to.hs$hs[cpc.to.hs$cpc %in% agri.sectors]

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

if(update.sec.imports){
  se.xlsx=sectoral.imports
  names(se.xlsx)=c("CPC 2 sector","Sector name (shortened)","World imports in 2017", "Share of total 2017 world imports")
  xlsx::write.xlsx(se.xlsx, file="0 report production/GTA 24/tables & figures/7 - Coalitions for liberalisation/Sectoral world trade in 2017.xlsx", row.names=F)
  
}


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




## top 20
sectoral.imports=sectoral.imports[order(-sectoral.imports$total.imports),]
short.order=sectoral.imports$cpc.short[order(-sectoral.imports$total.imports)]

setnames(sectoral.imports, "cpc","sector.scope")
cpc2=merge(cpc2, sectoral.imports[,c("sector.scope","share")], by="sector.scope", all.x=T)
setnames(sectoral.imports,"sector.scope", "cpc")

cpc2$sector.name=factor(cpc2$sector.name, levels=unique(cpc2$sector.name)[order(unique(cpc2$share))])

cpc2$member.category="none"
cpc2$member.category[cpc2$member.size<=49& cpc2$member.size>0]="up to 50"
cpc2$member.category[cpc2$member.size<=99 & cpc2$member.size>49]="50 - 99"
cpc2$member.category[cpc2$member.size>99]="100 or more"
cpc2$member.category=factor(cpc2$member.category, 
                            levels=c("none","up to 50","50 - 99","100 or more"))

bracket=1
cpc2$share.world.imports.cat=1
for(brk in seq(0.1,1,.1)){
  
  cpc2$share.world.imports.cat[cpc2$share.world.imports>(brk-.1) & cpc2$share.world.imports<=brk] =bracket
  bracket=bracket+1
  
}

bracket=1
cpc2$share.world.imports.lib.cat=1
for(brk in seq(0.1,1,.1)){
  
  cpc2$share.world.imports.lib.cat[cpc2$share.world.imports.liberalised>(brk-.1) & cpc2$share.world.imports.liberalised<=brk] =bracket
  bracket=bracket+1
  
}



cpc2.20=subset(cpc2, sector.scope %in% sectoral.imports$cpc[1:20])

#### ALL sectors
### member size
p1=
  ggplot(cpc2, aes(y=sector.name, x=import.utility.weight))+
  geom_tile(aes(fill=member.category))+
  scale_fill_manual(values=c("#ececec",gta_colour$green.shades(length(unique(cpc2$member.category)))[length(unique(cpc2$member.category)):1]))+
  # scale_fill_gradient(limits=c(0,max(cpc2$member.size)), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Number of\nmembers"))+
  theme(axis.text.x = element_text(angle = 90, vjust=.5),
        axis.text=element_text(family="Open Sans", size=13, colour="black")) +
  gta_theme()+
  theme(panel.background = element_blank())+
  labs(x="Relative import aversion", y="Sector",fill="Number of net beneficiaries\nfrom a single-sector accord")


p2<-ggplot(cpc2, aes(y=sector.name, x=import.utility.weight))+
  geom_tile(aes(fill=member.category))+
  scale_fill_manual(values=c("#ececec",gta_colour$green.shades(length(unique(cpc2$member.category)))[length(unique(cpc2$member.category)):1]))+
  # geom_tile(aes(fill=member.size))+
  # scale_fill_gradient(limits=c(0,max(cpc2$member.size)), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Number of\nmembers"))+
  theme(axis.text.x = element_text(angle = 90, vjust=.5),
        axis.text=element_text(family="Open Sans", size=13, colour="black")) +
  gta_theme()+
  theme(panel.background = element_blank())+
  labs(x="Relative import aversion", y="Sector",fill="Number of net beneficiaries\nfrom a single-sector accord")


g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it


png(paste(output.path,"/Figure ",chapter.number,".1 - Sectoral agreement member size.png", sep=""), width=1000, height=800, res=76)
grid.draw(g)
dev.off()

cairo_ps(paste(output.path,"/Figure ",chapter.number,".1 - Sectoral agreement member size.eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
grid.draw(g)
dev.off()

