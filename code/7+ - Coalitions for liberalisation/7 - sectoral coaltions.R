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
p1<-ggplot(cpc2, aes(y=sector.name, x=import.utility.weight))+
  geom_tile(aes(fill=member.category))+
  scale_fill_manual(values=c("#ececec",gta_colour$green.shades(length(unique(cpc2$member.category)))[length(unique(cpc2$member.category)):1]))+
  # scale_fill_gradient(limits=c(0,max(cpc2$member.size)), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Number of\nmembers"))+
  theme(axis.text.x = element_text(angle = 90, vjust=.5),
        axis.text=element_text(family="Open Sans", size=13, colour="black")) +
  gta_theme()+
  theme(panel.background = element_blank())+
  labs(x="Relative import aversion", y="Sector name",fill="Number of\ncoalition members")


p2<-ggplot(cpc2, aes(y=sector.name, x=import.utility.weight))+
  geom_tile(aes(fill=member.category))+
  scale_fill_manual(values=c("#ececec",gta_colour$green.shades(length(unique(cpc2$member.category)))[length(unique(cpc2$member.category)):1]))+
  # geom_tile(aes(fill=member.size))+
  # scale_fill_gradient(limits=c(0,max(cpc2$member.size)), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Number of\nmembers"))+
  theme(axis.text.x = element_text(angle = 90, vjust=.5),
        axis.text=element_text(family="Open Sans", size=13, colour="black")) +
  gta_theme()+
  theme(panel.background = element_blank())+
  labs(x="Relative import aversion", y="Sector name",fill="Number of\ncoalition members")


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


### world import share
# p1<-ggplot(cpc2, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
#   geom_tile(aes(fill=share.world.imports))+
#   scale_fill_gradient(limits=c(0,1), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Share of\nworld imports\n"))+
#   theme(axis.text.x = element_text(angle = 90, vjust=.5),
#         axis.text=element_text(family="Open Sans", size=13, colour="black")) +
#   gta_theme()+
#   theme(legend.text = element_text(size=10, angle=90, vjust=0.2,hjust=1))+
#   theme(panel.background = element_blank())+
#   labs(x="Relative import aversion", y="Sector name")
# 
# 
# p2<-ggplot(cpc2, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
#   geom_tile(aes(fill=share.world.imports))+
#   scale_fill_gradient(limits=c(0,1), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Share of\nworld imports\n"))+
#   theme(axis.text.x = element_text(angle = 90, vjust=.5),
#         axis.text=element_text(family="Open Sans", size=13, colour="black")) +
#   gta_theme()+
#   theme(panel.background = element_blank())+
#   theme(legend.text = element_text(size=10, angle=90, vjust=0.2,hjust=1))+
#   labs(x="Relative import aversion", y="Sector name")
# 
# 
# g1 <- ggplot_gtable(ggplot_build(p1))
# g2 <- ggplot_gtable(ggplot_build(p2))
# 
# # overlap the panel of 2nd plot on that of 1st plot
# pp <- c(subset(g1$layout, name == "panel", se = t:r))
# g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
#                      pp$l, pp$b, pp$l)
# 
# # axis tweaks
# ia <- which(g2$layout$name == "axis-l")
# ga <- g2$grobs[[ia]]
# ax <- ga$children[[2]]
# ax$widths <- rev(ax$widths)
# ax$grobs <- rev(ax$grobs)
# ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
# g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
# g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
# 
# # draw it
# png(paste(output.path,"/Figure ",chapter.number,".2 - Sectoral agreement & share of world imports covered - ",specification," (excl trade defense).png", sep=""), width=1000, height=800, res=76)
# grid.draw(g)
# dev.off()
# 
# cairo_ps(paste(output.path,"/Figure ",chapter.number,".2 - Sectoral agreement  & share of world imports covered - ",specification," (excl trade defense).eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
# grid.draw(g)
# dev.off()




#### TO P20 sectors
### member size
# p1<-ggplot(cpc2.20, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
#   geom_tile(aes(fill=member.size))+
#   scale_fill_gradient(limits=c(0,max(cpc2.20$member.size)), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Number of\nmembers"))+
#   theme(axis.text.x = element_text(angle = 90, vjust=.5),
#         axis.text=element_text(family="Open Sans", size=13, colour="black")) +
#   gta_theme()+
#   theme(panel.background = element_blank())+
#   labs(x="Relative import aversion", y="Sector name")
# 
# 
# p2<-ggplot(cpc2.20, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
#   geom_tile(aes(fill=member.size))+
#   scale_fill_gradient(limits=c(0,max(cpc2.20$member.size)), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Number of\nmembers"))+
#   theme(axis.text.x = element_text(angle = 90, vjust=.5),
#         axis.text=element_text(family="Open Sans", size=13, colour="black")) +
#   gta_theme()+
#   theme(panel.background = element_blank())+
#   labs(x="Relative import aversion", y="Sector name")
# 
# 
# g1 <- ggplot_gtable(ggplot_build(p1))
# g2 <- ggplot_gtable(ggplot_build(p2))
# 
# # overlap the panel of 2nd plot on that of 1st plot
# pp <- c(subset(g1$layout, name == "panel", se = t:r))
# g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
#                      pp$l, pp$b, pp$l)
# 
# # axis tweaks
# ia <- which(g2$layout$name == "axis-l")
# ga <- g2$grobs[[ia]]
# ax <- ga$children[[2]]
# ax$widths <- rev(ax$widths)
# ax$grobs <- rev(ax$grobs)
# ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
# g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
# g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
# 
# # draw it
# png(paste(output.path,"/Figure ",chapter.number,".1 - TOP 20 - Sectoral agreement member size - ",specification," (excl trade defense).png", sep=""), width=1000, height=800, res=76)
# grid.draw(g)
# dev.off()
# 
# cairo_ps(paste(output.path,"/Figure ",chapter.number,".1 - TOP 20 - Sectoral agreement member size - ",specification," (excl trade defense).eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
# grid.draw(g)
# dev.off()


### world import share
## currently out
# p1<-ggplot(cpc2.20, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
#   geom_tile(aes(fill=share.world.imports))+
#   scale_fill_gradient(limits=c(0,1), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Share of\nworld imports\n"))+
#   theme(axis.text.x = element_text(angle = 90, vjust=.5),
#         axis.text=element_text(family="Open Sans", size=13, colour="black")) +
#   gta_theme()+
#   theme(legend.text = element_text(size=10, angle=90, vjust=0.2,hjust=1))+
#   theme(panel.background = element_blank())+
#   labs(x="Relative import aversion", y="Sector name")
# 
# 
# p2<-ggplot(cpc2.20, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
#   geom_tile(aes(fill=share.world.imports))+
#   scale_fill_gradient(limits=c(0,1), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Share of\nworld imports\n"))+
#   theme(axis.text.x = element_text(angle = 90, vjust=.5),
#         axis.text=element_text(family="Open Sans", size=13, colour="black")) +
#   gta_theme()+
#   theme(legend.text = element_text(size=10, angle=90, vjust=0.2,hjust=1))+
#   theme(panel.background = element_blank())+
#   labs(x="Relative import aversion", y="Sector name")
# 
# 
# g1 <- ggplot_gtable(ggplot_build(p1))
# g2 <- ggplot_gtable(ggplot_build(p2))
# 
# # overlap the panel of 2nd plot on that of 1st plot
# pp <- c(subset(g1$layout, name == "panel", se = t:r))
# g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
#                      pp$l, pp$b, pp$l)
# 
# # axis tweaks
# ia <- which(g2$layout$name == "axis-l")
# ga <- g2$grobs[[ia]]
# ax <- ga$children[[2]]
# ax$widths <- rev(ax$widths)
# ax$grobs <- rev(ax$grobs)
# ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
# g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
# g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
# 
# # draw it
# png(paste(output.path,"/Figure ",chapter.number,".2 - TOP 20 - Sectoral agreement & share of world imports covered - ",specification," (excl trade defense).png", sep=""), width=1000, height=800, res=76)
# grid.draw(g)
# dev.off()
# 
# cairo_ps(paste(output.path,"/Figure ",chapter.number,".2 - TOP 20 - Sectoral agreement  & share of world imports covered - ",specification," (excl trade defense).eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
# grid.draw(g)
# dev.off()




## Top 20 summary for share of sec trade & value of liberalisation


# share of sectoral trade
mean.critical=round(mean(cpc2$share.world.imports)-.05,1)
max.critical=round(max(cpc2$share.world.imports)-.05,1)

plot=
  ggplot()+
  geom_tile(data=cpc2, aes(y=sector.name, x=import.utility.weight, fill=as.factor(share.world.imports.cat)))+
  scale_fill_manual(values=red.amber.green(11),
                    na.value="white",
                    labels=paste(seq(0,.9,.1),"-",seq(.1,1,.1)),
                    name="Share of\nworld imports\n")+
  scale_x_continuous(breaks=c(seq(0,-.75,-.25)))+
  theme(axis.text.x = element_text(angle = 90, vjust=.5),
        axis.text=element_text(family="Open Sans", size=13, colour="black")) +
  gta_theme()+
  labs(x="Relative import aversion", y="")+
  theme(panel.background = element_blank(), 
        panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
        legend.position="bottom",
        axis.text.x.bottom = element_text(hjust = 1))



plot


gta_plot_saver(plot = plot,
               path = output.path,
               name = paste0("Figure ", chapter.number, ".2 - Critical Mass Dimension"))


# value of liberalised trade
mean.econ=round(mean(cpc2$share.world.imports.liberalised)-.05,1)
max.econ=round(max(cpc2$share.world.imports.liberalised)-.05,1)
plot=ggplot()+
  geom_tile(data=cpc2, aes(y=sector.name, x=-import.utility.weight, fill=as.factor(share.world.imports.lib.cat)))+
  scale_fill_manual(values=red.amber.green(11),
                    na.value="white",
                    labels=paste(seq(0,.9,.1),"-",seq(.1,1,.1)),
                    name="Share of\nworld imports\nliberalised")+
  scale_x_continuous(breaks=c(seq(0,.75,.25)), labels=seq(0,-.75,-.25))+
  theme(axis.text.x = element_text(angle = 90, vjust=.5),
        axis.text=element_text(family="Open Sans", size=13, colour="black")) +
  gta_theme()+
  labs(x="Relative import aversion", y="")+
  theme(panel.background = element_blank(), 
        panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
        legend.position="bottom",
        axis.text.x.bottom = element_text(hjust = 1))



plot

gta_plot_saver(plot = plot,
               path = output.path,
               name = paste0("Figure ", chapter.number, ".2 - Economic Mass Dimension"))




## map for pul & gp machinery with IW = -.5
## EU/EEU members
eu.members=country.names$un_code[country.names$is.eu==T]
eeu.members=country.names$un_code[country.names$is.eeu==T]

my.sectors=c("General-purpose machinery","Pulp, paper & printing products", "Metal ores")

for(sec in my.sectors){
  members=subset(coalition.members, coalition.id == subset(coalition.stats, 
                                                           import.utility.weight==-.25 &
                                                             sector.name==sec)$coalition.id)[,c("i.un","type")]
  
  world <- gtalibrary::world.geo
  names(members)=c("UN", "value")
  
  if(10007 %in% members$UN){
    members=rbind(members,
                  data.frame(UN=eu.members,
                             value=members$value[members$UN==10007]))
  }
  
  if(10008 %in%  members$UN){
    members=rbind(members,
                  data.frame(UN=eeu.members,
                             value=members$value[members$UN==10008]))
  }
  
  
  world = merge(world, members[,c("UN","value")], by="UN", all.x=T)
  
  ###### IMPORTANT, sort for X (id) again
  world <-  world[with(world, order(X)),]
  world$value[is.na(world$value) == T] <- "no exports"
  
  
  
  map1=
    ggplot() +
    geom_polygon(data= subset(world, country != "Antarctica"), 
                 aes(x = long, y = lat, group = group, fill = value), size = 0.15, color = "white") +
    geom_polygon(data=subset(world, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#dadada", size = 0.15, colour = "white") +
    coord_fixed() + # Important to fix world map proportions
    scale_y_continuous(limits=c(-55,85))+
    scale_x_continuous(limits=c(-169,191))+
    labs(x="", y="", fill="country\nrole") +
    scale_fill_manual(values = c(gta_colour$amber[1],gta_colour$red[1],gta_colour$green[1],
                                     gta_colour$blue[1]),
                      # labels=c("bystander","b","c","d"),
                      position="bottom")+
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
    ) 
    
  
  map1
  
  gta_plot_saver(plot=map1,
                 path=output.path,
                 name=paste("Figure ", chapter.number, ".3 - Agreement map - ",sec, sep=""),
                 width = 21,
                 height = 12)
  
  
}




