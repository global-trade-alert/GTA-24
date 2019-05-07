library(gtalibrary)
library(ggplot2)
library(gtable)
library(grid)
library(data.table)
library(gridExtra)
rm(list=ls())
setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")

gta_colour_palette()
chapter.number = 7
chapter.title = 'Sectoral coalitions for liberalisation'
output.path = paste(chapter.number,chapter.title,sep = ' - ')
output.path=paste("0 report production/GTA 24/tables & figures/",output.path, sep="")

## Choosing trade data
gta_trade_value_bilateral(trade.data="2017",df.name="trade")
setnames(trade, "hs6","affected.product")
total.imports=aggregate(trade.value ~ affected.product, trade, sum)
setnames(total.imports, "trade.value","total.imports")

hs2cpc=cpc.to.hs
names(hs2cpc)=c("cpc","affected.product")

total.imports=merge(total.imports, hs2cpc, by="affected.product", all.x=T)
total.imports$cpc2=as.numeric(substr(sprintf(fmt = "%03i",total.imports$cpc),1,2))

sectoral.imports=aggregate(total.imports ~ cpc2, total.imports, sum)
sectoral.imports$share=sectoral.imports$total.imports/sum(trade$trade.value)
cpc.shorts=read.csv("0 report production/GTA 24/help files/cpc names shortened.csv", sep=";")
setnames(sectoral.imports, "cpc2", "cpc")

sectoral.imports=merge(sectoral.imports, cpc.shorts[,c("cpc","cpc.short")])
sectoral.imports$cpc.short=gsub("\\\\n"," ",sectoral.imports$cpc.short)
sectoral.imports=sectoral.imports[,c(1,4,2,3)]

se.xlsx=sectoral.imports
names(se.xlsx)=c("CPC 2 sector","Sector name (shortened)","World imports in 2017", "Share of total 2017 world imports")
xlsx::write.xlsx(se.xlsx, file="0 report production/GTA 24/tables & figures/7 - Coalitions for liberalisation/Sectoral world trade in 2017.xlsx", row.names=F)

####### CHOOSE SPECIFICATION

specification="inward only"

for(specification in c("all interventions","inward only","inward without subsidies")){
  
  
  if(specification=="all interventions") {
    ## all barriers
    load("0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results.Rdata")  
  }
  
  
  if(specification=="inward only") {
    ## inward barriers
    load("0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results - inward only (excl Trade defense).Rdata")
    
  }
  
  if(specification=="inward without subsidies") {
    
    ## inward barriers w/o subsidies
    load("0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results - inward only (excl Subsidies & trade defense).Rdata")
  }
  
  cpc.shorts=read.csv("0 report production/GTA 24/help files/cpc names shortened.csv", sep=";")
  setnames(cpc.shorts, "cpc", "sector.scope")
  
  cpc2=subset(coalition.stats, sector.level==2)[c("coalition.id","sector.scope","import.utility.weight","member.size","coalition.liberalised.trade","share.world.imports", "share.world.imports.liberalised")]
  cpc2=merge(cpc2, cpc.shorts[,c("sector.scope", "cpc.short","cpc.order")], by="sector.scope", all.x=T)
  cpc2$sector.name=gsub("\\\\n"," ",cpc2$cpc.short)
  # cpc2$sector.name=paste(gsub("\\\\n"," ",cpc2$cpc.short), " (CPC ",cpc2$sector.scope,")", sep="")
  
  ## top 20
  sectoral.imports=sectoral.imports[order(-sectoral.imports$total.imports),]
  cpc2.20=subset(cpc2, sector.scope %in% sectoral.imports$cpc[1:20])
  
  #### ALL sectors
  ### member size
  p1<-ggplot(cpc2, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
    geom_tile(aes(fill=member.size))+
    scale_fill_gradient(limits=c(0,max(cpc2$member.size)), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Number of\nmembers"))+
    theme(axis.text.x = element_text(angle = 90, vjust=.5),
          axis.text=element_text(family="Open Sans", size=13, colour="black")) +
    gta_theme()+
    theme(panel.background = element_blank())+
    labs(x="Relative import utility", y="Sector name")
  
  
  p2<-ggplot(cpc2, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
    geom_tile(aes(fill=member.size))+
    scale_fill_gradient(limits=c(0,max(cpc2$member.size)), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Number of\nmembers"))+
    theme(axis.text.x = element_text(angle = 90, vjust=.5),
          axis.text=element_text(family="Open Sans", size=13, colour="black")) +
    gta_theme()+
    theme(panel.background = element_blank())+
    labs(x="Relative import utility", y="Sector name")
  
  
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
  png(paste(output.path,"/Figure ",chapter.number,".1 - Sectoral agreement member size - ",specification," (excl trade defense).png", sep=""), width=1000, height=800, res=76)
  grid.draw(g)
  dev.off()
  
  cairo_ps(paste(output.path,"/Figure ",chapter.number,".1 - Sectoral agreement member size - ",specification," (excl trade defense).eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
  grid.draw(g)
  dev.off()
  
  
  ### world import share
  p1<-ggplot(cpc2, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
    geom_tile(aes(fill=share.world.imports))+
    scale_fill_gradient(limits=c(0,1), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Share of\nworld imports\n"))+
    theme(axis.text.x = element_text(angle = 90, vjust=.5),
          axis.text=element_text(family="Open Sans", size=13, colour="black")) +
    gta_theme()+
    theme(panel.background = element_blank())+
    labs(x="Relative import utility", y="Sector name")
  
  
  p2<-ggplot(cpc2, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
    geom_tile(aes(fill=share.world.imports))+
    scale_fill_gradient(limits=c(0,1), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Share of\nworld imports\n"))+
    theme(axis.text.x = element_text(angle = 90, vjust=.5),
          axis.text=element_text(family="Open Sans", size=13, colour="black")) +
    gta_theme()+
    theme(panel.background = element_blank())+
    labs(x="Relative import utility", y="Sector name")
  
  
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
  png(paste(output.path,"/Figure ",chapter.number,".2 - Sectoral agreement & share of world imports covered - ",specification," (excl trade defense).png", sep=""), width=1000, height=800, res=76)
  grid.draw(g)
  dev.off()
  
  cairo_ps(paste(output.path,"/Figure ",chapter.number,".2 - Sectoral agreement  & share of world imports covered - ",specification," (excl trade defense).eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
  grid.draw(g)
  dev.off()
  
  
  
  
  #### TO P20 sectors
  ### member size
  p1<-ggplot(cpc2.20, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
    geom_tile(aes(fill=member.size))+
    scale_fill_gradient(limits=c(0,max(cpc2.20$member.size)), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Number of\nmembers"))+
    theme(axis.text.x = element_text(angle = 90, vjust=.5),
          axis.text=element_text(family="Open Sans", size=13, colour="black")) +
    gta_theme()+
    theme(panel.background = element_blank())+
    labs(x="Relative import utility", y="Sector name")
  
  
  p2<-ggplot(cpc2.20, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
    geom_tile(aes(fill=member.size))+
    scale_fill_gradient(limits=c(0,max(cpc2.20$member.size)), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Number of\nmembers"))+
    theme(axis.text.x = element_text(angle = 90, vjust=.5),
          axis.text=element_text(family="Open Sans", size=13, colour="black")) +
    gta_theme()+
    theme(panel.background = element_blank())+
    labs(x="Relative import utility", y="Sector name")
  
  
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
  png(paste(output.path,"/Figure ",chapter.number,".1 - TOP 20 - Sectoral agreement member size - ",specification," (excl trade defense).png", sep=""), width=1000, height=800, res=76)
  grid.draw(g)
  dev.off()
  
  cairo_ps(paste(output.path,"/Figure ",chapter.number,".1 - TOP 20 - Sectoral agreement member size - ",specification," (excl trade defense).eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
  grid.draw(g)
  dev.off()
  
  
  ### world import share
  p1<-ggplot(cpc2.20, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
    geom_tile(aes(fill=share.world.imports))+
    scale_fill_gradient(limits=c(0,1), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Share of\nworld imports\n"))+
    theme(axis.text.x = element_text(angle = 90, vjust=.5),
          axis.text=element_text(family="Open Sans", size=13, colour="black")) +
    gta_theme()+
    theme(panel.background = element_blank())+
    labs(x="Relative import utility", y="Sector name")
  
  
  p2<-ggplot(cpc2.20, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight))+
    geom_tile(aes(fill=share.world.imports))+
    scale_fill_gradient(limits=c(0,1), low="white", high=gta_colour$green[1], na.value=gta_colour$green[1], name=paste("Share of\nworld imports\n"))+
    theme(axis.text.x = element_text(angle = 90, vjust=.5),
          axis.text=element_text(family="Open Sans", size=13, colour="black")) +
    gta_theme()+
    theme(panel.background = element_blank())+
    labs(x="Relative import utility", y="Sector name")
  
  
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
  png(paste(output.path,"/Figure ",chapter.number,".2 - TOP 20 - Sectoral agreement & share of world imports covered - ",specification," (excl trade defense).png", sep=""), width=1000, height=800, res=76)
  grid.draw(g)
  dev.off()
  
  cairo_ps(paste(output.path,"/Figure ",chapter.number,".2 - TOP 20 - Sectoral agreement  & share of world imports covered - ",specification," (excl trade defense).eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
  grid.draw(g)
  dev.off()
  
  
  
  
  ## Top 20 summary for share of sec trade & value of liberalisation
  
  
  # share of sectoral trade
  mean.critical=round(mean(cpc2.20$share.world.imports)-.05,1)
  max.critical=round(max(cpc2.20$share.world.imports)-.05,1)
  
  plot=ggplot()+
    geom_tile(data=cpc2.20, aes(y=forcats::fct_inorder(sector.name), x=import.utility.weight, fill=share.world.imports))+
    scale_fill_gradientn(colours = c(gta_colour$harmful[1], gta_colour$amber[1], gta_colour$liberalising[1]),
                         breaks=c(0,mean.critical, max.critical),
                         na.value="white",
                         name="Share of\nworld imports\n")+
    scale_x_continuous(breaks=c(seq(0,-.75,-.25)))+
    theme(axis.text.x = element_text(angle = 90, vjust=.5),
          axis.text=element_text(family="Open Sans", size=13, colour="black")) +
    gta_theme()+
    labs(x="Relative import utility", y="")+
    theme(panel.background = element_blank(), 
          panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
          legend.position="bottom",
          axis.text.x.bottom = element_text(hjust = 1))
  
  
  
  plot
  
  
  gta_plot_saver(plot = plot,
                 path = output.path,
                 name = paste0("Figure ", chapter.number, ".3 - Critical Mass Dimension - ",specification," (excl trade defense)"))
  
  
  # value of liberalised trade
  mean.econ=round(mean(cpc2.20$share.world.imports.liberalised)-.05,1)
  max.econ=round(max(cpc2.20$share.world.imports.liberalised)-.05,1)
  plot=ggplot()+
    geom_tile(data=cpc2.20, aes(y=forcats::fct_inorder(sector.name), x=-import.utility.weight, fill=share.world.imports.liberalised))+
    scale_fill_gradientn(colours = c(gta_colour$harmful[1], gta_colour$amber[1], gta_colour$liberalising[1]),
                         breaks=c(0,mean.econ,max.econ),
                         na.value="white",
                         name="Share of\nworld imports\nliberalised")+
    scale_x_continuous(breaks=c(seq(0,.75,.25)), labels=seq(0,-.75,-.25))+
    theme(axis.text.x = element_text(angle = 90, vjust=.5),
          axis.text=element_text(family="Open Sans", size=13, colour="black")) +
    gta_theme()+
    labs(x="Relative import utility", y="")+
    theme(panel.background = element_blank(), 
          panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
          legend.position="bottom",
          axis.text.x.bottom = element_text(hjust = 1))
  
  
  
  plot
  
  gta_plot_saver(plot = plot,
                 path = output.path,
                 name = paste0("Figure ", chapter.number, ".3 - Economic Mass Dimension - ",specification," (excl trade defense)"))
  
}

