library(gtalibrary)
library(foreign)
library(splitstackshape)
library(haven)
library(WDI)
library(data.table)
library(ggplot2)
library(xlsx)

rm(list = ls())

#setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
#setwd('C:/Users/Kamran/Dropbox/GTA cloud')
setwd('D:/Dropbox/Dropbox/GTA cloud')

gta_colour_palette()

chapter.number = 5
chapter.title = 'Fundaments explain less and less trade growth'
output.path = paste(chapter.number,chapter.title,sep = ' - ')


##### Figure 1: R2 from trade growth regressions
fig1.data <- xlsx::read.xlsx(paste0("0 report production/GTA 24/data/", output.path, "/R2 charts.xlsx"), sheetName = "for.ggplot")

fig1 <- ggplot(fig1.data, aes(x=year, y=value, color=type))+
  geom_line(size=1.1) +
  geom_smooth(method = "lm", se = F, linetype="dashed", size=1.1) +
  gta_theme() +
  scale_color_manual(values = gta_colour$qualitative[c(1:3)]) + 
  scale_y_continuous(sec.axis = dup_axis()) +
  xlab("Year") +
  ylab("R squared of each yearly regression") +
  theme(legend.title = element_blank(),
        legend.spacing.x = unit (.2, 'cm')) +
  guides(colour = guide_legend(override.aes = list(size=2.5)))
fig1

xlsx::write.xlsx(fig1.data, row.names=FALSE, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ",chapter.number,".1 - Data.xlsx", sep=""))

gta_plot_saver(plot=fig1,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name=paste0("Figure ", chapter.number,".1 - R2 of trade growth regression"))



########################################################################################################
############################ Simonovska & Waugh (2014) #################################################
########################################################################################################


SW <- read_stata("0 report production/GTA 24/exploration/Trade cost analysis/Asymmetric trade costs.dta")
names(SW) <- c("exporter", "importer", "Value", "exporter.code", "importer.code", "trade.cost", "Year")
SW$Year <- as.factor(SW$Year)

##### Figure 2: Importing costs for G20

SW$trade.cost.weighted <- SW$trade.cost * SW$Value

stat.year <- aggregate(trade.cost.weighted ~ Year, SW, sum)
stat.year <- merge(stat.year, aggregate(Value ~ Year, SW, sum), by="Year", all.x = T)
stat.year$exporter <- "G20"

SW$exporter <- as.character(SW$exporter)
stat.country.year <- aggregate(trade.cost.weighted ~ Year + exporter, SW, sum)
stat.country.year <- merge(stat.country.year, aggregate(Value ~ Year + exporter, SW, sum), by=c("Year", "exporter"), all.x = T)

stat.country.year <- rbind(stat.year, stat.country.year)
# rm(stat.year)
stat.country.year$weighted.TC <- stat.country.year$trade.cost.weighted / stat.country.year$Value

stat.country.year$exporter[stat.country.year$exporter=="United States of America"] <- "USA"
stat.country.year$exporter <- as.factor(stat.country.year$exporter)
stat.country.year$Year <- as.character(stat.country.year$Year)
stat.country.year$Year <- as.numeric(stat.country.year$Year)
fig2 <- ggplot(subset(subset(stat.country.year, exporter %in% c("G20", "China", "USA", "Japan", "Germany"))), aes(x=Year, y=weighted.TC, color=exporter))+
  geom_line(size=1.1) +
  gta_theme() +
  ylab("Weighted trade cost\nfaced by exporter")+
  scale_y_continuous(limits = c(1.5,3.2), breaks=seq(1.5,3,.5), labels=seq(1.5,3,.5), sec.axis = dup_axis()) +
  scale_color_manual(values = gta_colour$qualitative[c(1,2,3,8,7)]) +
  theme(legend.title = element_blank(),
        legend.spacing.x = unit (.2, 'cm')) +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

fig2

fig2.data = subset(subset(stat.country.year, exporter %in% c("G20", "China", "USA", "Japan", "Germany")))
# ggsave(filename = "0 report production/GTA 24/exploration/Trade cost analysis/Trade costs_SW_overtime_exporter.png", plot = p1, device = "png")

xlsx::write.xlsx(fig2.data, row.names=FALSE, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ",chapter.number,".2 - Data.xlsx", sep=""))

gta_plot_saver(plot=fig2,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name=paste0("Figure ", chapter.number,".2 - Exporting costs for G20"))




##### Figure 3: Importing costs for G20

stat.year <- aggregate(trade.cost.weighted ~ Year, SW, sum)
stat.year <- merge(stat.year, aggregate(Value ~ Year, SW, sum), by="Year", all.x = T)
stat.year$importer <- "G20"

SW$importer <- as.character(SW$importer)
stat.country.year <- aggregate(trade.cost.weighted ~ Year + importer, SW, sum)
stat.country.year <- merge(stat.country.year, aggregate(Value ~ Year + importer, SW, sum), by=c("Year", "importer"), all.x = T)

stat.country.year <- rbind(stat.year, stat.country.year)
# rm(stat.year)
stat.country.year$weighted.TC <- stat.country.year$trade.cost.weighted / stat.country.year$Value

stat.country.year$importer[stat.country.year$importer=="United States of America"] <- "USA"
stat.country.year$importer <- as.factor(stat.country.year$importer)
stat.country.year$Year <- as.character(stat.country.year$Year)
stat.country.year$Year <- as.numeric(stat.country.year$Year)
fig3 <- ggplot(subset(subset(stat.country.year, importer %in% c("G20", "China", "USA", "Japan", "Germany"))), aes(x=Year, y=weighted.TC, color=importer))+
  geom_line(size = 1.1) +
  scale_y_continuous(limits = c(1.5,3.2), breaks=seq(1.5,3,.5), labels=seq(1.5,3,.5), sec.axis = dup_axis()) +
  gta_theme() +
  ylab("Weighted trade cost\nfaced by importer") +
  scale_color_manual(values = gta_colour$qualitative[c(1,2,3,8,7)]) +
  theme(legend.title = element_blank(),
        legend.spacing.x = unit (.2, 'cm')) +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

fig3
fig3.data = subset(subset(stat.country.year, importer %in% c("G20", "China", "USA", "Japan", "Germany")))
# ggsave(filename = "0 report production/GTA 24/exploration/Trade cost analysis/Trade costs_SW_overtime_importer.png", plot = p1, device = "png")

xlsx::write.xlsx(fig3.data, row.names=FALSE, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ",chapter.number,".3 - Data.xlsx", sep=""))

gta_plot_saver(plot=fig3,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name=paste0("Figure ", chapter.number,".3 - Importing costs for G20"))




# p1 <- ggplot()+
#   geom_density(data = subset(SW, Year %in% c(1995,2005,2017)), aes(x=trade.cost, color=Year, fill=Year), alpha=0) +
#   geom_vline(data=aggregate(trade.cost ~ Year, subset(SW, Year %in% c(1995,2005,2017)), median), aes(xintercept = trade.cost, color=Year), show.legend = F) +
#   gta_theme() +
#   xlab("Trade Cost") +
#   scale_colour_manual(values=gta_colour$qualitative[c(1,3,4)])+
#   theme(legend.title = element_blank(),
#         legend.spacing.x = unit (.3, 'cm'))
# p1
# ggsave(filename = "0 report production/GTA 24/exploration/Trade cost analysis/Trade costs_SW.png", plot = p1, device = "png")
# 
# 
# 
# 
# p1 <- ggplot(subset(SW, Year %in% c(1995, 2005, 2017)), aes(x=trade.cost, color=Year))+
#   stat_ecdf(geom = "step") +
#   gta_theme() +
#   xlab("Trade Cost")+
#   ylab("Cumulative share") +
#   scale_colour_manual(values=gta_colour$qualitative[c(1,3,4)])+
#   theme(legend.title = element_blank(),
#         legend.spacing.x = unit (.3, 'cm'))
# p1
# ggsave(filename = "0 report production/GTA 24/exploration/Trade cost analysis/Trade costs_SW_cdf.png", plot = p1, device = "png")



##### Novy method archived!!!

# ###############################################################################################
# ##########################  Trade Cost calculation acc. Novy ##################################
# ###############################################################################################
# excluded.hs.codes <- c(1:15, 25:27)
# load(file = "0 report production/GTA 24/exploration/Trade cost analysis/Regression dataset.Rdata")
# master <- subset(master, !(hs2 %in% excluded.hs.codes))
# master.manufacturing <- aggregate(Value ~ Reporter.un + Reporter.jurisdiction + Partner.un + Partner.jurisdiction + Year + RTA, master, sum)
# master.manufacturing$Value[master.manufacturing$Value==0] <- 1000000 #this is zero because trade is so small. setting it to 1 million is obviously an approximation but it won't matter anyway and i dont want zeros
# exports.by.country <- aggregate(Value ~ Partner.un + Year, master, sum)
# names(exports.by.country) <- c("i.un", "Year", "manuf.exports.i")
# rm(master)
# 
# manufacturing.unido <- read.xlsx("0 report production/GTA 24/exploration/Trade cost analysis/imputed_output_data.xlsx", sheetName = "G20")
# names(manufacturing.unido) <- c("Year", "iso3c", "output")
# country.iso.un <- read.csv("R help files/country_iso_un2.csv", sep = ";")
# country.iso.un <- unique(subset(subset(country.iso.un, G20==1 & ISO!="na"), select = c("ISO", "UN")))
# names(country.iso.un) <- c("iso3c", "Partner.un")
# manufacturing.unido <- merge(manufacturing.unido, country.iso.un, by="iso3c", all.x = T)
# rm(country.iso.un)
# manufacturing.unido$iso3c <- NULL
# 
# 
# novy <- expand.grid(i=unique(master.manufacturing$Reporter.jurisdiction), j=unique(master.manufacturing$Reporter.jurisdiction), Year=unique(master.manufacturing$Year))
# novy <- subset(novy, i != j)
# # 1. add bilateral trade in other direction
# names(master.manufacturing) <- c("i.un", "i", "j.un", "j", "Year", "RTA", "trade.ji")
# novy <- merge(novy, master.manufacturing, by=c("i", "j", "Year"), all.x = T)
# names(master.manufacturing) <- c("j.un", "j", "i.un", "i", "Year", "RTA", "trade.ij")
# novy <- merge(novy, subset(master.manufacturing, select = c("i", "j", "Year", "trade.ij")), by=c("i", "j", "Year"), all.x = T)
# rm(master.manufacturing)
# 
# # 2. add total exports to calculate later Xii and Xjj
# novy <- merge(novy, exports.by.country, by=c("i.un", "Year"), all.x = T)
# names(exports.by.country) <- c("j.un", "Year", "manuf.exports.j")
# novy <- merge(novy, exports.by.country, by=c("j.un", "Year"), all.x = T)
# rm(exports.by.country)
# 
# # 3. add manufacturing output
# names(manufacturing.unido) <- c("Year", "output.i", "i.un")
# novy <- merge(novy, manufacturing.unido, by=c("i.un", "Year"), all.x = T)
# names(manufacturing.unido) <- c("Year", "output.j", "j.un")
# novy <- merge(novy, manufacturing.unido, by=c("j.un", "Year"), all.x = T)
# rm(manufacturing.unido)
# 
# novy$X.ii <- novy$output.i - novy$manuf.exports.i
# novy$X.jj <- novy$output.j - novy$manuf.exports.j
# novy$trade.cost <- ((novy$X.ii * novy$X.jj)/(novy$trade.ij * novy$trade.ji))^(1/6) -1
# nrow(subset(novy, is.na(trade.cost)))
# 
# 
# novy$Year <- as.factor(novy$Year)
# p1 <- ggplot()+
#   geom_density(data = subset(novy, Year %in% c(1995,2005,2017)), aes(x=trade.cost, color=Year)) +
#   geom_vline(data=aggregate(trade.cost ~ Year, subset(novy, Year %in% c(1995,2005,2017)), median), aes(xintercept = trade.cost, color=Year))
#   # geom_vline(data=aggregate(trade.cost ~ Year, subset(novy, Year %in% c(1995,2005,2017)), function(x) quantile(x, probs = .25)), aes(xintercept = trade.cost, color=Year), linetype="dashed") + 
#   # geom_vline(data=aggregate(trade.cost ~ Year, subset(novy, Year %in% c(1995,2005,2017)), function(x) quantile(x, probs = .75)), aes(xintercept = trade.cost, color=Year), linetype="dashed")
# p1
# ggsave(filename = "0 report production/GTA 24/exploration/Trade cost analysis/Trade costs_novy.png", plot = p1, device = "png")
# 
# p1 <- ggplot(subset(novy, Year %in% c(1995,2005,2017)), aes(x=trade.cost, color=Year))+
#   stat_ecdf(geom = "step")
# p1
# ggsave(filename = "0 report production/GTA 24/exploration/Trade cost analysis/Trade costs_novy_cdf.png", plot = p1, device = "png")
# 
# 
# novy$trade <- novy$trade.ij + novy$trade.ij
# novy$trade.cost.weighted <- novy$trade.cost * novy$trade
# 
# stat.year <- aggregate(trade.cost.weighted ~ Year, novy, sum)
# stat.year <- merge(stat.year, aggregate(trade ~ Year, novy, sum), by="Year", all.x = T)
# stat.year$i <- "G20"
# 
# stat.country.year <- aggregate(trade.cost.weighted ~ Year + i, novy, sum)
# stat.country.year <- merge(stat.country.year, aggregate(trade ~ Year + i, novy, sum), by=c("Year", "i"), all.x = T)
# 
# stat.country.year <- rbind(stat.year, stat.country.year)
# # rm(stat.year)
# stat.country.year$weighted.TC <- stat.country.year$trade.cost.weighted / stat.country.year$trade
# 
# stat.country.year$i <- as.factor(stat.country.year$i)
# stat.country.year$Year <- as.character(stat.country.year$Year)
# stat.country.year$Year <- as.numeric(stat.country.year$Year)
# p1 <- ggplot(subset(subset(stat.country.year, i %in% c("G20", "China", "United States of America", "Japan", "Germany"))), aes(x=Year, y=weighted.TC, color=i))+
#   geom_line()
# p1
# ggsave(filename = "0 report production/GTA 24/exploration/Trade cost analysis/Trade costs_overtime.png", plot = p1, device = "png")
# 
# 
# 
# 
# 
