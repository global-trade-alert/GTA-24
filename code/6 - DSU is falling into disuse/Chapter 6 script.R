rm(list=ls())

library(gtalibrary)
library(data.table)
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(lubridate)


## Pick-thy-folder
#setwd("C:/Users/Johannes Fritz/Dropbox/GTA/GTA cloud")
#setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
# setwd("/Users/piotrlukaszuk/Dropbox/GTA cloud")
#setwd('C:/Users/Kamran/Dropbox/GTA cloud')
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")
setwd('D:/Dropbox/Dropbox/GTA cloud')

chapter.nr = 6
chapter.name = '6 - DSU is falling into disuse'
output.path = paste0("0 report production/GTA 24/tables & figures/",chapter.name)
data.path = paste0('0 report production/GTA 24/data/',chapter.name)


df.DSU.per.year = readxl::read_xlsx(paste0(data.path,'/DSU cases per year.xlsx'))
df.DSU.by.complainant = readxl::read_xlsx(paste0(data.path,'/DSU cases by complainant.xlsx'))
country.descriptions = read.csv2('R help files/country_iso_un.csv')

source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")
g20.members <- as.numeric(g20.members)
gta_colour_palette()

###### TABLE 1 ######
# From the WTO website please collect data (since 1995) on (a) the number 
# of WTO DSU cases started each year and (b) the number of WTO DSU cases started 
# each year that involve a G20 member bringing a case against a G20 member.

# 1 --------------------------------------------------------------

df.DSU.per.year$row.id = 1:nrow(df.DSU.per.year)
df.DSU.per.year = df.DSU.per.year[(df.DSU.per.year$row.id %% 3 == 1), ]

total.DSU.per.year = colSums(!is.na(df.DSU.per.year[,!colnames(df.DSU.per.year) == 'row.id']))

df.DSU.name.per.year = matrix(NA, nrow=nrow(df.DSU.per.year), ncol=ncol(df.DSU.per.year[,names(df.DSU.per.year) != 'row.id']))

for (year in names(df.DSU.per.year[,names(df.DSU.per.year) != 'row.id'])){
  col.idx = grep(year, colnames(df.DSU.per.year))
  df.DSU.name.per.year[,col.idx] = stringi::stri_extract_first_words(df.DSU.per.year[[year]])
}

colnames(df.DSU.name.per.year) = names(df.DSU.per.year[,names(df.DSU.per.year) != 'row.id'])

g20.member.names <- append(g20.member.names, "European Union")
g20.member.names <- append(g20.member.names, "European Communities")
g20.member.names[g20.member.names == "South Korea"] <- "Korea, Republic of"
g20.member.names[g20.member.names == "Russia"] <- "Russian Federation"
g20.member.names[g20.member.names == "United States of America"] <- "United States"
g20.member.names[g20.member.names == "Saudi Arabia"] <- "Saudi Arabia, Kingdom of"

colnames(df.DSU.by.complainant)[1:3] = c('Complainant','Respondent', 'Disputes')
df.DSU.by.complainant[df.DSU.by.complainant == "European Union (formerly EC)"] <- "European Union"

df.DSU.by.complainant$row.id = 1:nrow(df.DSU.by.complainant)
condition.1 = which(df.DSU.by.complainant$row.id %% 2 == 1)
condition.2 = which(df.DSU.by.complainant$row.id %% 2 == 0)
df.DSU.by.complainant[condition.1,3] = df.DSU.by.complainant[condition.2,3]
df.DSU.by.complainant = df.DSU.by.complainant[condition.1, 1:3]

#fill in NA values with most recent appearance
goodIdx <- !is.na(df.DSU.by.complainant$Complainant)
goodVals <- c(NA, df.DSU.by.complainant$Complainant[goodIdx])
fillIdx <- cumsum(goodIdx)+1
df.DSU.by.complainant$Complainant = goodVals[fillIdx]
df.DSU.by.complainant.g20 = df.DSU.by.complainant[(df.DSU.by.complainant$Complainant %in% g20.member.names)&(df.DSU.by.complainant$Respondent %in% g20.member.names),]

g20.DSU.codes = paste(df.DSU.by.complainant.g20$Disputes,collapse=", ")
g20.DSU.codes = stringr::str_trim(unlist(strsplit(g20.DSU.codes,',')))
g20.DSU.codes = unique(g20.DSU.codes)

df.DSU.name.per.year = as.data.frame(df.DSU.name.per.year)

g20.total.DSU.per.year = vector()

for (year in names(df.DSU.per.year[,names(df.DSU.per.year) != 'row.id'])){
  col.idx = grep(year, colnames(df.DSU.per.year))
  g20.total.DSU.per.year[col.idx] = length(intersect(g20.DSU.codes,df.DSU.name.per.year[[year]]))
}
names(g20.total.DSU.per.year) = names(df.DSU.per.year[,names(df.DSU.per.year) != 'row.id'])


total.DSU.per.year <- as.data.frame(total.DSU.per.year)
total.DSU.per.year <- setDT(total.DSU.per.year, keep.rownames = TRUE)[]
names(total.DSU.per.year) <- c("Year","Value")

g20.total.DSU.per.year <- as.data.frame(g20.total.DSU.per.year)
g20.total.DSU.per.year <- setDT(g20.total.DSU.per.year, keep.rownames = TRUE)[]
names(g20.total.DSU.per.year) <- c("Year","Value")

save(g20.total.DSU.per.year, total.DSU.per.year, file="0 report production/GTA 24/data/6 - DSU is falling into disuse/DSU cases.Rdata")

DSU.xlsx <- list("Total" = total.DSU.per.year,
                 "G20" = g20.total.DSU.per.year)

# NOT in final report
# write.xlsx(DSU.xlsx, file=paste0(output.path, "/Table ", chapter.nr,".1 - DSU cases.xlsx"), row.names = F, col.names = T)




###### TABLE 2 ######

# Please compute for each year since 1995 the trillions of US dollar trade between the G20 members.

# 2 -----------------------------------------------------------------------

# LOAD DATA
load('data/comtrade/comtrade replica 1991-2004 - all HS vintages.Rdata')
load("data/support tables/Final goods support table.Rdata")
source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")
g20.members = as.numeric(as.character(g20.members))


# 1991 - 2004
head(tradedata)
# SUBSET TO G20 MEMBERS
length(unique(subset(tradedata, i.un %in% g20.members)$i.un))
trade.g20.1 <- subset(tradedata, i.un %in% g20.members & a.un %in% g20.members)
# AGGREGATE SUM OF TRADE PER YEAR
trade.g20.1 <- aggregate(Trade.Value~Period, trade.g20.1, function(x) sum(x))

# 2005 - 2017

head(final)
length(unique(subset(final, Reporter.un %in% g20.members)$Reporter.un))
# SUBSET TO G20 MEMBERS
trade.g20.2 <- subset(final, Reporter.un %in% g20.members & Partner.un %in% g20.members)
# AGGREGATE SUM OF TRADE PER YEAR
trade.g20.2 <- aggregate(Value~Year, trade.g20.2, function(x) sum(x))


# RBIND
names(trade.g20.1) <- c("Year","Value")
names(trade.g20.2) <- c("Year","Value")

trade.g20 <- rbind(trade.g20.1, trade.g20.2)
trade.g20$Value <- trade.g20$Value/1000000000000
names(trade.g20) <- c("Year", "Intra-G20 trade in trillion USD")
save(trade.g20, file="0 report production/GTA 24/data/6 - DSU is falling into disuse/trade g20.Rdata")
load(paste0(data.path,"/trade g20.Rdata"))
# NOT in final report
# write.xlsx(trade.g20, file=paste0(output.path,"/Table ", chapter.nr,".2 - G20 trade.xlsx"), row.names = F)

# trade.g20$`Intra-G20 trade in trillion USD`*1000000000000


###### TABLE 3 ######

# Please plot a chart of (a) and (b) and of the cumulative share of (b), where
# the cumulative share is calculated using the denominator total number DSU cases
# since 1995 started before the end of year X.

load(paste0(data.path,"/DSU cases.Rdata"))

# CALCULATE THE CUMULATIVE CASES EACH YEAR FROM THE YEARS BEFORE
g20.total.sum <- g20.total.DSU.per.year
g20.total.sum$Value <- cumsum(g20.total.sum$Value)
g20.total.sum <- merge(g20.total.sum, total.DSU.per.year, by="Year", all = T)
g20.total.sum$Value.y <- cumsum(g20.total.sum$Value.y)
g20.total.sum$share <- g20.total.sum$Value.x / g20.total.sum$Value.y
g20.total.sum <- g20.total.sum[,c(1,4)]
names(g20.total.sum) <- c("Year", "Value")

g20.total.sum$type = "share"
g20.total.DSU.per.year$type = "g20"
total.DSU.per.year$type = "total"

fig3 <- rbind(g20.total.DSU.per.year, g20.total.sum, total.DSU.per.year)
fig3$Year <- as.numeric(fig3$Year)

fig3.xlsx <- fig3
write.xlsx(fig3.xlsx, file=paste0(output.path,"/Table for Figure ",chapter.nr,".1.xlsx"), row.names = F)
fig3$Value[fig3$type=="share"] <-fig3$Value[fig3$type=="share"] *50
# PLOT
plot3 <- ggplot()+
  geom_line(data=fig3, aes(x=Year, y=Value, color=type), size=1) +
  gta_plot_wrapper(data=fig3, y.right.transform = 1/50,
                   data.x = "Year",
                   data.y = "Value",
                   # y.right.enable = T,
                   y.left.name = "Number of DSU cases",
                   y.right.name = "Share of cumulative G20 cases\nover total cases",
                   x.bottom.breaks = seq(1995,2019,1),
                   colour.palette = gta_colour$qualitative[c(1:3)],
                   x.bottom.name = "Year",
                   colour.labels = c("By a G20 member brought against a G20 member (LHS)","Share of cumulative sums of G20 over total (RHS)","Total (LHS)"),
                   colour.legend.title = "Complainant")+
  gta_theme(x.bottom.angle = 45)

plot3

gta_plot_saver(plot = plot3,
               path = output.path,
               name = paste0("Figure ", chapter.nr,".1 - DSU complaints"))



###### FIGURE 4 ######

# Please plot a chart of (b) and the ratio of (b) to the number
# of trillions of US dollars of trade between the G20 members.

# MERGE DSU AND TRADE DATA
g20.total.DSU.per.year$Year <- as.numeric(g20.total.DSU.per.year$Year)
fig4 <- merge(g20.total.DSU.per.year, trade.g20, by="Year", all.x=T)
fig4 <- fig4[,-c("type")]
names(fig4) <- c("Year","DSU","Trade in trillion USD")

# REMOVE NA ROWS
fig4 <- subset(fig4, is.na(`Trade in trillion USD`)==F)
# fig4$DSU.trade.ratio <- fig4$DSU / fig4$`Trade in trillion USD`
fig4$DSU.trade.ratio <- fig4$`Trade in trillion USD` / fig4$DSU

write.xlsx(fig4, file=paste0(output.path,"/Table for Figure ",chapter.nr,".2.xlsx"), row.names=F)
fig4.plot <- gather(fig4, type, value, c("DSU","Trade in trillion USD","DSU.trade.ratio"))


plot4 <- ggplot()+
  geom_line(data=subset(fig4.plot, type == "DSU"), aes(x=Year, y=value, colour=type), size=1)+
  geom_line(data=subset(fig4.plot, type == "DSU.trade.ratio"), aes(x=Year, y=value/(1.5/30), colour=type), size=1)+
  gta_plot_wrapper(data=fig4.plot,
                   data.x="Year",
                   data.y="value",
                   x.bottom.name = "Year",
                   x.bottom.breaks = c(seq(1995,2010,5),2017),
                   y.left.name = "Number of new DSU\ncomplaints by G20 members",
                   y.right.enable = T,
                   y.right.transform = (1.5/30),
                   y.right.limits = c(0,1.5),
                   y.right.breaks = c(seq(0,1.5,0.5)),
                   y.right.name = "Ratio of intra-G20 trade in trillion USD\nto new DSU complaints between G20 members",
                   colour.labels = c("Number of DSU complaints","Ratio of intra-G20 trade to complaints"),
                   colour.legend.title = NULL)+
  gta_theme()

plot4  

gta_plot_saver(plot = plot4,
               path = output.path,
               name = paste0("Figure ",chapter.nr,".2"))


###### FIGURE 5 ######

# Prepare the following four heat maps, each using the same colour 
# scale so they are comparable. That???s one map each for the situation 
# on 31 December on (i) 2009, (ii) 2012, (iii) 2015 and (iv) 2018. 
# Excluding the EU members of the G20 and replacing them with the EU28, 
# prepare a matrix where each G20 member is a row and column. The rows 
# reflect affected G20 members. The columns reflect the 
# implementing G20 members.

# Please calculate on each date the total number of red and amber 
# measures still in effect that the G20 implementer has in place that 
# affect the G20 victim. Choose the colour coding so that zero is 
# represented by green, yellow is represented by very low numbers, 
# orange by higher numbers, and red by the cases where there are many 
# hits by one G20 member on another. I want to be able to compare 
# across the heat maps.

# GET DATA OF AFFECTED COUNTRIES
# gta_data_slicer(gta.evaluation = c("Red","Amber"),
#                 implementing.country = c(g20.members, "EU-28"),
#                 keep.implementer = T,
#                 affected.country = c(g20.members, "EU-28"),
#                 keep.affected = T,
#                 nr.affected.incl = "SELECTED",
#                 keep.others = F)

# CALCULATE TRADE COVERAGES
coverages <- data.frame(implementing=character(),
                        affected=character(),
                        share=numeric(),
                        year = numeric())


country.set = c(g20.member.names, "EU-28")
country.set = country.set[! country.set %in% c("Germany","France","United Kingdom","Italy")]
country.set[country.set == "South Korea"] <- "Republic of Korea"

imp = country.set[1]
aff = country.set[2]
year = 2018

for(year in c(2009, 2012, 2015, 2018)){
  
  gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                     coverage.period = c(year, year),
                     implementer.role = c("Importer"),
                     implementation.period = c("2008-11-01", paste0(year,"-12-31")),
                     revocation.period = c(paste0(year+1,"-01-01"), "2111-11-11"),
                     keep.revocation.na = T,
                     importers = country.set,
                     group.importers = F,
                     separate.importer.groups = T,
                     keep.importers = T,
                     exporters = country.set,
                     separate.exporter.groups = T,
                     group.exporters = F,
                     keep.exporters = T,
                     intervention.ids = c(70350, 18891, 16819, 71578, 58794, 18254, 13633, 15366, 13512, 18892),
                     keep.interventions = F)
  
  coverages <- rbind(coverages, data.frame(implementing = trade.coverage.estimates[,1],
                                           affected = trade.coverage.estimates[,2],
                                           share = trade.coverage.estimates[,ncol(trade.coverage.estimates)],
                                           year = year))
}


coverages <- subset(coverages, implementing %in% country.set & affected %in% country.set)
save(coverages, file=paste0(data.path,"/coverages.Rdata"))
load(file=paste0(data.path,"/coverages.Rdata"))

country.names = c("Argentina",
                  "Australia",
                  "Brazil",
                  "Canada",
                  "China",
                  "EU-28",
                  "India",
                  "Indonesia",
                  "Japan",
                  "Mexico",
                  "Republic of Korea",
                  "Russia",
                  "Saudi Arabia",
                  "South Africa",
                  "Turkey",
                  "United States of America")

country.df <- data.frame(affected= country.names,
                         number = c(seq(1,16,1)))

master <- coverages

# REMOVE EU-EU INTRA TRADE
master <- master[master$affected != master$implementing,]

# SAVE XLSX
master.xlsx <- spread(master, year, share)
write.xlsx(master.xlsx, file=paste0(output.path,"/Table for Figure ",chapter.nr,".3.xlsx"), sheetName = "Trade coverages", row.names = F)

master.xlsx[is.na(master.xlsx)] <- 999
master <- gather(master.xlsx, year, share, 3:ncol(master.xlsx))

# ADD SAUDI ARABIA TO ARGENTINA MANUALLY, AS NO ROW IS EXISTING
master <- rbind(master, data.frame(implementing = "Argentina",
                                   affected = "Saudi Arabia",
                                   year  = c(2009, 2012, 2015, 2018),
                                   share = c(999,999,999,999)))

# ADD NUMERIC COLS FOR PLOTTIG
master <- merge(master, country.df, by="affected", all.x=T)
setnames(master, "number","affected.num")
setnames(country.df, "affected","implementing")
master <- merge(master, country.df, by="implementing", all.x=T)
setnames(master, "number","implementing.num")

country.names = c("Argentina",
                  "Australia",
                  "Brazil",
                  "Canada",
                  "China",
                  "European Union",
                  "India",
                  "Indonesia",
                  "Japan",
                  "Mexico",
                  "South Korea",
                  "Russia",
                  "Saudi Arabia",
                  "South Africa",
                  "Turkey",
                  "USA")

i = 2009

for (i in c(2009,2012,2015,2018)) {
  
  # FOR CHECKING
  test <- subset(master, year == i)
  head(test[with(test, order(-share)),])
  
  plot <- ggplot()+
    geom_tile(data=subset(master, year == i & share == 999), aes(x=implementing.num, y=affected.num), fill="#1e6530", color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(data=subset(master, year == i & share != 999), aes(x=implementing.num, y=affected.num, fill=share), color="#FFFFFF", size=0.2, na.rm = F)+
    gta_theme(x.bottom.angle = 45)+
    scale_fill_gradientn(name="Percentage of bilateral exports \nfacing importer\'s trade distortions", 
                         colours = c(gta_colour$green[2], gta_colour$green[2], "#ffcc00", gta_colour$amber[2], gta_colour$red[1]), values=c(0,0.2,0.25,0.5,1), 
                         breaks=c(0,0.2,0.5,0.8,max(subset(master, year == i & share != 999)$share)), labels=c("0%","20%","50%","70%", paste0('   ',scales::percent(max(subset(master, year == i & share != 999)$share)))),
                         limits=c(0,max(subset(master, year == i & share != 999)$share)),
                         guide=guide_colorbar(barwidth=13, title.position = "bottom", hjust=1, label.hjust=0.3))+
    scale_y_continuous(breaks=seq(1,length(unique(country.df$number)),1), labels = country.names, sec.axis = sec_axis(~., breaks=seq(1,length(unique(country.df$number)),1), labels = country.names, name = "Affected country"))+
    scale_x_continuous(breaks=seq(1,length(unique(country.df$number)),1), labels = country.names)+
    labs(x="Implementing country",y="Affected country")+
    theme(panel.background = element_blank(), 
          panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
          legend.position="bottom",
          axis.text.x.bottom = element_text(hjust = 1),
          legend.text = element_text(size = 10)
          )
  
  plot
  
  gta_plot_saver(plot = plot,
                 path = output.path,
                 name = paste0("Figure ", chapter.nr, ".3 - ",i))
  
}

# FIGURE OUT DIFFERENCES BETWEEN IMPORTER EXPORTER COMBINATIONS
master.2018 <- subset(master, year == 2018)
master.2018$share[master.2018$share == 999] <- 0

set = c(1:16)
i = 1
# b = partners[2]
master.2018$share.complement = 0
for (i in c(1:16)) {
  partners <- set[-i]
  for (b in partners) {
    if (length(master.2018$share[master.2018$affected.num == b & master.2018$implementing.num == i])>0) {
      master.2018$share.complement[master.2018$affected.num == i & master.2018$implementing.num == b] <- master.2018$share[master.2018$affected.num == b & master.2018$implementing.num == i]
    }
  }
}

master.2018$type <- "Red"
master.2018$type.order <- 1

master.2018$type[master.2018$share <= 0.25 & master.2018$share.complement <= 0.25] <- "Yellow"
master.2018$type.order[master.2018$share <= 0.25 & master.2018$share.complement <= 0.25] <- 2

master.2018$type[master.2018$share <= 0.125 & master.2018$share.complement <= 0.125] <- "Green"
master.2018$type.order[master.2018$share <= 0.125 & master.2018$share.complement <= 0.125] <- 3

master.2018.xlsx <- master.2018[,c("implementing","affected","share","year","type")]
master.2018.xlsx <- write.xlsx(x = master.2018.xlsx, file=paste0(output.path,"/Table for Figure ",chapter.nr,".3 - Bilateraly Affected Trade.xlsx"), row.names = F)

master.2018$type.order <- as.character(master.2018$type.order)

max.value = max(master.2018$share)
plot <- ggplot()+
  geom_tile(data=master.2018, aes(x=implementing.num, y=affected.num, fill=type.order), color="#FFFFFF", size=0.2, na.rm = F)+
  gta_theme(x.bottom.angle = 45)+
  scale_fill_manual(name="Percentage of bilateral trade affected", values=c(gta_colour$red[1], "#ffcc00",gta_colour$green[2]), labels=c(">25%","< 25%", "< 12.5%"),
                    guide=guide_legend(title.position = "bottom", hjust=1))+
  scale_y_continuous(breaks=seq(1,length(unique(country.df$number)),1), labels = country.names, sec.axis = sec_axis(~., breaks=seq(1,length(unique(country.df$number)),1), labels = country.names, name = "Affected country"))+
  scale_x_continuous(breaks=seq(1,length(unique(country.df$number)),1), labels = country.names)+
  labs(x="Implementing country",y="Affected country")+
  theme(panel.background = element_blank(), 
        panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
        legend.position="bottom",
        axis.text.x.bottom = element_text(hjust = 1))


plot

gta_plot_saver(plot = plot,
               path = output.path,
               name = paste0("Figure ", chapter.nr, ".3 - 2018 - Bilateraly Affected Trade"))

test = master[master$implementing == 'Japan',]
test = test[test$year == 2018,]

# I have one request concerning Graph 6.5 Bilateraly affected trade. Using the latest 
# year of trade data available please calculate the share and total amount of intra-G20 
# trade that is (a) in green cells and (b) in yellow cells.

# SUBSET MASTER 2018 SET
yellow <- subset(master.2018, type == "Yellow")
countries.yellow <- yellow$implementing

green <- subset(master.2018, type == "Green")
countries.green <- green$implementing

# Load 2017 trade data
# load("data/comtrade/comtrade 2017 no corrections.RData")
load("data/support tables/Final goods support table.Rdata")
trade.2017 <- subset(final, Year == 2017)


# Total G20 Trade
head(trade.2017)

# ADD EUROPEAN UNION TO G20 MEMBER SET
g20.member.names.EU <- append(g20.member.names, "European Union")
g20.member.names.EU <- g20.member.names.EU[! g20.member.names.EU %in% c("United Kingdom", "France", "Germany", "Italy")]
g20.members.EU <- append(g20.members, 10028)
g20.members.EU <- g20.members.EU[! g20.members.EU %in% c(276, 251, 381, 826)]

# GET EU CODES
correspondence <- gtalibrary::country.correspondence
countries <- gtalibrary::country.names
eu <- correspondence$un_code[correspondence$name == "EU-28" & correspondence$un_code %in% unique(countries$un_code)]

trade.2017$Partner.jurisdiction <- as.character(trade.2017$Partner.jurisdiction)
trade.2017$Reporter.jurisdiction <- as.character(trade.2017$Reporter.jurisdiction)

trade.2017$Partner.jurisdiction[trade.2017$Partner.un %in% eu] <- "European Union"
trade.2017$Reporter.jurisdiction[trade.2017$Reporter.un %in% eu] <- "European Union"
trade.2017$Partner.un[trade.2017$Partner.un %in% eu] <- 10028
trade.2017$Reporter.un[trade.2017$Reporter.un %in% eu] <- 10028

# MUST BE 16
length(unique(trade.2017$Partner.un[trade.2017$Partner.un %in% g20.members.EU]))

trade.2017 <- subset(trade.2017, Reporter.un %in% g20.members.EU & Partner.un %in% g20.members.EU)
# trade.2017 <- subset(trade.2017, Partner.Code %in% g20.members & Reporter.Code %in% g20.members)
trade.2017 <- aggregate(Value~Partner.jurisdiction+Reporter.jurisdiction, trade.2017, function(x) sum(x))
trade.2017 <- trade.2017[trade.2017$Partner.jurisdiction != trade.2017$Reporter.jurisdiction,]


trade.2017$Partner.jurisdiction[trade.2017$Partner.jurisdiction == "South Korea"] <- "Republic of Korea"
trade.2017$Reporter.jurisdiction[trade.2017$Reporter.jurisdiction == "South Korea"] <- "Republic of Korea"
names(trade.2017) <- c("affected","implementing","value")

# ALL TRADE
total.trade.G20.EU = sum(trade.2017$value)

# ALL TRADE WITH EU EXCLUDED FROM G20
load(paste0(data.path,"/trade g20.Rdata"))
total.trade.G20 = trade.g20$`Intra-G20 trade in trillion USD`[trade.g20$Year == 2017]*1000000000000


# Calculate total trade, and yellow/green trade share
results <- data.frame(type = character(),
                      absolute.g20 = numeric(),
                      share.g20 = numeric(),
                      absolute.g20.EU = numeric(),
                      share.g20.EU = numeric())


results <- rbind(results, data.frame(type = "All G20 trade",
                                     absolute.g20 = total.trade.G20,
                                     share.g20 = 1,
                                     absolute.g20.EU = total.trade.G20.EU,
                                     share.g20.EU = 1))

# YELLOW TRADE

trade.yellow <- merge(yellow, trade.2017, by=c("affected","implementing"), all.x=T)
trade.yellow$value[is.na(trade.yellow$value)] <- 0
results <- rbind(results, data.frame(type = "Trade where bilateraly affected < 25%",
                                     absolute.g20 = sum(trade.yellow$value),
                                     share.g20 = sum(trade.yellow$value)/total.trade.G20,
                                     absolute.g20.EU = sum(trade.yellow$value),
                                     share.g20.EU = sum(trade.yellow$value)/total.trade.G20.EU))


# GREEN TRADE

trade.green <- merge(green, trade.2017, by=c("affected","implementing"), all.x=T)
trade.green$value[is.na(trade.green$value)] <- 0
results <- rbind(results, data.frame(type = "Trade where bilateraly affected < 12.5%",
                                     absolute.g20 = sum(trade.green$value),
                                     share.g20 = sum(trade.green$value)/total.trade.G20,
                                     absolute.g20.EU = sum(trade.green$value),
                                     share.g20.EU = sum(trade.green$value)/total.trade.G20.EU))

names(results) <- c("Type","Absolute Values excl. EU", "Share excl. EU", "Absolute Values incl. EU","Shares incl. EU")
write.xlsx(results, file=paste0(output.path,"/Table ",chapter.nr,".3 - Bilateraly Affected Trade Share.xlsx"), row.names = F, sheetName = "Shares")

