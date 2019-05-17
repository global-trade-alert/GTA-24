library(gtalibrary)
library(ggplot2)
rm(list = ls())


# font_import()
loadfonts(device="postscript")
loadfonts(device="win")

setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")

#Settings
chapter.number=11
chapter.name="What's new"
output.path=paste("0 report production/GTA 24/tables & figures/", paste(chapter.number, chapter.name, sep=" - "),sep="")
source("0 report production/GTA 24/help files/GTA 23 cutoff and definitions.R")

### THE GTA standard colour palette
gta_colour_palette()

# Please prepare a chart showing the quarter by quarter totals of the number of policy interventions included in the GTA database from Q3 2009 to Q2 2018.

# FIGURE 13.1, total amount in database per quarter

gta_data_slicer(keep.implementation.na = T)

# FIGURE 13.1, total aggregate amount in database per quarter

agg.interventions <- master.sliced
agg.interventions$quarter <- quarter(agg.interventions$date.published)
agg.interventions$year <- year(agg.interventions$date.published)

fig10.1 <- data.frame(total =numeric(),
                      quarter=numeric(),
                      year=numeric())

for(y in 2008:2019) {
  for (q in 1:4) {
    total = length(unique(subset(agg.interventions, year<=y-1 | (year==y & quarter <= q))$intervention.id))
    x <- data.frame(total = total,
                    quarter = q,
                    year = y)
    fig10.1 <- rbind(fig10.1, x)
    rm(x)

  }
}

fig10.1 <- subset(fig10.1, ! ((year==2019 & quarter > 2) |(year==2009 & quarter <= 1) | (year==2008)))

fig10.1$quarter.name <- paste("Q",fig10.1$quarter," - ", fig10.1$year, sep="")

fig10.1 <- fig10.1[with(fig10.1, order(year, quarter)),]
fig10.1$quarter.name <- as.factor(fig10.1$quarter.name)
row.names(fig10.1) <- NULL

fig10.1.xlsx <- fig10.1[,c("total","quarter.name","quarter","year")]
names(fig10.1.xlsx) <- c("Number of interventions","Quarter Name", "Quarter", "Year")


xlsx::write.xlsx(fig10.1.xlsx, file=paste(output.path,"/Table ",chapter.number,".1  - Data for Figure ",chapter.number,".1.xlsx", sep=""), row.names=F)

fig10.1$quarter.name.2 <- gsub("\\s","",as.character(fig10.1$quarter.name))
fig10.1$quarter.name.2[fig10.1$quarter==1] <- ""
fig10.1$quarter.name.2[fig10.1$quarter==3] <- ""

p2 <- ggplot()+
  geom_line(data=fig10.1, aes(x=forcats::fct_inorder(quarter.name), y=total, group = 1), colour=gta_colour$blue[1], size=1)+
  geom_text(data=fig10.1[1,], aes(x=quarter.name, y=1400, label=total), colour=gta_colour$blue[1], nudge_x = 1)+
  geom_text(data=fig10.1[nrow(fig10.1),], aes(x=quarter.name, y=20100, label=total), colour=gta_colour$blue[1], nudge_x = -3.5)+
  xlab("Quarter")+
  scale_x_discrete(labels=fig10.1$quarter.name.2)+
  ylab("Number of interventions documented since GTA launch")+
  scale_y_continuous(limits=c(-100, 21000), breaks=seq(1000,21000,1000), sec.axis = sec_axis(~., name="Number of interventions documented since GTA launch", breaks=seq(1000, 21000, 1000)), expand = c(0,0))+
  gta_theme(x.bottom.angle = 90)+
  theme(axis.text.x = element_text(size=10, vjust=0.5, hjust=0),
        axis.title.y.left = element_text(size=10),
        axis.title.y.right = element_text(size=10),
        line = element_line(lineend = "round"))

p2

gta_plot_saver(plot=p2,
               path=output.path,
               name=paste("Figure ",chapter.number,".1 - Sum of intervention types from beginning to quarter", sep=""))


