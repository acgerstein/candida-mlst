# load libraries
library(here)
library(tidyverse)

# Open files
Calb <- read_csv(here("data_in", "Calb.csv")) #7 loci
Cglab <- read_csv(here("data_in", "Cglab.csv")) #6 loci
Ctrop <- read_csv(here("data_in", "Ctrop.csv")) #6 loci
Ckrus <- read_csv(here("data_in", "Ckrus.csv")) #6 loci

names(Calb)[25] <- "ST"
names(Cglab)[21] <- "ST"
names(Ctrop)[35] <- "ST"
names(Ckrus)[23] <- "ST"

# Geographic distribution
# countries
geo_Calb <- data.frame("country"=sort(table(Calb$country)/nrow(Calb)))
geo_Cglab <- data.frame("country"=sort(table(Cglab$country)/nrow(Cglab)))
geo_Ctrop <- data.frame("country"=sort(table(Ctrop$country)/nrow(Ctrop)))
geo_Ckrus <- data.frame("country"=sort(table(Ckrus$country)/nrow(Ckrus)))

names(geo_Calb)[1] <- "country"
names(geo_Cglab)[1] <- "country"
names(geo_Ctrop)[1] <- "country"
names(geo_Ckrus)[1] <- "country"

# add continent (this is not elegant but it works)
continent <- c()
i <- 0
for(i in 1:nrow(geo_Calb)){
  print(i)
  continent[i] <- as.character(subset(Calb, country == geo_Calb$country[i])$continent[1])
}
geo_Calb$continent <- continent
geo_Calb$species <- "alb"

continent <- c()
i <- 0
for(i in 1:nrow(geo_Cglab)){
  continent[i] <- as.character(subset(Cglab, country == geo_Cglab$country[i])$continent[1])
}
geo_Cglab$continent <- continent
geo_Cglab$species <- "glab"

continent <- c()
i <- 0
for(i in 1:nrow(geo_Ctrop)){
  continent[i] <- as.character(subset(Ctrop, country == geo_Ctrop$country[i])$continent[1])
}
geo_Ctrop$continent <- continent
geo_Ctrop$species <- "trop"

continent <- c()
i <- 0
for(i in 1:nrow(geo_Ckrus)){
  continent[i] <- as.character(subset(Ckrus, country == geo_Ckrus$country[i])$continent[1])
}
geo_Ckrus$continent <- continent
geo_Ckrus$species <- "krus"

d <- rbind(geo_Calb, geo_Cglab, geo_Ckrus, geo_Ctrop)
d$col[d$continent=="Asia"] <- "goldenrod"
d$col[d$continent=="Africa"] <- "orange"
d$col[d$continent=="South America"] <- "purple"
d$col[d$continent=="North America"] <- "darkgreen"
d$col[d$continent=="Europe"] <- "cadetblue"
d$col[d$continent=="Oceania"] <- "red"
d$col[d$continent==""] <- "grey"


par(mfrow = c(2, 2), mar=c(1, 1, 1, 1), oma=c(3, 3, 1, 1))
barplot(sort(subset(d, d$species =="alb")$country.Freq), xaxt="n", yaxt="n", col = d$col, pch=19)
axis(2, las=2, pos=0)
barplot(sort(subset(d, d$species =="glab")$country.Freq), xaxt="n", yaxt="n", col = d$col, pch=19)
axis(2, las=2, pos=0)
barplot(sort(subset(d, d$species =="trop")$country.Freq), xaxt="n", yaxt="n", col = d$col, pch=19)
axis(2, las=2, pos=0)
barplot(sort(subset(d, d$species =="krus")$country.Freq), xaxt="n", yaxt="n", col = d$col, pch=19)

# continents
names(Ckrus)[2] <- "isolate"
d_cont <- rbind(Calb[,1:5], Cglab[,1:5], Ctrop[,1:5], Ckrus[,1:5])

geo_Calb_cont <- data.frame("continent"=sort(table(Calb$continent)/nrow(Calb)))
geo_Cglab_cont <- data.frame("continent"=sort(table(Cglab$continent)/nrow(Cglab)))
geo_Ctrop_cont <- data.frame("continent"=sort(table(Ctrop$continent)/nrow(Ctrop)))
geo_Ckrus_cont <- data.frame("continent"=sort(table(Ckrus$continent)/nrow(Ckrus)))

names(geo_Calb_cont)[1] <- "continent"
names(geo_Cglab_cont)[1] <- "continent"
names(geo_Ctrop_cont)[1] <- "continent"
names(geo_Ckrus_cont)[1] <- "continent"
geo_Calb_cont$species <- "alb"
geo_Cglab_cont$species <- "glab"
geo_Ctrop_cont$species <- "trop"
geo_Ckrus_cont$species <- "krus"

d_cont <- rbind(geo_Calb_cont, geo_Cglab_cont, geo_Ckrus_cont, geo_Ctrop_cont)
d_cont$col[d_cont$continent=="Asia"] <- "goldenrod"
d_cont$col[d_cont$continent=="Africa"] <- "darkorange"
d_cont$col[d_cont$continent=="South America"] <- "purple"
d_cont$col[d_cont$continent=="North America"] <- "darkgreen"
d_cont$col[d_cont$continent=="Europe"] <- "darkblue"
d_cont$col[d_cont$continent=="Oceania"] <- "red"
d_cont$col[d_cont$continent==""] <- "grey"


pdf(width=7, height=4)
par(mfrow = c(2, 2), mar=c(1, 0, 1, 1), oma=c(3, 4, 1, 1))
barplot(sort(subset(d_cont, d_cont$species =="alb")$continent.Freq), xaxt="n", yaxt="n", col = subset(d_cont, d_cont$species =="alb")$col, pch=19, ylim=c(0, 0.8))
legend("topleft", c( "unknown",  "South America", "Oceania","North America","Europe", "Asia", "Africa"), pch=22, col=c("darkorange", "goldenrod", "darkblue", "darkgreen", "red", "purple", "grey"), pt.bg=c("orange", "goldenrod", "darkblue", "darkgreen", "red", "purple", "grey"), cex=0.8, inset=c(0.05, 0.02))
axis(2, las=2, pos=0)
mtext(expression(italic("C. albicans")), side=3, adj=0.01)

barplot(sort(subset(d_cont, d_cont$species =="glab")$continent.Freq), xaxt="n", yaxt="n", col = subset(d_cont, d_cont$species =="glab")$col, pch=19, ylim=c(0, 0.8), beside=FALSE)
axis(2, pos=0, labels=FALSE)
mtext(expression(italic("C. glabrata")), side=3, adj=0.01)

barplot(sort(subset(d_cont, d_cont$species =="trop")$continent.Freq), xaxt="n", yaxt="n", col = subset(d_cont, d_cont$species =="trop")$col, pch=19, ylim=c(0, 0.8))
axis(2, las=2, pos=0)
mtext(expression(italic("C. tropicalis")), side=3, adj=0.01)

barplot(sort(subset(d_cont, d_cont$species =="krus")$continent.Freq), xaxt="n", yaxt="n", col = subset(d_cont, d_cont$species =="krus")$col, pch=19, ylim=c(0, 0.8))
axis(2, las=2, pos=0, labels=FALSE)
mtext(expression(italic("C. krusei")), side=3, adj=0.01)

mtext("Continent", outer=TRUE, side=1)
mtext("Proportion of isolates", outer=TRUE, side=2, line=3)
dev.off()

# source
sort(table(Calb$source)/nrow(Calb))
# blood = 0.23, oral = 0.15, vaginal = 0.1, oropharynx = 0.08, animal = 0.04, urine = 0.04, sputum = 0.03, faeces = 0.03, skin = 0.01, (oropharynx = 0.26)
sort(table(Cglab$source)/nrow(Cglab))
# blood = 0.44, urine = 0.06, vaginal swab = 0.04, oral = 0.03, BAL = 0.02, sputum= 0.02, (oropharynx = 0.07)
sort(table(Ctrop$anat_source)/nrow(Ctrop))
# blood = 0.33, oropharynx (incl. sputum & BAL) = 0.22, urine = 0.09, vagina = 0.06, other sterile = 0.06, other superficial = 0.04, faeces = 0.03
sort(table(Ckrus$anat_source) /nrow(Ckrus))
# blood = 0.3, animal = 0.18, oropharynx = 0.13, other superficial = 0.04, other sterile = 0.03, vagina = 0.02

# ST analysis
t_alb <- sort(table(Calb$ST)/nrow(Calb))
t_glab <- table(Cglab$ST)
t_trop <- table(Ctrop$ST)
t_krus <- table(Ckrus$ST)
t_alb/sum(table(t_alb))
table(t_glab)/sum(table(t_glab))

sort(table(Calb$ST), decreasing=TRUE)
hist(table(Calb$ST))

length(unique(Calb$sender)) #76
length(unique(Cglab$sender)) #14
length(unique(Ctrop$sender)) #25
length(unique(Ckrus$sender)) #7

# one table
names(Ckrus)[2] <- "isolate"
Calb$species <- "alb"
Cglab$species <- "glab"
Ctrop$species <- "trop"
Ckrus$species <- "krus"

d_cont <- rbind(Calb[,c(4:5, 26)], Cglab[,c(4:5, 22)], Ctrop[,c(4:5, 36)], Ckrus[,c(4:5, 24)])
d_cont$continent[is.na(d_cont$continent)] <- "unknown"
d_cont_tab <- table(d_cont$continent, d_cont$species)

pdf(width=7, height=4, family="Times")
par(mar=c(3, 5, 2, 6))
t<-barplot(prop.table(d_cont_tab, 2), col=c("darkorange", "goldenrod", "darkblue", "darkgreen", "red", "purple", "grey"), xaxt="n", yaxt="n", xlim=c(0.2, 7))
axis(2, las=2)
labels <- expression(italic("C. albicans"), italic("C. glabrata"), italic("C. tropicalis"), italic("C. krusei"))
axis(1, at=t, labels=labels, line=-1, tick=FALSE, cex.axis=0.8)
legend("bottomright", c( "unknown",  "South America", "Oceania","North America","Europe", "Asia", "Africa"), pch=22, col=c(   "grey","purple", "red",   "darkgreen", "darkblue","goldenrod","darkorange"), pt.bg=c("grey", "purple", "red",   "darkgreen", "darkblue","goldenrod","darkorange"), cex=0.8, inset=c(0.05, 0.02))

#legend("topright", c("Africa", "Asia", "Europe", "North America", "Oceania", "South America", "unknown"), pch=22, col=c("darkorange", "goldenrod", "darkblue", "darkgreen", "red", "purple", "grey"), pt.bg=c("orange", "goldenrod", "darkblue", "darkgreen", "red", "purple", "grey"), cex=0.8)
mtext("Proportion of isolates", side=2, line=3)
dev.off()

#animals
sort(table(subset(Calb, source == "animal")$country)/sum(table(subset(Calb, source == "animal")$country)))

sort(table(subset(Calb, source == "animal")$ST)/sum(table(subset(Calb, source == "animal")$ST)))
t <- table(subset(Calb, source != "animal")$ST)/sum(table(subset(Calb, source == "animal")$ST))
t["172"]
#ST172: 6.5% of animals vs.
