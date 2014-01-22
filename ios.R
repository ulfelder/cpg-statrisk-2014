# INTERNATIONAL ORGANIZATIONS AND TREATY REGIMES
# Jay Ulfelder
# 2014-01-21

# Note: This script calls the requisite file from a local folder, but the data set can also be downloaded from
# ICPSR here: http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/30541?q=ulfelder&searchSource=find-analyze-home

rm(list=ls(all=TRUE))

ios <- read.csv("c:/users/jay/documents/ushmm/statrisk2014/indata/ulfelder io data 2010.csv")
ios$sftgcode <- as.character(ios$pitfcode)
ios <- subset(ios, select = c(sftgcode, year, iccpr1, gattwto))
names(ios) <- c("sftgcode", "year", "io.iccpr1", "io.wto")
ios$sftgcode[ios$sftgcode=="UK"] <- "UK "

source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
rack <- merge(rack, ios, all.x = TRUE)
rack <- rack[order(rack$country, rack$year),]

# Updates source: http://treaties.un.org/Pages/ViewDetails.aspx?src=TREATY&mtdsg_no=IV-5&chapter=4&lang=en
for (i in 1:dim(rack)[1]) rack$io.iccpr1[i] <- ifelse(rack$year[i] > 2010, rack$io.iccpr1[i-1], rack$io.iccpr1[i])
rack$io.iccpr1[rack$sftgcode=="TUN" & rack$year>=2011] <- 1 # acceded in 2011
rack$io.iccpr1[rack$sftgcode=="USA"] <- 0 
rack$io.iccpr1[rack$sftgcode=="SSD"] <- 0 
rack$io.iccpr1[rack$sftgcode=="MNG"] <- 0 
rack$io.iccpr1[rack$sftgcode=="SWA"] <- 0 
rack$io.iccpr1[rack$sftgcode=="GNB" & rack$year>=2013] <- 1  # acceded in 2013

# Source: http://www.wto.org/english/thewto_e/whatis_e/tif_e/org6_e.htm
for (i in 1:dim(rack)[1]) rack$io.wto[i] <- ifelse(rack$year[i] > 2010, rack$io.wto[i-1], rack$io.wto[i])
rack$io.wto[rack$sftgcode=="MNG" & rack$year>=2012] <- 1  # acceded in 2012
rack$io.wto[rack$sftgcode=="RUS" & rack$year>=2012] <- 1  # acceded in 2012
rack$io.wto[rack$sftgcode=="SRB"] <- 0  # fill in missing
rack$io.wto[rack$sftgcode=="SSD"] <- 0  # fill in missing
rack$io.wto[rack$sftgcode=="USA"] <- 1  # fill in missing
rack$io.wto[rack$sftgcode=="LAO" & rack$year>=2013] <- 1  # acceded in 2013
rack$io.wto[rack$sftgcode=="TAJ" & rack$year>=2013] <- 1  # acceded in 2013

write.csv(rack, "c:/users/jay/documents/ushmm/statrisk2014/outdata/ios.csv", row.names = FALSE)
