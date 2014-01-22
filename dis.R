# DISCRIMINATION
# Jay Ulfelder
# 2014-01-21

# Source: Center for Systemic Peace via PITF

# Note: These data are not posted online, but they are in the public domain.

rm(list=ls(all=TRUE))

dis <- read.csv("c:/users/jay/documents/ushmm/statrisk2014/indata/dis2012.csv")
dis <- subset(dis, select=c(COUNTRY, YEAR, DISPOTA4))
names(dis) <- c("sftgcode", "year", "dispota4")
dis$sftgcode <- as.character(dis$sftgcode)
dis$sftgcode[dis$sftgcode=="UK"] <- "UK "

source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
rack <- merge(rack, dis, all.x = TRUE)
rack <- rack[order(rack$country, rack$year),]

# Fill in uncoded series as zeroes
rack$dispota4[is.na(rack$dispota4)==TRUE & rack$year >= min(dis$year) & rack$year <= max(dis$year)] <- 0

# Log of pop share subject to state-led discrimination
rack$dispota4ln <- log1p(100*rack$dispota4)

write.csv(rack, "c:/users/jay/documents/ushmm/statrisk2014/outdata/dis.csv", row.names=FALSE)
