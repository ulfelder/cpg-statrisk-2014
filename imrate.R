# CENSUS BUREAU INFANT MORTALITY ESTIMATES
# Jay Ulfelder
# 2014-01-21

# Source: U.S. Census Bureau International Division, via PITF

# Note: These data are not posted online, but they are in the public domain.

rm(list=ls(all=TRUE))

imrates <- read.csv("c:/users/jay/documents/ushmm/statrisk2014/indata/cnsimr2012.csv")
imrates$sftgcode <- substr(as.character(imrates$sftgcode),1,3)

source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
rack <- merge(rack, imrates, all.x = TRUE)
rack <- rack[order(rack$country, rack$year),]

# Hard code values for USA, which source doesn't cover
rack$cnsimr[rack$sftgcode=="USA"] <- rack$cnsimr[rack$sftgcode=="CAN"]
rack$xxxcimr[rack$sftgcode=="USA"] <- rack$xxxcimr[rack$sftgcode=="CAN"]

# Log of rate rel to annual global median
rack$xxxcimrln <- log(rack$xxxcimr)

write.csv(rack, "c:/users/jay/documents/ushmm/statrisk2014/outdata/imrate.csv", row.names = FALSE)
