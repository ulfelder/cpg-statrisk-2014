# ELITE CHARACTERISTICS
# Jay Ulfelder
# 2014-01-21

# Source: Center for Systemic Peace via PITF

# Note: These data are not posted online, but they are in the public domain.

rm(list=ls(all=TRUE))

elc <- read.csv("c:/users/jay/documents/ushmm/statrisk2014/indata/elc2012.csv")
names(elc) <- c("sftgcode", "year", "elceleth", "elceliti")
elc$sftgcode <- as.character(elc$sftgcode)
elc$sftgcode[elc$sftgcode=="UK"] <- "UK " # Fix for merging

source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
rack <- merge(rack, elc, all.x = TRUE)
rack <- rack[order(rack$country, rack$year),]

# Hard code values for United States, which source doesn't cover
rack$elceliti[rack$sftgcode=="USA" & rack$year >= min(elc$year) & rack$year <= max(elc$year)] <- 0
rack$elceleth[rack$sftgcode=="USA" & rack$year >= min(elc$year) & rack$year <= max(elc$year)] <- 0

# Create indicator for politically salient elite ethnicity: either, minority, majority
rack$elcelethc <- ifelse(rack$elceleth==1 | rack$elceleth==2, 1, ifelse(is.na(rack$elceleth)==FALSE, 0, NA) )
rack$elceleth2 <- ifelse(rack$elceleth==2, 1, ifelse(is.na(rack$elceleth)==FALSE, 0, NA) )
rack$elceleth1 <- ifelse(rack$elceleth==1, 1, ifelse(is.na(rack$elceleth)==FALSE, 0, NA) )

write.csv(rack, "c:/users/jay/documents/ushmm/statrisk2014/outdata/elc.csv", row.names=FALSE)
