# POLITY AND ITS DERIVATIVES
# Jay Ulfelder
# 2014-01-20

# Source: Center for Systemic Peace

rm(list=ls(all=TRUE))
require(XLConnect)
polity <- readWorksheetFromFile("c:/users/jay/documents/ushmm/statrisk2014/indata/p4v2012.xls", sheet=1)
polity <- polity[,3:21]
require(reshape)
polity <- rename(polity, c(scode = "sftgcode"))
polity <- subset(polity, select=c("sftgcode", "year", "polity", "durable", "exrec", "parcomp", "xconst"))

# Change a few country codes to match PITF standard
polity$sftgcode[polity$sftgcode=="UKG"] <- "UK "
polity$sftgcode[polity$sftgcode=="SER"] <- "SRB"
polity$sftgcode[polity$sftgcode=="MNT"] <- "MNE"
polity$sftgcode[polity$sftgcode=="GMY"] <- "GER"
polity$sftgcode[polity$sftgcode=="SSU"] <- "SSD"
polity$sftgcode[polity$sftgcode=="SDN"] <- "SUD"
polity$sftgcode[polity$sftgcode=="USR"] <- "USS"

source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
rack <- merge(rack, polity, all.x = TRUE)
rack <- rack[order(rack$country, rack$year),]

# TRANSFORMATIONS FOR MODELING
# Fearon & Laitin regime type (autocracy, anocracy, democracy)
rack$polcat[rack$polity >= -10 & rack$polity < -5] <- 1 
rack$polcat[rack$polity >= -5 & rack$polity <= 5] <- 2
rack$polcat[rack$polity > 5] <- 3
rack$polcat[rack$polity == -66 | rack$polity == -77 | rack$polity == -88 ] <- 7

# PITF AJPS regime types
rack$pitfcat[rack$polity==-66 | rack$polity==-77 | rack$polity==-88] <- "other"
rack$pitfcat[(rack$exrec >= 1 & rack$exrec <= 6) & (rack$parcomp == 1 | rack$parcomp == 2)] <- "A/F"
rack$pitfcat[(rack$exrec >= 1 & rack$exrec <= 6) &
  (rack$parcomp == 0 | rack$parcomp == 3 | rack$parcomp == 4 | rack$parcomp == 5)] <- "A/P"
rack$pitfcat[(rack$exrec == 7 | rack$exrec == 8) & (rack$parcomp == 1 | rack$parcomp == 2)] <- "A/P"
rack$pitfcat[rack$parcomp == 3 & (rack$exrec == 7 | rack$exrec==8)] <- "D/fact"
rack$pitfcat[rack$exrec == 8 & (rack$parcomp == 0 | rack$parcomp == 4 )] <- "D/P"
rack$pitfcat[rack$exrec == 7 & (rack$parcomp == 0 | rack$parcomp == 4 | rack$parcomp == 5)] <- "D/P"
rack$pitfcat[rack$exrec == 8 & rack$parcomp == 5] <- "D/F"

# Harff autocracy indicator
rack$autocracy <- ifelse(rack$polity <= 0 & rack$polity >= -10, 1,
  ifelse(is.na(rack$polity)==FALSE, 0, NA) )

# Executive constraints indicator for C&C model
rack$xconccnc <- ifelse(rack$xconst >= 3, 1, ifelse(is.na(rack$polity)==FALSE, 0, NA) )

# Dummied version of polcat for RF
rack$polcat1 <- ifelse(rack$polcat==1, 1, ifelse(is.na(rack$polcat)==TRUE, NA, 0) )
rack$polcat2 <- ifelse(rack$polcat==2, 1, ifelse(is.na(rack$polcat)==TRUE, NA, 0) )
rack$polcat3 <- ifelse(rack$polcat==3, 1, ifelse(is.na(rack$polcat)==TRUE, NA, 0) )
rack$polcat7 <- ifelse(rack$polcat==7, 1, ifelse(is.na(rack$polcat)==TRUE, NA, 0) )

# Dummied version of PITF regime type for RF
rack$pitfcat1 <- ifelse(rack$pitfcat=="A/F", 1, ifelse(is.na(rack$pitfcat)==TRUE, NA, 0) )
rack$pitfcat2 <- ifelse(rack$pitfcat=="A/P", 1, ifelse(is.na(rack$pitfcat)==TRUE, NA, 0) )
rack$pitfcat3 <- ifelse(rack$pitfcat=="D/fact", 1, ifelse(is.na(rack$pitfcat)==TRUE, NA, 0) )
rack$pitfcat4 <- ifelse(rack$pitfcat=="D/P", 1, ifelse(is.na(rack$pitfcat)==TRUE, NA, 0) )
rack$pitfcat5 <- ifelse(rack$pitfcat=="D/F", 1, ifelse(is.na(rack$pitfcat)==TRUE, NA, 0) )
rack$pitfcat6 <- ifelse(rack$pitfcat=="other", 1, ifelse(is.na(rack$pitfcat)==TRUE, NA, 0) )

# Regime stability, logged
rack$durableln <- log1p(rack$durable)

# WRITE IT OUT
write.csv(rack, "c:/users/jay/documents/ushmm/statrisk2014/outdata/polity.csv", row.names = FALSE)
