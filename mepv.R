# MAJOR EPISODES OF POLITICAL VIOLENCE DATA
# Jay Ulfelder
# 2014-01-21

# Source: Center for Systemic Peace

rm(list=ls(all=TRUE))

# Load the MPEV data set
require(XLConnect)
mepv <- readWorksheetFromFile("c:/users/jay/documents/ushmm/statrisk2014/indata/mepv2012.xls", sheet=1)
mepv$ccode <- mepv$country <- NULL
require(reshape)
mepv <- rename(mepv, c(scode="sftgcode"))

# Modify some country codes to match for merging
mepv$sftgcode[mepv$sftgcode=="GMY"] <- "GER"
mepv$sftgcode[mepv$sftgcode=="MNT"] <- "MNE"
mepv$sftgcode[mepv$sftgcode=="SER"] <- "SRB"
mepv$sftgcode[mepv$sftgcode=="SSU"] <- "SSD"
mepv$sftgcode[mepv$sftgcode=="SDN"] <- "SUD"
mepv$sftgcode[mepv$sftgcode=="UKG"] <- "UK "

source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
rack <- merge(rack, mepv, all.x = TRUE)

regyrsum <- as.data.frame( cbind( seq(min(rack$year), max(rack$year), 1),
  tapply(rack$actotal, list(rack$year, rack$region), sum, na.rm=TRUE) ) )
names(regyrsum) <- c("year", "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9",           # regions
                     "r12", "r14", "r23", "r25", "r41", "r50", "r53", "r56", "r67", "r89", "r90",  # straddle states
                     "r99")                                                                        # isolated states

rack <- merge(rack, regyrsum, all.x = TRUE)
rack <- rack[order(rack$country, rack$year),]

# Create sum of armed conflict intensity for other countries in same subregion
rack$regac <- NA
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==0,
  sum(rack$r0[i], rack$r50[i], rack$r90[i], na.rm=TRUE) - rack$actotal[i], rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==1,
  sum(rack$r1[i], rack$r12[i], rack$r14[i], rack$r41[i], na.rm=TRUE) - rack$actotal[i], rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==2,
  sum(rack$r2[i], rack$r12[i], rack$r23[i], rack$r25[i], rack$r53[i], na.rm=TRUE) - rack$actotal[i], rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==3,
  sum(rack$r3[i], rack$r23[i], rack$r41[i], rack$r53[i], na.rm=TRUE) - rack$actotal[i], rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==4,
  sum(rack$r4[i], rack$r14[i], rack$r41[i], na.rm=TRUE) - rack$actotal[i], rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==5,
  sum(rack$r5[i], rack$r25[i], rack$r50[i], rack$r53[i], rack$r56[i], na.rm=TRUE) - rack$actotal[i], rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==6,
  sum(rack$r6[i], rack$r56[i], rack$r67[i], na.rm=TRUE) - rack$actotal[i], rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==7,
  sum(rack$r7[i], rack$r67[i], na.rm=TRUE) - rack$actotal[i], rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==8,
  sum(rack$r8[i], rack$r89[i], na.rm=TRUE) - rack$actotal[i], rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==9,
  sum(rack$r9[i], rack$r89[i], rack$r90[i], na.rm=TRUE) - rack$actotal[i], rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==12,
  sum(rack$r1[i], rack$r2[i], na.rm=TRUE), rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==14,
  sum(rack$r1[i], rack$r4[i], na.rm=TRUE), rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==23,
  sum(rack$r2[i], rack$r3[i], na.rm=TRUE), rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==25,
  sum(rack$r2[i], rack$r5[i], na.rm=TRUE), rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==41,  # DRC only
  sum(rack$r1[i], rack$r3[i], rack$r4[i], na.rm=TRUE), rack$regac[i] )  
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==50,
  sum(rack$r5[i], rack$r0[i], na.rm=TRUE), rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==53,  # Sudan only
  sum(rack$r2[i], rack$r3[i], rack$r5[i], na.rm=TRUE), rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==56,
  sum(rack$r5[i], rack$r6[i], na.rm=TRUE), rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==67,
  sum(rack$r6[i], rack$r7[i], na.rm=TRUE), rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==89,
  sum(rack$r8[i], rack$r9[i], na.rm=TRUE), rack$regac[i] )
for (i in 1:dim(rack)[1]) rack$regac[i] <- ifelse(rack$region[i]==90,
  sum(rack$r9[i], rack$r0[i], na.rm=TRUE), rack$regac[i] )
rack$regac[rack$region==99] <- 0

# Logged version of regional conflict measure
rack$regacln <- log1p(rack$regac)

# Create a binary indicator for any civil conflict
rack$civconc <- ifelse(rack$civtot > 0 & is.na(rack$civtot)==FALSE, 1, ifelse(rack$civtot==0, 0, NA) )

# Create log of civil conflict score
rack$civconln <- log1p(rack$civtot)

# Subset to variables of interest
rack <- subset(rack, select=c(1:17,40:43))

write.csv(rack, "c:/users/jay/documents/ushmm/statrisk2014/outdata/mepv.csv", row.names=FALSE)
