# PITF PROBLEM SET WITH ONSET INDICATORS AND DURATION CLOCKS
# Jay Ulfelder
# 2014-01-20

# Source: Center for Systemic Peace
# http://www.systemicpeace.org/inscr/inscr.htm

rm(list=ls(all=TRUE))

require(XLConnect)
require(reshape)

reg <- readWorksheetFromFile("c:/users/jay/documents/chenoweth project/indata/pitf adverse regime changes 2012.xls",
  sheet=1, startCol=1, endCol=13)
reg <- subset(reg, select=c("SCODE", "YEAR", "YRBEGIN", "YREND", "MAGFAIL", "MAGCOL", "MAGVIOL", "MAGAVE"))
reg$pitf.reg.ongoing <- 1
reg$pitf.reg.onset <- ifelse(reg$YEAR == reg$YRBEGIN, 1, 0)
reg$pitf.reg.end <- ifelse(reg$YEAR == reg$YREND, 1, 0)
reg$pitf.reg.dur <- reg$YEAR - reg$YRBEGIN + 1
reg$YRBEGIN <- reg$YREND <- NULL
names(reg) <- c("sftgcode", "year", "pitf.reg.magfail", "pitf.reg.magcol", "pitf.reg.magviol", "pitf.reg.magave",
    "pitf.reg.ongoing", "pitf.reg.onset", "pitf.reg.end", "pitf.reg.dur")

eth <- readWorksheetFromFile("c:/users/jay/documents/chenoweth project/indata/pitf ethnic wars 2012.xls",
  sheet=1, startCol=1, endCol=13)
eth <- subset(eth, select=c("SCODE", "YEAR", "YRBEGIN", "YREND", "MAGFIGHT", "MAGFATAL", "MAGAREA", "AVEMAG"))
eth$pitf.eth.ongoing <- 1
eth$pitf.eth.onset <- ifelse(eth$YEAR == eth$YRBEGIN, 1, 0)
eth$pitf.eth.end <- ifelse(eth$YEAR == eth$YREND, 1, 0)
eth$pitf.eth.dur <- eth$YEAR - eth$YRBEGIN + 1
eth$YRBEGIN <- eth$YREND <- NULL
names(eth) <- c("sftgcode", "year", "pitf.eth.magfight", "pitf.eth.magfatal", "pitf.eth.magarea",
  "pitf.eth.magave", "pitf.eth.ongoing", "pitf.eth.onset", "pitf.eth.end", "pitf.eth.dur")
ethmagfight <- tapply(eth$pitf.eth.magfight, list(eth$sftgcode, eth$year), max, na.rm=TRUE)
ethmagfight <- melt(ethmagfight)
names(ethmagfight) <- c("sftgcode", "year", "pitf.eth.magfight")
ethmagfatal <- tapply(eth$pitf.eth.magfatal, list(eth$sftgcode, eth$year), max, na.rm=TRUE)
ethmagfatal <- melt(ethmagfatal)
names(ethmagfatal) <- c("sftgcode", "year", "pitf.eth.magfatal")
ethmagarea <- tapply(eth$pitf.eth.magarea, list(eth$sftgcode, eth$year), max, na.rm=TRUE)
ethmagarea <- melt(ethmagarea)
names(ethmagarea) <- c("sftgcode", "year", "pitf.eth.magarea")
ethmagave <- tapply(eth$pitf.eth.magave, list(eth$sftgcode, eth$year), max, na.rm=TRUE)
ethmagave <- melt(ethmagave)
names(ethmagave) <- c("sftgcode", "year", "pitf.eth.magave")
ethongoing <- tapply(eth$pitf.eth.ongoing, list(eth$sftgcode, eth$year), max, na.rm=TRUE)
ethongoing <- melt(ethongoing)
names(ethongoing) <- c("sftgcode", "year", "pitf.eth.ongoing")
ethonset <- tapply(eth$pitf.eth.onset, list(eth$sftgcode, eth$year), max, na.rm=TRUE)
ethonset <- melt(ethonset)
names(ethonset) <- c("sftgcode", "year", "pitf.eth.onset")
ethend <- tapply(eth$pitf.eth.end, list(eth$sftgcode, eth$year), max, na.rm=TRUE)
ethend <- melt(ethend)
names(ethend) <- c("sftgcode", "year", "pitf.eth.end")
ethdur <- tapply(eth$pitf.eth.dur, list(eth$sftgcode, eth$year), min, na.rm=TRUE)
ethdur <- melt(ethdur)
names(ethdur) <- c("sftgcode", "year", "pitf.eth.dur")
ethc <- merge(ethmagfight, ethmagfatal)
ethc <- merge(ethc, ethmagarea)
ethc <- merge(ethc, ethmagave)
ethc <- merge(ethc, ethongoing)
ethc <- merge(ethc, ethonset)
ethc <- merge(ethc, ethend)
ethc <- merge(ethc, ethdur)
ethc$sftgcode <- as.character(ethc$sftgcode)
eth <- na.omit(ethc)

rev <- readWorksheetFromFile("c:/users/jay/documents/chenoweth project/indata/pitf revolutionary wars 2012.xls",
  sheet=1, startCol=1, endCol=13)
rev <- subset(rev, select=c("SCODE", "YEAR", "YRBEGIN", "YREND", "MAGFIGHT", "MAGFATAL", "MAGAREA", "AVEMAG"))
rev$pitf.rev.ongoing <- 1
rev$pitf.rev.onset <- ifelse(rev$YEAR == rev$YRBEGIN, 1, 0)
rev$pitf.rev.end <- ifelse(rev$YEAR == rev$YREND, 1, 0)
rev$pitf.rev.dur <- rev$YEAR - rev$YRBEGIN + 1
rev$YRBEGIN <- rev$YREND <- NULL
names(rev) <- c("sftgcode", "year", "pitf.rev.magfight", "pitf.rev.magfatal", "pitf.rev.magarea",
  "pitf.rev.magave", "pitf.rev.ongoing", "pitf.rev.onset", "pitf.rev.end", "pitf.rev.dur")
# Next block of code deals with years w/more than 1 rev episode ongoing
revmagfight <- tapply(rev$pitf.rev.magfight, list(rev$sftgcode, rev$year), max, na.rm=TRUE)
revmagfight <- melt(revmagfight)
names(revmagfight) <- c("sftgcode", "year", "pitf.rev.magfight")
revmagfatal <- tapply(rev$pitf.rev.magfatal, list(rev$sftgcode, rev$year), max, na.rm=TRUE)
revmagfatal <- melt(revmagfatal)
names(revmagfatal) <- c("sftgcode", "year", "pitf.rev.magfatal")
revmagarea <- tapply(rev$pitf.rev.magarea, list(rev$sftgcode, rev$year), max, na.rm=TRUE)
revmagarea <- melt(revmagarea)
names(revmagarea) <- c("sftgcode", "year", "pitf.rev.magarea")
revmagave <- tapply(rev$pitf.rev.magave, list(rev$sftgcode, rev$year), max, na.rm=TRUE)
revmagave <- melt(revmagave)
names(revmagave) <- c("sftgcode", "year", "pitf.rev.magave")
revongoing <- tapply(rev$pitf.rev.ongoing, list(rev$sftgcode, rev$year), max, na.rm=TRUE)
revongoing <- melt(revongoing)
names(revongoing) <- c("sftgcode", "year", "pitf.rev.ongoing")
revonset <- tapply(rev$pitf.rev.onset, list(rev$sftgcode, rev$year), max, na.rm=TRUE)
revonset <- melt(revonset)
names(revonset) <- c("sftgcode", "year", "pitf.rev.onset")
revend <- tapply(rev$pitf.rev.end, list(rev$sftgcode, rev$year), max, na.rm=TRUE)
revend <- melt(revend)
names(revend) <- c("sftgcode", "year", "pitf.rev.end")
revdur <- tapply(rev$pitf.rev.dur, list(rev$sftgcode, rev$year), min, na.rm=TRUE)
revdur <- melt(revdur)
names(revdur) <- c("sftgcode", "year", "pitf.rev.dur")
revc <- merge(revmagfight, revmagfatal)
revc <- merge(revc, revmagarea)
revc <- merge(revc, revmagave)
revc <- merge(revc, revongoing)
revc <- merge(revc, revonset)
revc <- merge(revc, revend)
revc <- merge(revc, revdur)
revc$sftgcode <- as.character(revc$sftgcode)
rev <- na.omit(revc)

gen <- readWorksheetFromFile("c:/users/jay/documents/chenoweth project/indata/pitf genopoliticides 2012.xls",
  sheet=1, startCol = 1, endCol = 10)
gen <- subset(gen, select=c("SCODE", "YEAR", "YRBEGIN", "YREND", "DEATHMAG"))
gen$pitf.gen.ongoing <- 1
gen$pitf.gen.onset <- ifelse(gen$YEAR == gen$YRBEGIN, 1, 0)
gen$pitf.gen.end <- ifelse(gen$YEAR == gen$YREND, 1, 0)
gen$pitf.gen.dur <- gen$YEAR - gen$YRBEGIN + 1
gen$YRBEGIN <- gen$YREND <- NULL
names(gen) <- c("sftgcode", "year", "pitf.gen.deathmag", "pitf.gen.ongoing", "pitf.gen.onset",
  "pitf.gen.end", "pitf.gen.dur")

source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.R")
pitfps <- merge(subset(rack, select=c(country,sftgcode,year)), reg, all.x = TRUE)
pitfps <- merge(pitfps, rev, all.x = TRUE)
pitfps <- merge(pitfps, eth, all.x = TRUE)
pitfps <- merge(pitfps, gen, all.x = TRUE)
pitfps[is.na(pitfps) & pitfps$year >= 1955 & pitfps$year <= 2013] <- 0
pitfps <- subset(pitfps, select=c(country, sftgcode, year,
                 pitf.reg.magfail, pitf.reg.magcol, pitf.reg.magviol, pitf.reg.magave,
                 pitf.reg.ongoing, pitf.reg.onset, pitf.reg.end, pitf.reg.dur,
                 pitf.rev.magfight, pitf.rev.magfatal, pitf.rev.magarea, pitf.rev.magave,
                 pitf.rev.ongoing, pitf.rev.onset, pitf.rev.end, pitf.rev.dur,
                 pitf.eth.magfight, pitf.eth.magfatal, pitf.eth.magarea, pitf.eth.magave,
                 pitf.eth.ongoing, pitf.eth.onset, pitf.eth.end, pitf.eth.dur,
                 pitf.gen.deathmag, pitf.gen.ongoing, pitf.gen.onset, pitf.gen.end, pitf.gen.dur))
rack <- pitfps[order(pitfps$country, pitfps$year),]

rack$pitf.any.onset <- ifelse(rack$pitf.reg.onset==1 | rack$pitf.eth.onset==1 | rack$pitf.rev.onset==1, 1, 0)
rack$pitf.any.ongoing <- ifelse(rack$pitf.reg.ongoing==1 | rack$pitf.eth.ongoing==1 | rack$pitf.rev.ongoing==1, 1, 0)
for (i in 1:dim(rack)[1]) rack$pitf.dur.min[i] <-
  ifelse(rack$pitf.reg.dur[i] > 0 & rack$pitf.eth.dur[i] > 0 & rack$pitf.rev.dur[i] > 0, min(rack$pitf.reg.dur[i], rack$pitf.eth.dur[i], rack$pitf.rev.dur[i]),
    ifelse(rack$pitf.reg.dur[i] > 0 & rack$pitf.eth.dur[i] > 0 & rack$pitf.rev.dur[i] == 0, min(rack$pitf.reg.dur[i], rack$pitf.eth.dur[i]),
      ifelse(rack$pitf.reg.dur[i] > 0 & rack$pitf.eth.dur[i] == 0 & rack$pitf.rev.dur[i] > 0, min(rack$pitf.reg.dur[i], rack$pitf.rev.dur[i]),
        ifelse(rack$pitf.reg.dur[i] == 0 & rack$pitf.eth.dur[i] > 0 & rack$pitf.rev.dur[i] > 0, min(rack$pitf.eth.dur[i], rack$pitf.rev.dur[i]),
          ifelse(rack$pitf.reg.dur[i] > 0, rack$pitf.reg.dur[i],
            ifelse(rack$pitf.eth.dur[i] > 0, rack$pitf.eth.dur[i],
              ifelse(rack$pitf.rev.dur[i] > 0, rack$pitf.rev.dur[i], 0  )))))))
for (i in 1:dim(rack)[1]) rack$pitf.dur.max[i] <- max(rack$pitf.reg.dur[i], rack$pitf.eth.dur[i], rack$pitf.rev.dur[i])

pitf <- rack[order(rack$country, rack$year),]
rm(rack)

# UPHEAVAL SCORES FOR HARFF GENO-POLITICIDE MODEL

# (a) Identify maximum annual magnitude score for each type of instability
for (i in 1:dim(pitf)[1]) pitf$pitf.reg.max[i] <- max(pitf$pitf.reg.magfail[i], pitf$pitf.reg.magcol[i], pitf$pitf.reg.magviol[i])
for (i in 1:dim(pitf)[1]) pitf$pitf.eth.max[i] <- max(pitf$pitf.eth.magfight[i], pitf$pitf.eth.magfatal[i], pitf$pitf.eth.magarea[i])
for (i in 1:dim(pitf)[1]) pitf$pitf.rev.max[i] <- max(pitf$pitf.rev.magfight[i], pitf$pitf.rev.magfatal[i], pitf$pitf.rev.magarea[i])

# (b) Identify overall maximum annual magnitude score with and w/o genocides
for (i in 1:dim(pitf)[1]) pitf$pitf.mag.max[i] <- max(pitf$pitf.reg.max[i], pitf$pitf.eth.max[i], pitf$pitf.rev.max[i], pitf$pitf.gen.deathmag[i])
for (i in 1:dim(pitf)[1]) pitf$pitf.mag.max2[i] <- max(pitf$pitf.reg.max[i], pitf$pitf.eth.max[i], pitf$pitf.rev.max[i])

# (c) Sum those annual maxima across the previous 15 years with and w/o genocides
pitf$sftpuhvl.15 <- NA
for (i in 1:dim(pitf)[1]) pitf$sftpuhvl.15[i] <- ifelse(pitf$year[i] >= 1969 & pitf$year[i] < 2013,
  sum(pitf$pitf.mag.max[i], pitf$pitf.mag.max[i-1], pitf$pitf.mag.max[i-2], pitf$pitf.mag.max[i-3], pitf$pitf.mag.max[i-4],
  pitf$pitf.mag.max[i-5], pitf$pitf.mag.max[i-6], pitf$pitf.mag.max[i-7], pitf$pitf.mag.max[i-8], pitf$pitf.mag.max[i-9],
  pitf$pitf.mag.max[i-10], pitf$pitf.mag.max[i-11], pitf$pitf.mag.max[i-12], pitf$pitf.mag.max[i-13], pitf$pitf.mag.max[i-14],
  na.rm=TRUE), NA)
pitf$sftpuhvl2.15 <- NA
for (i in 1:dim(pitf)[1]) pitf$sftpuhvl2.15[i] <- ifelse(pitf$year[i] >= 1969 & pitf$year[i] < 2013,
  sum(pitf$pitf.mag.max2[i], pitf$pitf.mag.max2[i-1], pitf$pitf.mag.max2[i-2], pitf$pitf.mag.max2[i-3], pitf$pitf.mag.max2[i-4],
  pitf$pitf.mag.max2[i-5], pitf$pitf.mag.max2[i-6], pitf$pitf.mag.max2[i-7], pitf$pitf.mag.max2[i-8], pitf$pitf.mag.max2[i-9],
  pitf$pitf.mag.max2[i-10], pitf$pitf.mag.max2[i-11], pitf$pitf.mag.max2[i-12], pitf$pitf.mag.max2[i-13], pitf$pitf.mag.max2[i-14],
  na.rm=TRUE), NA)

# 10-year version of upheaval
pitf$sftpuhvl.10 <- NA
for (i in 1:dim(pitf)[1]) pitf$sftpuhvl.10[i] <- ifelse(pitf$year[i] >= 1964 & pitf$year[i] < 2013,
  sum(pitf$pitf.mag.max[i], pitf$pitf.mag.max[i-1], pitf$pitf.mag.max[i-2], pitf$pitf.mag.max[i-3], pitf$pitf.mag.max[i-4],
  pitf$pitf.mag.max[i-5], pitf$pitf.mag.max[i-6], pitf$pitf.mag.max[i-7], pitf$pitf.mag.max[i-8], pitf$pitf.mag.max[i-9],
  na.rm=TRUE), NA)
pitf$sftpuhvl2.10 <- NA
for (i in 1:dim(pitf)[1]) pitf$sftpuhvl2.10[i] <- ifelse(pitf$year[i] >= 1964 & pitf$year[i] < 2013,
  sum(pitf$pitf.mag.max2[i], pitf$pitf.mag.max2[i-1], pitf$pitf.mag.max2[i-2], pitf$pitf.mag.max2[i-3], pitf$pitf.mag.max2[i-4],
  pitf$pitf.mag.max2[i-5], pitf$pitf.mag.max2[i-6], pitf$pitf.mag.max2[i-7], pitf$pitf.mag.max2[i-8], pitf$pitf.mag.max2[i-9],
  na.rm=TRUE), NA)

# 5-year version of upheaval
pitf$sftpuhvl.5 <- NA
for (i in 1:dim(pitf)[1]) pitf$sftpuhvl.5[i] <- ifelse(pitf$year[i] >= 1959 & pitf$year[i] < 2013,
  sum(pitf$pitf.mag.max[i], pitf$pitf.mag.max[i-1], pitf$pitf.mag.max[i-2], pitf$pitf.mag.max[i-3], pitf$pitf.mag.max[i-4],
  na.rm=TRUE), NA)
pitf$sftpuhvl2.5 <- NA
for (i in 1:dim(pitf)[1]) pitf$sftpuhvl2.5[i] <- ifelse(pitf$year[i] >= 1959 & pitf$year[i] < 2013,
  sum(pitf$pitf.mag.max2[i], pitf$pitf.mag.max2[i-1], pitf$pitf.mag.max2[i-2], pitf$pitf.mag.max2[i-3], pitf$pitf.mag.max2[i-4],
  na.rm=TRUE), NA)

# Civil-war onset for civil-war risk model
pitf$pitf.cwar.onset <- ifelse(pitf$pitf.rev.onset==1 | pitf$pitf.eth.onset==1, 1, ifelse(is.na(pitf$pitf.rev.onset)==TRUE, NA, 0) )

# Logged 
pitf$pitf.reg.durln <- log1p(pitf$pitf.reg.dur)
pitf$pitf.eth.durln <- log1p(pitf$pitf.eth.dur)
pitf$pitf.rev.durln <- log1p(pitf$pitf.rev.dur)
pitf$pitf.dur.minln <- log1p(pitf$pitf.dur.min)
pitf$sftpuhvl2.10ln <- log1p(pitf$sftpuhvl2.10)

write.csv(pitf, "c:/users/jay/documents/ushmm/statrisk2014/outdata/pitfproblemset.csv", row.names = FALSE)
