# BLENDED COUP DATA
# Jay Ulfelder
# 2015-01-20

# Source: Center for Systemic Peace http://www.systemicpeace.org/inscr/CSPCoupsList2012.xls
# Source: Powell & Thyne http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt 

rm(list=ls(all=TRUE))

pt <- read.delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt")
pt <- subset(pt, select=c("country", "year", "coup"))
data <- pt
require(reshape)
data <- rename(data, c(country="name"))
source("c:/users/jay/documents/ushmm/statrisk/r/pitf_code_maker.r")
pt <- data
pt$pt.succ <- ifelse(pt$coup==2, 1, 0)
pt$pt.fail <- ifelse(pt$coup==1, 1, 0)
pt <- rename(pt, c(code="sftgcode"))
ptsum.s <- tapply(pt$pt.succ, list(pt$sftgcode, pt$year), sum)
ptsum.f <- tapply(pt$pt.fail, list(pt$sftgcode, pt$year), sum)
pt.s <- melt(ptsum.s)
pt.f <- melt(ptsum.f)
names(pt.s) <- c("sftgcode", "year", "pt.succ")
names(pt.f) <- c("sftgcode", "year", "pt.fail")
source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
pt.tscs <- merge(rack, pt.s, all.x = TRUE)
pt.tscs <- merge(pt.tscs, pt.f, all.x = TRUE)
pt.tscs <- pt.tscs[which(pt.tscs$year >= 1950 & pt.tscs$year <= 2013),]
pt.tscs <- pt.tscs[order(pt.tscs$country, pt.tscs$year),]
pt.tscs[is.na(pt.tscs)] <- 0
source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
pt.tscs <- merge(rack, pt.tscs, all.x = TRUE)

require(XLConnect)
csp <- readWorksheetFromFile("c:/users/jay/documents/ushmm/statrisk2014/indata/cspcoupslist2012.xls", sheet=1)
csp <- subset(csp, is.na(scode)==FALSE, select=c(scode, year, success))
names(csp) <- c("sftgcode", "year", "success")
csp$successful <- ifelse(csp$success==1, 1, 0)
csp$failed <- ifelse(csp$success==2, 1, 0)
csp$plot <- ifelse(csp$success==3, 1, 0)
csp$rumor <- ifelse(csp$success==4, 1, 0)
coupsum.s <- tapply(csp$successful, list(csp$sftgcode, csp$year), sum)
coupsum.f <- tapply(csp$failed, list(csp$sftgcode, csp$year), sum)
coupsum.p <- tapply(csp$plot, list(csp$sftgcode, csp$year), sum)
coupsum.r <- tapply(csp$rumor, list(csp$sftgcode, csp$year), sum)
require(reshape)
coup.s <- melt(coupsum.s)
coup.f <- melt(coupsum.f)
coup.p <- melt(coupsum.p)
coup.r <- melt(coupsum.r)
names(coup.s) <- c("sftgcode", "year", "csp.succ")
names(coup.f) <- c("sftgcode", "year", "csp.fail")
names(coup.p) <- c("sftgcode", "year", "csp.plot")
names(coup.r) <- c("sftgcode", "year", "csp.rumr")
source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.r")
rack <- subset(rack, year >= 1946 & year <= 2012, select=c(country, sftgcode, year))
csp.tscs <- merge(rack, coup.s, all.x = TRUE)
csp.tscs <- merge(csp.tscs, coup.f, all.x = TRUE)
csp.tscs <- merge(csp.tscs, coup.p, all.x = TRUE)
csp.tscs <- merge(csp.tscs, coup.r, all.x = TRUE)
csp.tscs[is.na(csp.tscs)] <- 0
source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
csp.tscs <- merge(rack, csp.tscs, all.x = TRUE)
csp.tscs <- csp.tscs[order(csp.tscs$country, csp.tscs$year),]

write.csv(pt.tscs, "c:/users/jay/documents/ushmm/statrisk2014/outdata/coups.pt.csv", row.names = FALSE)
write.csv(csp.tscs, "c:/users/jay/documents/ushmm/statrisk2014/outdata/coups.csp.csv", row.names = FALSE)

meld <- merge(csp.tscs, pt.tscs, all.x = TRUE)
meld <- meld[order(meld$country, meld$year),]

meld$cou.s.d <- ifelse(is.na(meld$pt.succ)==TRUE, ifelse(meld$csp.succ > 0, 1, 0),
    ifelse(is.na(meld$csp.succ)==TRUE, ifelse(meld$pt.succ > 0, 1, 0),
      ifelse(meld$csp.succ > 0 | meld$pt.succ > 0, 1, 0) ) )
meld$cou.f.d <- ifelse(is.na(meld$pt.fail)==TRUE, ifelse(meld$csp.fail > 0, 1, 0),
    ifelse(is.na(meld$csp.fail)==TRUE, ifelse(meld$pt.fail > 0, 1, 0),
      ifelse(meld$csp.fail > 0 | meld$pt.fail > 0, 1, 0) ) )
meld$cou.a.d <- ifelse(meld$cou.f.d==1 | meld$cou.s.d==1, 1, ifelse(is.na(meld$cou.s.d)==TRUE, NA, 0) )
meld <- meld[order(meld$country,meld$year),]

# Create indicator for any attempts in previous five years
require(plm)
lags <- pdata.frame(meld, index = c("country", "year"))
lags$cou.s.d.1 <- lag(lags$cou.s.d, 1)
lags$cou.s.d.2 <- lag(lags$cou.s.d, 2)
lags$cou.s.d.3 <- lag(lags$cou.s.d, 3)
lags$cou.s.d.4 <- lag(lags$cou.s.d, 4)
lags$cou.f.d.1 <- lag(lags$cou.f.d, 1)
lags$cou.f.d.2 <- lag(lags$cou.f.d, 2)
lags$cou.f.d.3 <- lag(lags$cou.f.d, 3)
lags$cou.f.d.4 <- lag(lags$cou.f.d, 4)
meld <- as.data.frame(lags)
meld$year <- as.numeric(as.character(meld$year))
# Workaround for list to df
temp <- tempfile()
write.csv(meld, temp, row.names = FALSE)
meld <- read.csv(temp)
meld$cou.tries5d <-
  ifelse(is.na(meld$cou.s.d.1)==TRUE, ifelse(meld$cou.s.d>0 | meld$cou.f.d>0, 1, 0),
    ifelse(is.na(meld$cou.s.d.2)==TRUE, ifelse(meld$cou.s.d>0 | meld$cou.f.d>0 | meld$cou.s.d.1>0 | meld$cou.f.d.1>0, 1, 0),
      ifelse(is.na(meld$cou.s.d.3)==TRUE, ifelse(meld$cou.s.d>0 | meld$cou.f.d>0 | meld$cou.s.d.1>0 | meld$cou.f.d.1>0 | meld$cou.s.d.2>0 | meld$cou.f.d.2>0, 1, 0),  
        ifelse(is.na(meld$cou.s.d.4)==TRUE, ifelse(meld$cou.s.d>0 | meld$cou.s.d.1>0 | meld$cou.s.d.2>0 | meld$cou.s.d.3>0 | meld$cou.f.d>0 | meld$cou.f.d.1>0 | meld$cou.f.d.2>0 | meld$cou.f.d.3>0, 1, 0),
          ifelse(meld$cou.s.d>0 | meld$cou.s.d.1>0 | meld$cou.s.d.2>0 | meld$cou.s.d.3>0 | meld$cou.s.d.4>0 | meld$cou.f.d>0 | meld$cou.f.d.1>0 | meld$cou.f.d.2>0 | meld$cou.f.d.3>0 | meld$cou.f.d.4>0, 1, 0)))))
meld$cou.tries5d[meld$year<1950] <- NA
meld$cou.s.d.1 <- meld$cou.s.d.2 <- meld$cou.s.d.3 <- meld$cou.s.d.4 <- NULL 
meld$cou.f.d.1 <- meld$cou.f.d.2 <- meld$cou.f.d.3 <- meld$cou.f.d.4 <- NULL

write.csv(meld, "c:/users/jay/documents/ushmm/statrisk2014/outdata/coups.csv", row.names = FALSE)
