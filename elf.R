# ETHNIC FRACTIONALIZATION (FEARON)
# Jay Ulfelder
# 2014-01-20

# Source: http://www.stanford.edu/group/ethnic/publicdata/publicdata.html

rm(list=ls(all=TRUE))

require(foreign)
ethnic <- read.dta("c:/users/jay/documents/ushmm/statrisk2014/indata/egroupsrepdata.dta")
ef <- tapply(ethnic$ef, ethnic$country, max)
elf <- tapply(ethnic$elf, ethnic$country, max)
require(reshape)
melt.ef <- melt(ef)
melt.elf <- melt(elf)
names(melt.ef) <- c("country", "ef")
names(melt.elf) <- c("country", "elf")
eth <- merge(melt.ef, melt.elf)
require(countrycode)
eth$ccode <- countrycode(eth$country, "country.name", "cown")
data <- eth
source("c:/users/jay/documents/ushmm/statrisk/r/cow_to_pitf_code.r")
eth <- data

# Fill in missing codes by hand
eth$sftgcode <- as.character(eth$sftgcode)
eth$sftgcode[eth$country=="COSTARICA"] <- "COS"
eth$sftgcode[eth$country=="ETHIOPIA"] <- "ETI"
eth$sftgcode[eth$country=="GERMAN DEM. REP."] <- "GDR"
eth$sftgcode[eth$country=="GERMANYFED. REP."] <- "GFR"
eth$sftgcode[eth$country=="GERMANYFED. REP."] <- "GFR"
eth$sftgcode[eth$country=="PAKISTAN"] <- "PAK"
eth$sftgcode[eth$country=="PAPUA N.G."] <- "PNG"
eth$sftgcode[eth$country=="RUSSIA"] <- "RUS"
eth$sftgcode[eth$country=="USSR"] <- "USS"
eth$sftgcode[eth$country=="VIETNAM"] <- "VIE"
eth$sftgcode[eth$country=="YEMEN"] <- "YEM"
eth$sftgcode[eth$country=="YUGOSLAV"] <- "YGS"
eth$sftgcode[eth$country=="YUGOSLAVIA"] <- "YUG"
eth$sftgcode[eth$country=="N. KOREA"] <- "PRK"
eth <- subset(eth, select=c("sftgcode", "ef", "elf"))

# Fill in missing values from EPR where possible
pks <- c("PKS", 0.69, NA) # From EPR 
et2 <- c("ETH", eth$ef[eth$sftgcode=="ETI"], eth$elf[eth$sftgcode=="ETI"])
ypr <- c("YPR", 0, NA) # From EPR
yar <- c("YAR", 0.42, NA) # From EPR
srb <- c("SRB", eth$ef[eth$sftgcode=="YGS"], eth$elf[eth$sftgcode=="YGS"])
mne <- c("MNE", eth$ef[eth$sftgcode=="YGS"], eth$elf[eth$sftgcode=="YGS"])
kos <- c("KOS", eth$ef[eth$sftgcode=="YGS"], eth$elf[eth$sftgcode=="YGS"])
com <- c("COM", NA, NA)
rvn <- c("RVN", 0.39, NA) # From EPR
drv <- c("DRV", 0.39, NA) # Using EPR's value for RVN
eqg <- c("EQG", NA, NA)
etm <- c("ETM", NA, NA)
ger <- c("GER", eth$ef[eth$sftgcode=="GFR"], eth$elf[eth$sftgcode=="GFR"])
qat <- c("QAT", NA, NA)
sol <- c("SOL", NA, NA)
ssd <- c("SSD", eth$ef[eth$sftgcode=="SUD"], eth$elf[eth$sftgcode=="SUD"])
eth <- as.data.frame(rbind(eth, pks, et2, ypr, yar, srb, mne, kos, com, rvn, drv, eqg, etm, ger, qat, sol, ssd))

# Put into country-year rack
source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
rack <- merge(rack, eth, all.x = TRUE)

# Fix types
rack$ef <- as.numeric(rack$ef)
rack$elf <- as.numeric(rack$elf)

# Create categorical variable used in modeling
rack$efcat[rack$ef < quantile(rack$ef, 1/3, na.rm=TRUE)] <- 1
rack$efcat[rack$ef >= quantile(rack$ef, 1/3, na.rm=TRUE) & rack$ef <= quantile(rack$ef, 2/3, na.rm=TRUE)] <- 2
rack$efcat[rack$ef > quantile(rack$ef, 2/3, na.rm=TRUE)] <- 3
rack$efcat[is.na(rack$ef)==TRUE] <- 9

# Dummied version of categorical variable
rack$efcat1 <- ifelse(rack$efcat==1, 1, ifelse(is.na(rack$efcat)==TRUE, NA, 0) )
rack$efcat2 <- ifelse(rack$efcat==2, 1, ifelse(is.na(rack$efcat)==TRUE, NA, 0) )
rack$efcat3 <- ifelse(rack$efcat==3, 1, ifelse(is.na(rack$efcat)==TRUE, NA, 0) )
rack$efcat9 <- ifelse(rack$efcat==9, 1, ifelse(is.na(rack$efcat)==TRUE, NA, 0) )

write.csv(rack, "c:/users/jay/documents/ushmm/statrisk2014/outdata/elf.csv", row.names = FALSE)
