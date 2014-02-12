# CPG STATISTICAL RISK ASSESSMENT
# Jay Ulfelder
# 2014-01-21

# Clear workspace
rm(list=ls(all=TRUE))

# Set working directory
setwd("c:/users/jay/documents/ushmm/statrisk2014/outdata/")

# Load the required data sets
source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.r")
mkl <- read.csv("masskillings.csv")
wdi <- read.csv("wdi.csv")
coups <- read.csv("coups.csv")
polity <- read.csv("polity.csv")
pitf <- read.csv("pitfproblemset.csv")
mepv <- read.csv("mepv.csv")
ios <- read.csv("ios.csv")
imrate <- read.csv("imrate.csv")
elc <- read.csv("elc.csv")
dis <- read.csv("dis.csv")
elf <- read.csv("elf.csv")
econ <- read.csv("econstats.csv")

# Merge those data sets
df <- merge(rack, mkl, all.x = TRUE)
df <- merge(df, wdi, all.x = TRUE)
df <- merge(df, coups, all.x = TRUE)
df <- merge(df, polity, all.x = TRUE)
df <- df[!duplicated(df),]
df <- merge(df, pitf, all.x = TRUE)
df <- merge(df, mepv, all.x = TRUE)
df <- merge(df, ios, all.x = TRUE)
df <- merge(df, imrate, all.x = TRUE)
df <- merge(df, elc, all.x = TRUE)
df <- merge(df, dis, all.x = TRUE)
df <- merge(df, elf, all.x = TRUE)
df <- merge(df, econ, all.x = TRUE)
df <- df[order(df$country, df$year),]

# DEPENDENT VARIABLE (1-YEAR LEAD)
df <- df[order(df$country, df$year),]
for (i in 1:dim(df)[1]) df$start.1[i] <- df$start[i+1]
df$start.1 <- replace(df$start.1, which(df$year >= 2013 | df$year==df$yrdied), NA)

# COUP ATTEMPT (1-YEAR LEAD)
df <- df[order(df$country, df$year),]
for (i in 1:dim(df)[1]) df$cou.a.d.1[i] <- df$cou.a.d[i+1]
df$cou.a.d.1 <- replace(df$cou.a.d.1, which(df$year >= 2013 | df$year==df$yrdied), NA)

# CIVIL-WAR ONSET (1-YEAR LEAD)
df <- df[order(df$country, df$year),]
for (i in 1:dim(df)[1]) df$pitf.cwar.onset.1[i] <- df$pitf.cwar.onset[i+1]
df$pitf.cwar.onset.1 <- replace(df$pitf.cwar.onset.1, which(df$year >= 2013 | df$year==df$yrdied), NA)

# PITF INSTABILITY ONSET (1-YEAR LEAD)
df <- df[order(df$country, df$year),]
for (i in 1:dim(df)[1]) df$pitf.any.onset.1[i] <- df$pitf.any.onset[i+1]
df$pitf.any.onset.1 <- replace(df$pitf.any.onset.1, which(df$year >= 2013 | df$year==df$yrdied), NA)

write.csv(df, "statrisk2014.merge.20140121.csv", row.names=FALSE)

####################################
# Model Formulae
####################################

f.coup <- formula(cou.a.d.1 ~
    xxxcimrln + 
    regacln +
    postcw +
    cou.tries5d +
    polcat2 + polcat3 + polcat7 +
    durableln +
    mix.gdppcgrowsr +
    civconln +
    io.iccpr1)

f.cwar <- formula(pitf.cwar.onset.1 ~
    wdi.popsizeln + 
    xxxcimrln +
    polcat2 + polcat3 + polcat7 +
    regacln +
    civconln)

f.threat <- formula(start.1 ~ log(coup.p) + log(cwar.p))

f.pitf <- formula(pitf.any.onset.1 ~
    reg.eap + reg.eur + reg.mna + reg.sca + reg.amr +
    xxxcimrln +
    pitfcat2 + pitfcat3 + pitfcat4 + pitfcat5 + pitfcat6 +
    dispota4ln +
    regacln)

f.harff <- formula(start.1 ~
    log(pitf.p) +
    ever +
    sftpuhvl2.10ln +
    autocracy +
    elceliti +
    elcelethc +
    wdi.tradeln)

f.rf <- formula(as.factor(start.1) ~
    reg.afr + reg.eap + reg.eur + reg.mna + reg.sca + reg.amr +
    ongoing +
    ever +
    ageln + 
    wdi.popsizeln + 
    xxxcimrln +
    mix.gdppcgrowsr +
    wdi.tradeln +
    io.iccpr1 +
    postcw +
    polcat1 + polcat2 + polcat3 + polcat7 +
    durableln +
    dispota4ln +
    efcat1 + efcat2 + efcat3 + efcat9 +
    elceleth1 + elceleth2 +
    elceliti +
    cou.tries5d +
    sftpuhvl2.10ln +
    regacln +
    civconln)

#############################
# Model Validation
#############################

require(caret)

valdat <- subset(df, year >= 1960 & year <= 2013 & df$year >= df$yrborn & df$year <= df$yrdied &
  is.na(start.1) == FALSE)
y <- valdat$start.1
valdat$k <- createFolds(y, k = 10, list = FALSE)

predit <- function(x) {
  train <- subset(valdat, k != x)
  test <- subset(valdat, k == x)
  test$cwar.p <- predict(glm(f.cwar, family = binomial, data = train, na.action = na.exclude),
    newdata = test, type = "response")
  test$coup.p <- predict(glm(f.coup, family = binomial, data = train, na.action = na.exclude),
    newdata = test, type = "response")
  train$cwar.p <- predict(glm(f.cwar, family = binomial, data = train, na.action = na.exclude), type = "response")
  train$coup.p <- predict(glm(f.coup, family = binomial, data = train, na.action = na.exclude), type = "response")
  test$threat.p <- predict(glm(f.threat, family = binomial, data = train, na.action = na.exclude),
    newdata = test, type = "response")
  test$pitf.p <- predict(glm(f.pitf, family = binomial, data = subset(train, pitf.any.ongoing==0),
    na.action = na.exclude), newdata = test, type = "response")
  train$pitf.p <- predict(glm(f.pitf, family = binomial, data = train, na.action = na.exclude), type = "response")
  test$harff.p <- predict(glm(f.harff, family = binomial, data = train, na.action = na.exclude),
    newdata = test, type = "response")
  require(randomForest)
  test$rf.p <- predict(randomForest(f.rf, data = train, na.action="na.exclude",
    ntree = 1000, mtry = 3, cutoff=c(0.2,0.8)),
    newdata = test, type = "prob", na.action = "na.exclude")[,2]
  test$mean.p <- (test$threat.p + test$harff.p + test$rf.p)/3
  out <- subset(test, select = c(sftgcode, year, start.1, mean.p, threat.p, harff.p, rf.p, k))
  return(out)
}

test1 <- predit(1)
test2 <- predit(2)
test3 <- predit(3)
test4 <- predit(4)
test5 <- predit(5)
test6 <- predit(6)
test7 <- predit(7)
test8 <- predit(8)
test9 <- predit(9)
test10 <- predit(10)
out <- rbind(test1, test2, test3, test4, test5,
             test6, test7, test8, test9, test10)

# Distribution of AUC scores by fold
fun.auc <- function(df, x) {
  require(verification)
  mean <- roc.area(df$start.1[out$k==x], df$mean.p[out$k==x])
  harff <- roc.area(df$start.1[out$k==x], df$harff.p[out$k==x])
  threat <- roc.area(df$start.1[out$k==x], df$threat.p[out$k==x])
  rf <- roc.area(df$start.1[out$k==x], df$rf.p[out$k==x])
  all <- c(x, mean$A, harff$A, threat$A, rf$A )
  names(all) <- c("fold", "mean", "harff", "threat", "RF")
  return(all)
}

auc1 <- fun.auc(out, 1)
auc2 <- fun.auc(out, 2)
auc3 <- fun.auc(out, 3)
auc4 <- fun.auc(out, 4)
auc5 <- fun.auc(out, 5)
auc6 <- fun.auc(out, 6)
auc7 <- fun.auc(out, 7)
auc8 <- fun.auc(out, 8)
auc9 <- fun.auc(out, 9)
auc10 <- fun.auc(out, 10)
auctab <- as.data.frame(rbind(auc1, auc2, auc3, auc4, auc5, auc6, auc7, auc8, auc9, auc10))

png(file = "cpg.statrisk2014.val.auc.by.fold.png",
     width=12, height=12, units='cm', bg='white', res=150)
plot(density(auctab$mean), xlim=c(0.5,1), ylim=c(0,10), lwd = 2.5, main = "", col = "black",
     bty = "n", mai = c(1.5, 0.5, 0.25, 0.25), xaxt = "n", yaxt = "n", xlab = "AUC", ylab = "" )
axis(1, tick = FALSE)
lines(density(auctab$harff), col = "red", xlim=c(0.5,1), ylim=c(0,10), lwd = 1.5)
lines(density(auctab$threat), col = "blue", xlim=c(0.5,1), ylim=c(0,10), lwd = 1.5)
lines(density(auctab$RF), col = "forestgreen", xlim=c(0.5,1), ylim=c(0,10), lwd = 1.5)
text(x = 0.95,y = 8, "mean", col = "black", adj = 0)
text(x = 0.95,y = 7.5, "Harff", col = "red", adj = 0)
text(x = 0.95,y = 7, "threat", col = "blue", adj = 0)
text(x = 0.95,y = 6.5, "RF", col = "forestgreen", adj = 0)
dev.off()

# ROC curves by model
library(ROCR)
mean.pred <- prediction(out$mean.p, out$start.1)
mean.roc <- performance(mean.pred, "tpr", "fpr")
mean.auc <- performance(mean.pred, measure = "auc")
harff.pred <- prediction(out$harff.p, out$start.1)
harff.roc <- performance(harff.pred, "tpr", "fpr")
harff.auc <- performance(harff.pred, measure = "auc")
threat.pred <- prediction(out$threat.p, out$start.1)
threat.roc <- performance(threat.pred, "tpr", "fpr")
threat.auc <- performance(threat.pred, measure = "auc")
rf.pred <- prediction(out$rf.p, out$start.1)
rf.roc <- performance(rf.pred, "tpr", "fpr")
rf.auc <- performance(rf.pred, measure = "auc")

png(file = "cpg.statrisk2014.val.roc.by.model.png",
     width=12, height=12, units='cm', bg='white', res=150)
plot(mean.roc, col = "black", lwd=2, add = FALSE)
plot(threat.roc, col = "blue", add = TRUE)
plot(harff.roc, col = "red", add = TRUE)
plot(rf.roc, col = "forestgreen", add = TRUE)
text(x=1,y=0.15,
     labels = paste("RF", substring(as.character(rf.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "forestgreen")
text(x=1,y=0.10,
     labels = paste("Harff", substring(as.character(harff.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "red")
text(x=1,y=0.05,
     labels = paste("threat", substring(as.character(threat.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "blue")
text(x=1,y=0,
     labels = paste("Mean", substring(as.character(mean.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "black")
dev.off()

####################################
# Model Estimation
####################################

# Remove most recent couple of years to avoid treating in-sample ests as forecasts
subdat <- subset(df, year < as.numeric(substr(as.Date(Sys.Date()),1,4)) - 2)

coup <- glm(f.coup, family = binomial, data = subdat, na.action = na.exclude)
df$coup.p <- predict(coup, newdata = df, type = "response")
subdat$coup.p <- predict(coup, newdata = subdat, type = "response")

cwar <- glm(f.cwar, family = binomial, data = subdat, na.action = na.exclude)
df$cwar.p <- predict(cwar, newdata = df, type = "response")
subdat$cwar.p <- predict(cwar, newdata = subdat, type = "response")

threat <- glm(f.threat, family = binomial, data = subdat, na.action = na.exclude)
df$threat.p <- predict(threat, newdata = df, type = "response")

pitf <- glm(f.pitf, family = binomial, data = subset(subdat, pitf.any.ongoing==0), na.action = na.exclude)
df$pitf.p <- predict(pitf, newdata = df, type = "response")
subdat$pitf.p <- predict(pitf, newdata = subdat, type = "response")

harff <- glm(f.harff, family = binomial, data = subdat, na.action = na.exclude)
df$harff.p <- predict(harff, newdata = df, type = "response")

require(randomForest)
rf <- randomForest(f.rf, data = subdat, na.action = na.exclude, ntree = 1000,
  mtry = 3, cutoff = c(0.2,0.8)) # Params selected thru gridded search in validation stage
df$rf.p <- predict(rf, type = "prob", newdata = df, na.action = na.exclude)[,2]

df$mean.p <- (df$harff.p + df$threat.p + df$rf.p)/3

df$insamp <- ifelse(df$year < as.numeric(substr(as.Date(Sys.Date()),1,4)) - 2, 1, 0)

# Write out in-sample responses
insamp <- subset(df, select=c(country, sftgcode, iso3code, year, start.1, mean.p, harff.p, threat.p, rf.p))
write.csv(insamp, file = "c:/users/jay/documents/ushmm/statrisk2014/outdata/insample.csv", row.names = FALSE)

###############################################
# Predictions Using Last Valid Observations
###############################################

id <- c("country", "sftgcode", "iso3code", "year")
dv <- c("start", "ongoing")
modvars <- c("ageln",
             "postcw",
             "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca", "reg.amr",
             "wdi.popsizeln",
             "wdi.tradeln",
             "xxxcimrln",
             "polcat1", "polcat2", "polcat3", "polcat7",
             "pitfcat1", "pitfcat2", "pitfcat3", "pitfcat4", "pitfcat5", "pitfcat6",     
             "durableln",
             "autocracy",
             "efcat1", "efcat2", "efcat3", "efcat9",
             "ever",
             "ongoing",
             "cou.tries5d", 
             "regacln",
             "civconln",
             "io.iccpr1",
             "mix.gdppcgrowsr",
             "dispota4ln",
             "elceliti",
             "elcelethc",
             "elceleth1",
             "elceleth2",
             "sftpuhvl2.10ln")
varlist <- c(id, dv, modvars)

# Function to pull the latest available observation of each required variable by country
pull <- function(i) {
  d <- subset(df, sftgcode==i, select = varlist)
  snip <- function(x) { rev( na.omit(x) )[1] }
  xi <- lapply(d, snip)
  xi <- as.data.frame(xi)
  return(xi)
}

# Do each separately so you can see where trouble arises
AFG <- pull("AFG")
ALB <- pull("ALB")
ALG <- pull("ALG")
ANG <- pull("ANG")
ARG <- pull("ARG")
ARM <- pull("ARM")
AUL <- pull("AUL")
AUS <- pull("AUS")
AZE <- pull("AZE")
BAH <- pull("BAH")
BNG <- pull("BNG")
BLR <- pull("BLR")
BEL <- pull("BEL")
BEN <- pull("BEN")
BHU <- pull("BHU")
BOL <- pull("BOL")
BOS <- pull("BOS")
BOT <- pull("BOT")
BRA <- pull("BRA")
BFO <- pull("BFO")
BUL <- pull("BUL")
BUI <- pull("BUI")
CAM <- pull("CAM")
CAO <- pull("CAO")
CAN <- pull("CAN")
CEN <- pull("CEN")
CHA <- pull("CHA")
CHL <- pull("CHL")
CHN <- pull("CHN")
COL <- pull("COL")
COM <- pull("COM")
CON <- pull("CON")
ZAI <- pull("ZAI")
COS <- pull("COS")
CRO <- pull("CRO")
CUB <- pull("CUB")
CYP <- pull("CYP")
CZR <- pull("CZR")
DEN <- pull("DEN")
DJI <- pull("DJI")
DOM <- pull("DOM")
ECU <- pull("ECU")
EGY <- pull("EGY")
SAL <- pull("SAL")
EQG <- pull("EQG")
ERI <- pull("ERI")
EST <- pull("EST")
ETI <- pull("ETI")
FJI <- pull("FJI")
FIN <- pull("FIN")
FRN <- pull("FRN")
GAB <- pull("GAB")
GAM <- pull("GAM")
GRG <- pull("GRG")
GER <- pull("GER")
GHA <- pull("GHA")
GRC <- pull("GRC")
GUA <- pull("GUA")
GUI <- pull("GUI")
GNB <- pull("GNB")
GUY <- pull("GUY")
HAI <- pull("HAI")
HON <- pull("HON")
HUN <- pull("HUN")
IND <- pull("IND")
INS <- pull("INS")
IRN <- pull("IRN")
IRQ <- pull("IRQ")
IRE <- pull("IRE")
ISR <- pull("ISR")
ITA <- pull("ITA")
IVO <- pull("IVO")
JAM <- pull("JAM")
JPN <- pull("JPN")
JOR <- pull("JOR")
KZK <- pull("KZK")
KEN <- pull("KEN")
KUW <- pull("KUW")
KYR <- pull("KYR")
LAO <- pull("LAO")
LAT <- pull("LAT")
LEB <- pull("LEB")
LES <- pull("LES")
LBR <- pull("LBR")
LIB <- pull("LIB")
LIT <- pull("LIT")
MAC <- pull("MAC")
MAG <- pull("MAG")
MAW <- pull("MAW")
MAL <- pull("MAL")
MLI <- pull("MLI")
MAA <- pull("MAA")
MAS <- pull("MAS")
MEX <- pull("MEX")
MLD <- pull("MLD")
MON <- pull("MON")
MNE <- pull("MNE")
MOR <- pull("MOR")
MZM <- pull("MZM")
MYA <- pull("MYA")
NAM <- pull("NAM")
NEP <- pull("NEP")
NTH <- pull("NTH")
NEW <- pull("NEW")
NIC <- pull("NIC")
NIR <- pull("NIR")
NIG <- pull("NIG")
PRK <- pull("PRK")
NOR <- pull("NOR")
OMA <- pull("OMA")
PAK <- pull("PAK")
PAN <- pull("PAN")
PNG <- pull("PNG")
PAR <- pull("PAR")
PER <- pull("PER")
PHI <- pull("PHI")
POL <- pull("POL")
POR <- pull("POR")
QAT <- pull("QAT")
RUM <- pull("RUM")
RUS <- pull("RUS")
RWA <- pull("RWA")
SAU <- pull("SAU")
SEN <- pull("SEN")
SRB <- pull("SRB")
SIE <- pull("SIE")
SIN <- pull("SIN")
SLO <- pull("SLO")
SLV <- pull("SLV")
SOL <- pull("SOL")
SOM <- pull("SOM")
SAF <- pull("SAF")
ROK <- pull("ROK")
SSD <- pull("SSD")
SPN <- pull("SPN")
SRI <- pull("SRI")
SUD <- pull("SUD")
SWA <- pull("SWA")
SWD <- pull("SWD")
SWZ <- pull("SWZ")
SYR <- pull("SYR")
TAJ <- pull("TAJ")
TAZ <- pull("TAZ")
THI <- pull("THI")
ETM <- pull("ETM")
TOG <- pull("TOG")
TRI <- pull("TRI")
TUN <- pull("TUN")
TUR <- pull("TUR")
TKM <- pull("TKM")
UGA <- pull("UGA")
UKR <- pull("UKR")
UAE <- pull("UAE")
UKG <- pull("UK ")
USA <- pull("USA")
URU <- pull("URU")
UZB <- pull("UZB")
VEN <- pull("VEN")
VIE <- pull("VIE")
YEM <- pull("YEM")
ZAM <- pull("ZAM")
ZIM <- pull("ZIM")

# Hard code trade openness for IRQ and PRK from CIA World Factbook
IRQ$wdi.tradeln <- log( 100*( (93.9+56.9)/242.5 ) )
PRK$wdi.tradeln <- log( 100*( (4.7+4.3)/40.0 ) )

# Hard code growth for PRK from CIA World Factbook
PRK$mix.gdppcgrowsr <- sqrt(abs(0.8))

latest <- rbind(AFG, ALB, ALG, ANG, ARG, ARM, AUL, AUS, AZE, BAH,
                BNG, BLR, BEL, BEN, BHU, BOL, BOS, BOT, BRA, BUL,
                BFO, BUI, CAM, CAO, CAN, CEN, CHA, CHL, CHN, COL,
                COM, CON, ZAI, COS, CRO, CUB, CYP, CZR, DEN, DJI,
                DOM, ECU, EGY, SAL, EQG, ERI, EST, ETI, FJI, FIN,
                FRN, GAB, GAM, GRG, GER, GHA, GRC, GUA, GUI, GNB,
                GUY, HAI, HON, HUN, IND, INS, IRN, IRQ, IRE, ISR,
                ITA, IVO, JAM, JPN, JOR, KZK, KEN, KUW, KYR, LAO,
                LAT, LEB, LES, LBR, LIB, LIT, MAC, MAG, MAW, MAL,
                MLI, MAA, MAS, MEX, MLD, MON, MNE, MOR, MZM, MYA,
                NAM, NEP, NTH, NEW, NIC, NIR, NIG, PRK, NOR, OMA,
                PAK, PAN, PNG, PAR, PER, PHI, POL, POR, QAT, RUM,
                RUS, RWA, SAU, SEN, SRB, SIE, SIN, SLO, SLV, SOL,
                SOM, SAF, ROK, SSD, SPN, SRI, SUD, SWA, SWD, SWZ,
                SYR, TAJ, TAZ, THI, ETM, TOG, TRI, TUN, TUR, TKM,
                UGA, UKR, UAE, UKG, USA, URU, UZB, VEN, VIE, YEM,
                ZAM, ZIM)

# Function to get predictions from data frame
forecast <- function(df) {
  df$coup.p <- predict(coup, newdata = df, type = "response", na.action = na.exclude)
  df$cwar.p <- predict(cwar, newdata = df, type = "response", na.action = na.exclude)
  df$pitf.p <- predict(pitf, newdata = df, type = "response", na.action = na.exclude)
  df$threat.p <- predict(threat, newdata = df, type = "response", na.action = na.exclude)
  df$harff.p <- predict(harff, newdata = df, type = "response", na.action = na.exclude)
  df$rf.p <- predict(rf, type = "prob", newdata = df, na.action = na.exclude)[,2]
  df$mean.p <- (df$harff.p + df$threat.p + df$rf.p)/3
  df$date <- as.Date(Sys.Date())
  return(df)
}

newcast <- forecast(latest)

write.csv(newcast, "c:/users/jay/documents/ushmm/statrisk2014/outdata/latest.csv", row.names = FALSE)

#####################################
# Plots and Maps
#####################################

require(rworldmap)

newcast$country <- as.character(newcast$country)

# Join the data to a map
map2014 <- joinCountryData2Map(newcast, nameJoinColumn = "country", joinCode = "NAME", verbose = TRUE)

# Correct mismatches
newcast$country[newcast$country=="Timor Leste"] <- "Timor-Leste"
newcast$country[newcast$country=="Congo-Brazzaville"] <- "Congo"
newcast$country[newcast$country=="Congo-Kinshasa"] <- "Democratic Republic of Congo"

# Redo with mismatches corrected
map2014 <- joinCountryData2Map(newcast, nameJoinColumn = "country", joinCode = "NAME", verbose = TRUE)

# Map the scores
date <- as.Date(Sys.Date())
datestring <- paste(substr(date,1,4), substr(date,6,7), substr(date,9,10), sep="")
mapname <- paste("c:/users/jay/documents/ushmm/statrisk2014/figs/heatmap", datestring, "png", sep = ".")
png(mapname, width=800, height=450, bg="white")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
map.score <- mapCountryData(map2014,
  nameColumnToPlot="mean.p",
  addLegend = FALSE,
  numCats = 5, catMethod="logFixedWidth",
  colourPalette = "heat", borderCol = "gray",
  mapTitle = "Risk of Onset of State-Led Mass Killing Episode")
do.call(addMapLegend, c(map.score, legendWidth=0.5, legendMar = 2))
mtext("map made using rworldmap             ", line=-4, side=1, adj=1, cex=0.8)
dev.off()

# Map with color scaled to distance
date <- as.Date(Sys.Date())
datestring <- paste(substr(date,1,4), substr(date,6,7), substr(date,9,10), sep="")
mapname <- paste("c:/users/jay/documents/ushmm/statrisk2014/figs/scaledmap", datestring, "png", sep = ".")
cats <- c(0,0.01,0.02,0.04,0.08,0.16)
cols <- c("lightgoldenrod", "yellow", "orange", "orangered", "orangered4")
png(mapname, width=800, height=450, bg="white")
par(mai=c(0,0,0.2,0), xaxs="i", yaxs="i")
map.score <- mapCountryData(map2014,
  nameColumnToPlot="mean.p",
  addLegend = FALSE,
  catMethod = cats,
  colourPalette = cols,
  borderCol = "white",
  oceanCol = "lightblue",
  mapTitle = "")
legend(x = -170, y = -85, xjust = 0, yjust = 0,
  legend = c("< 1%", "1-2%", "2-4%", "4-8%", "8-16%"),
  fill = cols, border = cols, bty = "n")
text(x = 170, y = -80, "made using rworldmap", adj = 1, cex = 0.8)
dev.off()

# Dot plot of scores
require(Hmisc)
newcast <- newcast[order(-newcast$mean.p),]
condcol <- ifelse(newcast$ongoing==1, "firebrick3", "gray")
date <- as.Date(Sys.Date())
datestring <- paste(substr(date,1,4), substr(date,6,7), substr(date,9,10), sep="")
dotname <- paste("c:/users/jay/documents/ushmm/statrisk2014/figs/dotplot", datestring, "png", sep = ".")
png(file = dotname, width=14, height=40, unit = "cm", bg="white", res=150)
dotchart2(newcast$mean.p, labels=newcast$country,
    lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1, col=condcol, pch=20, cex.labels=0.5 )
title(main=list("Risk of Onset of State-Led Mass Killing Episode", cex=0.9))
dev.off()
