# ECONOMIC GROWTH AND INFLATION MELDED FROM WORLD BANK AND IMF
# Jay Ulfelder
# 2014-01-20

# Measure used in models is categorical: annual % change in GDP per capita < 2%
# WDI is used for historical data, IMF World Economic Outlook for latest year (forecast)

rm(list=ls(all=TRUE))

# Create scaffolding
source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))

# WDI annual percent change in GDP per capita
require(WDI)
wdi <- WDI(country="all", indicator = "NY.GDP.MKTP.KD.ZG", extra = FALSE, start = 1960, end = as.numeric(substr(Sys.Date(),1,4)))
names(wdi) <- c("iso2c", "name", "wdi.gdppcgrow", "year")
data <- wdi
source("C:/users/jay/documents/USHMM/statrisk/R/pitf_code_maker.R")
wdi <- data
require(reshape)
wdi <- rename(wdi, c(code="sftgcode"))
wdi <- subset(wdi, is.na(sftgcode)==FALSE, select=c("sftgcode", "year", "wdi.gdppcgrow"))
rack <- merge(rack, wdi, all.x = TRUE)

# 2013 updates from IMF
# Source: http://www.imf.org/external/pubs/ft/weo/2013/02/weodata/index.aspx
# Download "By countries" > All countries > Continue > 
# [check] Gross domestic product per capita, constant prices (national currency) > 
# [start year -> 2012] [end year -> 2013] [uncheck] Append country/series-specific notes > Prepare report
weo2013 <- read.delim("c:/users/jay/documents/ushmm/statrisk2014/indata/imfgdppc.aspx")
weo2013$g2013 <- as.numeric(gsub(",", "", as.character(weo2013$X2013)))
weo2013$g2012 <- as.numeric(gsub(",", "", as.character(weo2013$X2012)))
weo2013$g2011 <- as.numeric(gsub(",", "", as.character(weo2013$X2011)))
weo2013$g2010 <- as.numeric(gsub(",", "", as.character(weo2013$X2010)))
weo2013$g2009 <- as.numeric(gsub(",", "", as.character(weo2013$X2009)))
weo2013$g2008 <- as.numeric(gsub(",", "", as.character(weo2013$X2008)))
weo2013$g2007 <- as.numeric(gsub(",", "", as.character(weo2013$X2007)))
weo2013$g2006 <- as.numeric(gsub(",", "", as.character(weo2013$X2006)))
weo2013$g2005 <- as.numeric(gsub(",", "", as.character(weo2013$X2005)))
weo2013$g2004 <- as.numeric(gsub(",", "", as.character(weo2013$X2004)))
weo2013$g2003 <- as.numeric(gsub(",", "", as.character(weo2013$X2003)))
weo2013$g2002 <- as.numeric(gsub(",", "", as.character(weo2013$X2002)))
weo2013$g2001 <- as.numeric(gsub(",", "", as.character(weo2013$X2001)))
weo2013$g2000 <- as.numeric(gsub(",", "", as.character(weo2013$X2000)))
weo2013$g1999 <- as.numeric(gsub(",", "", as.character(weo2013$X1999)))
weo2013$g1998 <- as.numeric(gsub(",", "", as.character(weo2013$X1998)))
weo2013$g1997 <- as.numeric(gsub(",", "", as.character(weo2013$X1997)))
weo2013$g1996 <- as.numeric(gsub(",", "", as.character(weo2013$X1996)))
weo2013$g1995 <- as.numeric(gsub(",", "", as.character(weo2013$X1995)))
weo2013$g1994 <- as.numeric(gsub(",", "", as.character(weo2013$X1994)))
weo2013$g1993 <- as.numeric(gsub(",", "", as.character(weo2013$X1993)))
weo2013$g1992 <- as.numeric(gsub(",", "", as.character(weo2013$X1992)))
weo2013$g1991 <- as.numeric(gsub(",", "", as.character(weo2013$X1991)))
weo2013$g1990 <- as.numeric(gsub(",", "", as.character(weo2013$X1990)))
weo2013$g1989 <- as.numeric(gsub(",", "", as.character(weo2013$X1989)))
weo2013$g1988 <- as.numeric(gsub(",", "", as.character(weo2013$X1988)))
weo2013$g1987 <- as.numeric(gsub(",", "", as.character(weo2013$X1987)))
weo2013$g1986 <- as.numeric(gsub(",", "", as.character(weo2013$X1986)))
weo2013$g1985 <- as.numeric(gsub(",", "", as.character(weo2013$X1985)))
weo2013$g1984 <- as.numeric(gsub(",", "", as.character(weo2013$X1984)))
weo2013$g1983 <- as.numeric(gsub(",", "", as.character(weo2013$X1983)))
weo2013$g1982 <- as.numeric(gsub(",", "", as.character(weo2013$X1982)))
weo2013$g1981 <- as.numeric(gsub(",", "", as.character(weo2013$X1981)))
weo2013$g1980 <- as.numeric(gsub(",", "", as.character(weo2013$X1980)))
weo2013 <- subset(weo2013, select=c(1,40:73))
require(reshape)
weomelt <- melt(weo2013)
names(weomelt) <- c("country", "gyear", "imf.gdppc")
weomelt$year <- as.numeric(substr( as.character(weomelt$gyear),2,5  ) )
weomelt$gyear <- NULL
weomelt$name <- as.character(weomelt$country)
data <- weomelt
source("c:/users/jay/documents/ushmm/statrisk/r/pitf_code_maker.r")
weomelt <- data
weomelt <- rename(weomelt, c(code="sftgcode"))
rack <- merge(rack, subset(weomelt, select=c(2,3,5)), all.x = TRUE)
rack <- rack[order(rack$country,rack$year),]
for (i in 1:dim(rack)[1]) rack$imf.gdppcgrow[i] <- ifelse(rack$year[i] > rack$yrborn[i] & is.na(rack$imf.gdppc[i])==FALSE, 
  100 * ((rack$imf.gdppc[i] - rack$imf.gdppc[i-1])/rack$imf.gdppc[i-1]), NA )

# Check to make sure two series are similar (cor ~ 0.82)
cor(subset(rack, select=c(18,20)), use="complete.obs")

# Create meld of two growth series
rack$mix.gdppcgrow <- NA
rack$mix.gdppcgrow <- replace(rack$mix.gdppcgrow, which(is.na(rack$imf.gdppcgrow)==FALSE),
  rack$imf.gdppcgrow[is.na(rack$imf.gdppcgrow)==FALSE] )
rack$mix.gdppcgrow <- replace(rack$mix.gdppcgrow, which(is.na(rack$imf.gdppcgrow)==TRUE & is.na(rack$wdi.gdppcgrow)==FALSE),
  rack$wdi.gdppcgrow[is.na(rack$imf.gdppcgrow)==TRUE & is.na(rack$wdi.gdppcgrow)==FALSE] )

rack$mix.gdppcgrowsr <- ifelse( rack$wdi.gdppcgrow <= 0, -1 * sqrt(abs(rack$wdi.gdppcgrow)), sqrt(abs(rack$wdi.gdppcgrow)) )

# Create slow growth dummy using wdi first, imf if not available
rack$slowgrowth <- ifelse(rack$mix.gdppcgrow < 2, 1, ifelse(is.na(rack$mix.gdppcgrow)==FALSE, 0, NA) )
rack$slowgrowth[is.na(rack$wdi.gdppcgrow)==FALSE] <- ifelse(rack$wdi.gdppcgrow[is.na(rack$wdi.gdppcgrow)==FALSE] < 2, 1, 0)
rack$slowgrowth[is.na(rack$wdi.gdppcgrow)==TRUE & is.na(rack$imf.gdppcgrow)==FALSE] <-
  ifelse(rack$imf.gdppcgrow[is.na(rack$wdi.gdppcgrow)==TRUE & is.na(rack$imf.gdppcgrow)==FALSE] < 2, 1, 0)

# IMF inflation, average consumer prices (percent change)
infl <- read.delim("c:/users/jay/documents/ushmm/statrisk2014/indata/imfinflation.aspx")
infl <- subset(infl, select=c(1,5:38))

infl$X2013 <- as.numeric(as.character(infl$X2013))
infl$X2012 <- as.numeric(as.character(infl$X2012))
infl$X2011 <- as.numeric(as.character(infl$X2011))
infl$X2010 <- as.numeric(as.character(infl$X2010))
infl$X2009 <- as.numeric(as.character(infl$X2009))
infl$X2008 <- as.numeric(as.character(infl$X2008))
infl$X2007 <- as.numeric(as.character(infl$X2007))
infl$X2006 <- as.numeric(as.character(infl$X2006))
infl$X2005 <- as.numeric(as.character(infl$X2005))
infl$X2004 <- as.numeric(as.character(infl$X2004))
infl$X2003 <- as.numeric(as.character(infl$X2003))
infl$X2002 <- as.numeric(as.character(infl$X2002))
infl$X2001 <- as.numeric(as.character(infl$X2001))
infl$X2000 <- as.numeric(as.character(infl$X2000))
infl$X1999 <- as.numeric(as.character(infl$X1999))
infl$X1998 <- as.numeric(as.character(infl$X1998))
infl$X1997 <- as.numeric(as.character(infl$X1997))
infl$X1996 <- as.numeric(as.character(infl$X1996))
infl$X1995 <- as.numeric(as.character(infl$X1995))
infl$X1994 <- as.numeric(as.character(infl$X1994))
infl$X1993 <- as.numeric(as.character(infl$X1993))
infl$X1992 <- as.numeric(as.character(infl$X1992))
infl$X1991 <- as.numeric(as.character(infl$X1991))
infl$X1990 <- as.numeric(as.character(infl$X1990))
infl$X1989 <- as.numeric(as.character(infl$X1989))
infl$X1988 <- as.numeric(as.character(infl$X1988))
infl$X1987 <- as.numeric(as.character(infl$X1987))
infl$X1986 <- as.numeric(as.character(infl$X1986))
infl$X1985 <- as.numeric(as.character(infl$X1985))
infl$X1984 <- as.numeric(as.character(infl$X1984))
infl$X1983 <- as.numeric(as.character(infl$X1983))
infl$X1982 <- as.numeric(as.character(infl$X1982))
infl$X1981 <- as.numeric(as.character(infl$X1981))
infl$X1980 <- as.numeric(as.character(infl$X1980))
require(reshape)
infl <- melt(infl)
names(infl) <- c("country", "gyear", "imf.inflrate")
infl$year <- as.numeric(substr( as.character(infl$gyear),2,5  ) )
infl$gyear <- NULL
infl$name <- as.character(infl$country)
data <- infl
source("c:/users/jay/documents/ushmm/statrisk/r/pitf_code_maker.r")
infl <- data
infl <- rename(infl, c(code="sftgcode"))
rack <- merge(rack, subset(infl, select=c(2,3,5)), all.x = TRUE)
rack <- rack[order(rack$country,rack$year),]

write.csv(rack, "c:/users/jay/documents/ushmm/statrisk2014/outdata/econstats.csv", row.names = FALSE)
