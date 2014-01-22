# MASS KILLING EPISODE DATA MAKER
# Jay Ulfelder
# 2014-01-20

rm(list=ls(all=TRUE)) 

# Create rack of names and years, starting in 1945, with codes
source("c:/users/jay/documents/ushmm/statrisk2014/r/countryyear.rack.maker.R")
rack <- subset(rack, year <= 2013, select=c(country,sftgcode,year))

# For type: 1 = civil war, 2 = uprising, 3 = repression, 4 = other

# Create columns for new variables with zeros
rack$start <- 0
rack$end <- 0
rack$ongoing <- 0
rack$type <- 0
rack$ever <- 0

# Bulgaria, 1944-1956 (repression)
rack$start[rack$country=="Bulgaria" & rack$year==1944] <- 1
rack$end[rack$country=="Bulgaria" & rack$year==1956] <- 1
rack$ongoing[rack$country=="Bulgaria" & rack$year>=1944 & rack$year<=1956] <- 1
rack$type[rack$country=="Bulgaria" & rack$year==1944] <- 3
rack$ever[rack$country=="Bulgaria" & rack$year>=1944] <- 1

# Albania, 1944-1985 (repression)
rack$start[rack$country=="Albania" & rack$year==1944] <- 1
rack$end[rack$country=="Albania" & rack$year==1985] <- 1
rack$ongoing[rack$country=="Albania" & rack$year>=1944 & rack$year<=1985] <- 1
rack$type[rack$country=="Albania" & rack$year==1944] <- 3
rack$ever[rack$country=="Albania" & rack$year>=1944] <- 1

# Poland, 1945-1956 (repression)
rack$start[rack$country=="Poland" & rack$year==1945] <- 1
rack$end[rack$country=="Poland" & rack$year==1956] <- 1
rack$ongoing[rack$country=="Poland" & rack$year>=1945 & rack$year<=1956] <- 1
rack$type[rack$country=="Poland" & rack$year==1945] <- 3
rack$ever[rack$country=="Poland" & rack$year>=1945] <- 1

# Poland, 1945-1948 (expulsion of Germans)
# Subsumed under preceding case

# Poland, 1945-1946 (Ukranian nationalists)
# Subsumed under preceding case

# Hungary, 1945-1960 (repression)
rack$start[rack$country=="Hungary" & rack$year==1945] <- 1
rack$end[rack$country=="Hungary" & rack$year==1960] <- 1
rack$ongoing[rack$country=="Hungary" & rack$year>=1945 & rack$year<=1960] <- 1
rack$type[rack$country=="Hungary" & rack$year==1945] <- 3
rack$ever[rack$country=="Hungary" & rack$year>=1945] <- 1

# Yugoslavia, 1945-1956 (repression)
rack$start[rack$country=="Yugoslavia" & rack$year==1945] <- 1
rack$end[rack$country=="Yugoslavia" & rack$year==1956] <- 1
rack$ongoing[rack$country=="Yugoslavia" & rack$year>=1945 & rack$year<=1956] <- 1
rack$type[rack$country=="Yugoslavia" & rack$year==1945] <- 3
rack$ever[rack$country=="Yugoslavia" & rack$year>=1945] <- 1

# Yugoslavia, 1945-1948 (expulsion of Germans)
# Subsumed under preceding case

# Romania, 1945-1989 (repression)
rack$start[rack$country=="Romania" & rack$year==1945] <- 1
rack$end[rack$country=="Romania" & rack$year==1989] <- 1
rack$ongoing[rack$country=="Romania" & rack$year>=1945 & rack$year<=1989] <- 1
rack$type[rack$country=="Romania" & rack$year==1945] <- 3
rack$ever[rack$country=="Romania" & rack$year>=1945] <- 1

# Czechoslovakia, 1945-1946 (expulsion of Germans)
rack$start[rack$country=="Czechoslovakia" & rack$year==1945] <- 1
rack$end[rack$country=="Czechoslovakia" & rack$year==1946] <- 1
rack$ongoing[rack$country=="Czechoslovakia" & rack$year>=1945 & rack$year<=1946] <- 1
rack$type[rack$country=="Czechoslovakia" & rack$year==1945] <- 4
rack$ever[rack$country=="Czechoslovakia" & rack$year>=1945] <- 1

# Czechoslovakia, 1948-1963 (repression)
rack$start[rack$country=="Czechoslovakia" & rack$year==1948] <- 1
rack$end[rack$country=="Czechoslovakia" & rack$year==1963] <- 1
rack$ongoing[rack$country=="Czechoslovakia" & rack$year>=1948 & rack$year<=1963] <- 1
rack$type[rack$country=="Czechoslovakia" & rack$year==1948] <- 3

# Philippines, 1946-1954 (Huks)
rack$start[rack$country=="Philippines" & rack$year==1946] <- 1
rack$end[rack$country=="Philippines" & rack$year==1954] <- 1
rack$ongoing[rack$country=="Philippines" & rack$year>=1946 & rack$year<=1954] <- 1
rack$type[rack$country=="Philippines" & rack$year==1946] <- 1
rack$ever[rack$country=="Philippines" & rack$year>=1946] <- 1

# Philippines, 1969- (New Peoples Army)
rack$start[rack$country=="Philippines" & rack$year==1969] <- 1
rack$ongoing[rack$country=="Philippines" & rack$year>=1969] <- 1
rack$type[rack$country=="Philippines" & rack$year==1969] <- 1

# Philippines, 1972-1986 (Moro Liberation)
rack$start[rack$country=="Philippines" & rack$year==1972] <- 1
rack$end[rack$country=="Philippines" & rack$year==1986] <- 1
rack$ongoing[rack$country=="Philippines" & rack$year>=1972 & rack$year<=1986] <- 1
rack$type[rack$country=="Philippines" & rack$year==1972] <- 1

# China-Taiwan, 1947
rack$start[rack$country=="China" & rack$year==1947] <- 1
rack$end[rack$country=="China" & rack$year==1947] <- 1
rack$ongoing[rack$country=="China" & rack$year==1947] <- 1
rack$type[rack$country=="China" & rack$year==1947] <- 2
rack$ever[rack$country=="China" & rack$year>=1947] <- 1

# China, 1949-1977 (Communist)
rack$start[rack$country=="China" & rack$year==1949] <- 1
rack$end[rack$country=="China" & rack$year==1977] <- 1
rack$ongoing[rack$country=="China" & rack$year>=1949 & rack$year<=1977] <- 1
rack$type[rack$country=="China" & rack$year==1949] <- 3

# China, 1954-1977 (Tibet)
rack$start[rack$country=="China" & rack$year==1954] <- 1
rack$end[rack$country=="China" & rack$year==1977] <- 1
rack$ongoing[rack$country=="China" & rack$year>=1954 & rack$year<=1977] <- 1
rack$type[rack$country=="China" & rack$year==1954] <- 1

# Burma/Myanmar, 1948- (ethnic separatists)
rack$start[rack$country=="Burma" & rack$year==1948] <- 1
rack$ongoing[rack$country=="Burma" & rack$year>=1948] <- 1
rack$type[rack$country=="Burma" & rack$year==1948] <- 1
rack$ever[rack$country=="Burma" & rack$year>=1948] <- 1

# Burma/Myanmar, 1948-1990 (Communist insurgency/repression)
# Subsumed under preceding case

# Korea, 1948-1950 (civil violence in south - Cheju and Yosu)
rack$start[rack$country=="South Korea" & rack$year==1948] <- 1
rack$end[rack$country=="South Korea" & rack$year==1950] <- 1
rack$ongoing[rack$country=="South Korea" & rack$year>=1948 & rack$year<=1950] <- 1
rack$type[rack$country=="South Korea" & rack$year==1948] <- 1
rack$ever[rack$country=="South Korea" & rack$year>=1948] <- 1

# North Korea, 1948- (repression)
rack$start[rack$country=="North Korea" & rack$year==1948] <- 1
rack$ongoing[rack$country=="North Korea" & rack$year>=1948] <- 1
rack$type[rack$country=="North Korea" & rack$year==1948] <- 3
rack$ever[rack$country=="North Korea" & rack$year>=1948] <- 1

# Guatemala, 1954-1996 (civil war and repression)
rack$start[rack$country=="Guatemala" & rack$year==1954] <- 1
rack$end[rack$country=="Guatemala" & rack$year==1996] <- 1
rack$ongoing[rack$country=="Guatemala" & rack$year>=1954 & rack$year<=1996] <- 1
rack$type[rack$country=="Guatemala" & rack$year==1954] <- 1
rack$ever[rack$country=="Guatemala" & rack$year>=1954] <- 1

# North Vietnam, 1954-1957 (repression)
rack$start[rack$country=="North Vietnam" & rack$year==1954] <- 1
rack$end[rack$country=="North Vietnam" & rack$year==1957] <- 1
rack$ongoing[rack$country=="North Vietnam" & rack$year>=1954 & rack$year<=1957] <- 1
rack$type[rack$country=="North Vietnam" & rack$year==1954] <- 3
rack$ever[rack$country=="North Vietnam" & rack$year>=1954] <- 1

# South Vietnam, 1954-1975 (civil war)
rack$start[rack$country=="South Vietnam" & rack$year==1954] <- 1
rack$end[rack$country=="South Vietnam" & rack$year==1975] <- 1
rack$ongoing[rack$country=="South Vietnam" & rack$year>=1954 & rack$year<=1975] <- 1
rack$type[rack$country=="South Vietnam" & rack$year==1954] <- 1
rack$ever[rack$country=="South Vietnam" & rack$year>=1954] <- 1

# Vietnam, 1975- (post-war repression)
rack$start[rack$country=="Vietnam" & rack$year==1975] <- 1
rack$ongoing[rack$country=="Vietnam" & rack$year>=1975] <- 1
rack$type[rack$country=="Vietnam" & rack$year==1975] <- 1
rack$ever[rack$country=="Vietnam" & rack$year>=1975] <- 1

# Sudan, 1956-1972 (civil war)
rack$start[rack$country=="Sudan" & rack$year==1956] <- 1
rack$end[rack$country=="Sudan" & rack$year==1972] <- 1
rack$ongoing[rack$country=="Sudan" & rack$year>=1956 & rack$year<=1972] <- 1
rack$type[rack$country=="Sudan" & rack$year==1956] <- 1
rack$ever[rack$country=="Sudan" & rack$year>=1956] <- 1

# Sudan, 1983-2005
rack$start[rack$country=="Sudan" & rack$year==1983] <- 1
rack$end[rack$country=="Sudan" & rack$year==2005] <- 1
rack$ongoing[rack$country=="Sudan" & rack$year>=1983 & rack$year<=2005] <- 1
rack$type[rack$country=="Sudan" & rack$year==1983] <- 1

# Haiti, 1958-1986 (Duvalier-repression)
rack$start[rack$country=="Haiti" & rack$year==1958] <- 1
rack$end[rack$country=="Haiti" & rack$year==1986] <- 1
rack$ongoing[rack$country=="Haiti" & rack$year>=1958 & rack$year<=1986] <- 1
rack$type[rack$country=="Haiti" & rack$year==1958] <- 3
rack$ever[rack$country=="Haiti" & rack$year>=1958] <- 1

# Cuba, 1959-1970 (Castro-repression)
rack$start[rack$country=="Cuba" & rack$year==1959] <- 1
rack$end[rack$country=="Cuba" & rack$year==1970] <- 1
rack$ongoing[rack$country=="Cuba" & rack$year>=1959 & rack$year<=1970] <- 1
rack$type[rack$country=="Cuba" & rack$year==1959] <- 3
rack$ever[rack$country=="Cuba" & rack$year>=1959] <- 1

# Iraq, 1959 (Mosul uprising)
rack$start[rack$country=="Iraq" & rack$year==1959] <- 1
rack$end[rack$country=="Iraq" & rack$year==1959] <- 1
rack$ongoing[rack$country=="Iraq" & rack$year==1959] <- 1
rack$type[rack$country=="Iraq" & rack$year==1959] <- 1
rack$ever[rack$country=="Iraq" & rack$year>=1959] <- 1

# Iraq, 1961-1991 (Kurds)
rack$start[rack$country=="Iraq" & rack$year==1961] <- 1
rack$end[rack$country=="Iraq" & rack$year==1991] <- 1
rack$ongoing[rack$country=="Iraq" & rack$year>=1961 & rack$year<=1991] <- 1
rack$type[rack$country=="Iraq" & rack$year==1961] <- 1

# Iraq, 1963-2003 (Saddam-repression)
rack$start[rack$country=="Iraq" & rack$year==1963] <- 1
rack$end[rack$country=="Iraq" & rack$year==2003] <- 1
rack$ongoing[rack$country=="Iraq" & rack$year>=1963 & rack$year<=2003] <- 1
rack$type[rack$country=="Iraq" & rack$year==1963] <- 3

# Guinea, 1960-1980 (Sekou Toure-repression)
rack$start[rack$country=="Guinea" & rack$year==1960] <- 1
rack$end[rack$country=="Guinea" & rack$year==1980] <- 1
rack$ongoing[rack$country=="Guinea" & rack$year>=1960 & rack$year<=1980] <- 1
rack$type[rack$country=="Guinea" & rack$year==1960] <- 3
rack$ever[rack$country=="Guinea" & rack$year>=1960] <- 1

# Laos, 1960-1973 (Communists-civil war)
rack$start[rack$country=="Laos" & rack$year==1960] <- 1
rack$end[rack$country=="Laos" & rack$year==1973] <- 1
rack$ongoing[rack$country=="Laos" & rack$year>=1960 & rack$year<=1973] <- 1
rack$type[rack$country=="Laos" & rack$year==1960] <- 1
rack$ever[rack$country=="Laos" & rack$year>=1960] <- 1

# Laos, 1975- (Communist repression/Hmong civil war)
rack$start[rack$country=="Laos" & rack$year==1975] <- 1
rack$ongoing[rack$country=="Laos" & rack$year>=1975] <- 1
rack$type[rack$country=="Laos" & rack$year==1975] <- 1

# Congo, 1960-1963 (Kasai)
rack$start[rack$country=="Congo-Kinshasa" & rack$year==1960] <- 1
rack$end[rack$country=="Congo-Kinshasa" & rack$year==1963] <- 1
rack$ongoing[rack$country=="Congo-Kinshasa" & rack$year>=1960 & rack$year<=1963] <- 1
rack$type[rack$country=="Congo-Kinshasa" & rack$year==1960] <- 1
rack$ever[rack$country=="Congo-Kinshasa" & rack$year>=1960] <- 1

# Congo, 1964-1965 (CNL-Simbas)
rack$start[rack$country=="Congo-Kinshasa" & rack$year==1964] <- 1
rack$end[rack$country=="Congo-Kinshasa" & rack$year==1965] <- 1
rack$ongoing[rack$country=="Congo-Kinshasa" & rack$year>=1964 & rack$year<=1965] <- 1
rack$type[rack$country=="Congo-Kinshasa" & rack$year==1964] <- 1

# Ethiopia, 1961-1991 (Eritrea-civil war)
rack$start[rack$country=="Ethiopia" & rack$year==1961] <- 1
rack$end[rack$country=="Ethiopia" & rack$year==1991] <- 1
rack$ongoing[rack$country=="Ethiopia" & rack$year>=1961 & rack$year<=1991] <- 1
rack$type[rack$country=="Ethiopia" & rack$year==1961] <- 1
rack$ever[rack$country=="Ethiopia" & rack$year>=1961] <- 1

# Ethiopia, 1974-1991 (political repression by Dergue-Tigre civil war)
rack$start[rack$country=="Ethiopia" & rack$year==1974] <- 1
rack$end[rack$country=="Ethiopia" & rack$year==1991] <- 1
rack$ongoing[rack$country=="Ethiopia" & rack$year>=1974 & rack$year<=1991] <- 1
rack$type[rack$country=="Ethiopia" & rack$year==1974] <- 3

# Ethiopia, 1977-1985 (Ogaden)
rack$start[rack$country=="Ethiopia" & rack$year==1977] <- 1
rack$end[rack$country=="Ethiopia" & rack$year==1985] <- 1
rack$ongoing[rack$country=="Ethiopia" & rack$year>=1977 & rack$year<=1985] <- 1
rack$type[rack$country=="Ethiopia" & rack$year==1977] <- 1

# Rwanda, 1963-1967
rack$start[rack$country=="Rwanda" & rack$year==1963] <- 1
rack$end[rack$country=="Rwanda" & rack$year==1967] <- 1
rack$ongoing[rack$country=="Rwanda" & rack$year>=1963 & rack$year<=1967] <- 1
rack$type[rack$country=="Rwanda" & rack$year==1963] <- 1
rack$ever[rack$country=="Rwanda" & rack$year>=1963] <- 1

# Algeria, 1962 (post-independence retribution)
rack$start[rack$country=="Algeria" & rack$year==1962] <- 1
rack$end[rack$country=="Algeria" & rack$year==1962] <- 1
rack$ongoing[rack$country=="Algeria" & rack$year==1962] <- 1
rack$type[rack$country=="Algeria" & rack$year==1962] <- 1
rack$ever[rack$country=="Algeria" & rack$year>=1962] <- 1

# Yemen, 1962-1970
rack$start[rack$country=="North Yemen" & rack$year==1962] <- 1
rack$end[rack$country=="North Yemen" & rack$year==1970] <- 1
rack$ongoing[rack$country=="North Yemen" & rack$year>=1962 & rack$year<=1970] <- 1
rack$type[rack$country=="North Yemen" & rack$year==1962] <- 1
rack$ever[rack$country=="North Yemen" & rack$year>=1962] <- 1

# Zanzibar, 1964 (political repression)
rack$start[rack$country=="Tanzania" & rack$year==1964] <- 1
rack$end[rack$country=="Tanzania" & rack$year==1964] <- 1
rack$ongoing[rack$country=="Tanzania" & rack$year==1964] <- 1
rack$type[rack$country=="Tanzania" & rack$year==1964] <- 3
rack$ever[rack$country=="Tanzania" & rack$year>=1964] <- 1

# Malawi, 1964-1994 (political repression)
rack$start[rack$country=="Malawi" & rack$year==1964] <- 1
rack$end[rack$country=="Malawi" & rack$year==1994] <- 1
rack$ongoing[rack$country=="Malawi" & rack$year>=1964 & rack$year<=1994] <- 1
rack$type[rack$country=="Malawi" & rack$year==1964] <- 3
rack$ever[rack$country=="Malawi" & rack$year>=1964] <- 1

# Colombia, 1948-1958 (la Violencia) [JU: Is this state-sponsored?]
rack$start[rack$country=="Colombia" & rack$year==1948] <- 1
rack$end[rack$country=="Colombia" & rack$year==1958] <- 1
rack$ongoing[rack$country=="Colombia" & rack$year>=1948 & rack$year<=1958] <- 1
rack$type[rack$country=="Colombia" & rack$year==1948] <- 1
rack$ever[rack$country=="Colombia" & rack$year>=1948] <- 1

# Colombia, 1965- (FARC, ELN, etc.)
rack$start[rack$country=="Colombia" & rack$year==1965] <- 1
rack$ongoing[rack$country=="Colombia" & rack$year>=1965] <- 1
rack$type[rack$country=="Colombia" & rack$year==1965] <- 1

# Dominican Republic, 1965-1978 (civil war)
rack$start[rack$country=="Dominican Republic" & rack$year==1965] <- 1
rack$end[rack$country=="Dominican Republic" & rack$year==1978] <- 1
rack$ongoing[rack$country=="Dominican Republic" & rack$year>=1965 & rack$year<=1978] <- 1
rack$type[rack$country=="Dominican Republic" & rack$year==1965] <- 1
rack$ever[rack$country=="Dominican Republic" & rack$year>=1965] <- 1

# Indonesia, 1949-1962 (Darul Islam)
rack$start[rack$country=="Indonesia" & rack$year==1949] <- 1
rack$end[rack$country=="Indonesia" & rack$year==1962] <- 1
rack$ongoing[rack$country=="Indonesia" & rack$year>=1949 & rack$year<=1962] <- 1
rack$type[rack$country=="Indonesia" & rack$year==1949] <- 1
rack$ever[rack$country=="Indonesia" & rack$year>=1949] <- 1

# Indonesia, 1965-1966 (anti-Communist massacres)
rack$start[rack$country=="Indonesia" & rack$year==1965] <- 1
rack$end[rack$country=="Indonesia" & rack$year==1966] <- 1
rack$ongoing[rack$country=="Indonesia" & rack$year>=1965 & rack$year<=1966] <- 1
rack$type[rack$country=="Indonesia" & rack$year==1965] <- 3

# Indonesia, 1969- (West Papua)
rack$start[rack$country=="Indonesia" & rack$year==1969] <- 1
rack$ongoing[rack$country=="Indonesia" & rack$year>=1969] <- 1
rack$type[rack$country=="Indonesia" & rack$year==1969] <- 1

# Indonesia, 1975-1999 (East Timor)
rack$start[rack$country=="Indonesia" & rack$year==1975] <- 1
rack$end[rack$country=="Indonesia" & rack$year==1999] <- 1
rack$ongoing[rack$country=="Indonesia" & rack$year>=1975 & rack$year<=1999] <- 1
rack$type[rack$country=="Indonesia" & rack$year==1975] <- 1

# Burundi, 1965-1973
rack$start[rack$country=="Burundi" & rack$year==1965] <- 1
rack$end[rack$country=="Burundi" & rack$year==1973] <- 1
rack$ongoing[rack$country=="Burundi" & rack$year>=1965 & rack$year<=1973] <- 1
rack$type[rack$country=="Burundi" & rack$year==1965] <- 1
rack$ever[rack$country=="Burundi" & rack$year>=1965] <- 1

# Cambodia, 1967-1975
rack$start[rack$country=="Cambodia" & rack$year==1967] <- 1
rack$end[rack$country=="Cambodia" & rack$year==1975] <- 1
rack$ongoing[rack$country=="Cambodia" & rack$year>=1967 & rack$year<=1975] <- 1
rack$type[rack$country=="Cambodia" & rack$year==1967] <- 1
rack$ever[rack$country=="Cambodia" & rack$year>=1967] <- 1

# Cambodia, 1975-1979 (Khmer Rouge)
rack$start[rack$country=="Cambodia" & rack$year==1975] <- 1
rack$end[rack$country=="Cambodia" & rack$year==1979] <- 1
rack$ongoing[rack$country=="Cambodia" & rack$year>=1975 & rack$year<=1979] <- 1
rack$type[rack$country=="Cambodia" & rack$year==1975] <- 3

# Nigeria, 1967-1970 (Biafra)
rack$start[rack$country=="Nigeria" & rack$year==1967] <- 1
rack$end[rack$country=="Nigeria" & rack$year==1970] <- 1
rack$ongoing[rack$country=="Nigeria" & rack$year>=1967 & rack$year<=1970] <- 1
rack$type[rack$country=="Nigeria" & rack$year==1967] <- 1
rack$ever[rack$country=="Nigeria" & rack$year>=1967] <- 1

# Nigeria, 1980 (Kano)
rack$start[rack$country=="Nigeria" & rack$year==1980] <- 1
rack$end[rack$country=="Nigeria" & rack$year==1980] <- 1
rack$ongoing[rack$country=="Nigeria" & rack$year==1980] <- 1
rack$type[rack$country=="Nigeria" & rack$year==1980] <- 1

# Equatorial Guinea, 1969-1979
rack$start[rack$country=="Equatorial Guinea" & rack$year==1969] <- 1
rack$end[rack$country=="Equatorial Guinea" & rack$year==1979] <- 1
rack$ongoing[rack$country=="Equatorial Guinea" & rack$year>=1969 & rack$year<=1979] <- 1
rack$type[rack$country=="Equatorial Guinea" & rack$year==1969] <- 3
rack$ever[rack$country=="Equatorial Guinea" & rack$year>=1969] <- 1

# Jordan, 1970-1971 (PLO) [Black September]
rack$start[rack$country=="Jordan" & rack$year==1970] <- 1
rack$end[rack$country=="Jordan" & rack$year==1971] <- 1
rack$ongoing[rack$country=="Jordan" & rack$year>=1970 & rack$year<=1971] <- 1
rack$type[rack$country=="Jordan" & rack$year==1970] <- 1
rack$ever[rack$country=="Jordan" & rack$year>=1970] <- 1

# Uganda, 1971-1979 (Amin)
rack$start[rack$country=="Colombia" & rack$year==1971] <- 1
rack$end[rack$country=="Colombia" & rack$year==1979] <- 1
rack$ongoing[rack$country=="Colombia" & rack$year>=1971 & rack$year<=1979] <- 1
rack$type[rack$country=="Colombia" & rack$year==1971] <- 3
rack$ever[rack$country=="Colombia" & rack$year>=1971] <- 1

# Uganda, 1981-1986 (civil wa)
rack$start[rack$country=="Uganda" & rack$year==1981] <- 1
rack$end[rack$country=="Uganda" & rack$year==1986] <- 1
rack$ongoing[rack$country=="Uganda" & rack$year>=1981 & rack$year<=1986] <- 1
rack$type[rack$country=="Uganda" & rack$year==1981] <- 1
rack$ever[rack$country=="Uganda" & rack$year>=1981] <- 1

#Pakistan, 1971 (Bangladesh)
rack$start[rack$country=="Pakistan" & rack$year==1971] <- 1
rack$end[rack$country=="Pakistan" & rack$year==1971] <- 1
rack$ongoing[rack$country=="Pakistan" & rack$year==1971] <- 1
rack$type[rack$country=="Pakistan" & rack$year==1971] <- 1
rack$ever[rack$country=="Pakistan" & rack$year>=1971] <- 1

#Pakistan, 1973-1977 (Baluchistan)
rack$start[rack$country=="Pakistan" & rack$year==1973] <- 1
rack$end[rack$country=="Pakistan" & rack$year==1977] <- 1
rack$ongoing[rack$country=="Pakistan" & rack$year>=1973 & rack$year<=1977] <- 1
rack$type[rack$country=="Pakistan" & rack$year==1973] <- 1

#Sri Lanka, 1971 (JVP)
rack$start[rack$country=="Sri Lanka" & rack$year==1971] <- 1
rack$end[rack$country=="Sri Lanka" & rack$year==1971] <- 1
rack$ongoing[rack$country=="Sri Lanka" & rack$year==1971] <- 1
rack$type[rack$country=="Sri Lanka" & rack$year==1971] <- 1
rack$ever[rack$country=="Sri Lanka" & rack$year>=1971] <- 1

# Sri Lanka, 1983-2002 (Tamil)
rack$start[rack$country=="Sri Lanka" & rack$year==1983] <- 1
rack$end[rack$country=="Sri Lanka" & rack$year==2002] <- 1
rack$ongoing[rack$country=="Sri Lanka" & rack$year>=1983 & rack$year<=2002] <- 1
rack$type[rack$country=="Sri Lanka" & rack$year==1983] <- 1

# Zimbabwe, 1972-1979 (civil war)
rack$start[rack$country=="Zimbabwe" & rack$year==1972] <- 1
rack$end[rack$country=="Zimbabwe" & rack$year==1979] <- 1
rack$ongoing[rack$country=="Zimbabwe" & rack$year>=1972 & rack$year<=1979] <- 1
rack$type[rack$country=="Zimbabwe" & rack$year==1972] <- 1
rack$ever[rack$country=="Zimbabwe" & rack$year>=1972] <- 1

# Zimbabwe, 1982-1987 (civil war)
rack$start[rack$country=="Zimbabwe" & rack$year==1982] <- 1
rack$end[rack$country=="Zimbabwe" & rack$year==1987] <- 1
rack$ongoing[rack$country=="Zimbabwe" & rack$year>=1982 & rack$year<=1987] <- 1
rack$type[rack$country=="Zimbabwe" & rack$year==1982] <- 1

# Chile, 1973-1978
rack$start[rack$country=="Chile" & rack$year==1973] <- 1
rack$end[rack$country=="Chile" & rack$year==1978] <- 1
rack$ongoing[rack$country=="Chile" & rack$year>=1973 & rack$year<=1978] <- 1
rack$type[rack$country=="Chile" & rack$year==1973] <- 3
rack$ever[rack$country=="Chile" & rack$year>=1973] <- 1

# Nicaragua, 1974-1979 (Somoza) [Sandinista insurgency]
rack$start[rack$country=="Nicaragua" & rack$year==1974] <- 1
rack$end[rack$country=="Nicaragua" & rack$year==1979] <- 1
rack$ongoing[rack$country=="Nicaragua" & rack$year>=1974 & rack$year<=1979] <- 1
rack$type[rack$country=="Nicaragua" & rack$year==1974] <- 1
rack$ever[rack$country=="Nicaragua" & rack$year>=1974] <- 1

# Nicaragua, 1981-1990 (Contras)
rack$start[rack$country=="Nicaragua" & rack$year==1981] <- 1
rack$end[rack$country=="Nicaragua" & rack$year==1990] <- 1
rack$ongoing[rack$country=="Nicaragua" & rack$year>=1981 & rack$year<=1990] <- 1
rack$type[rack$country=="Nicaragua" & rack$year==1981] <- 1

# Mozambique, 1975-1992 (RENAMO)
rack$start[rack$country=="Mozambique" & rack$year==1975] <- 1
rack$end[rack$country=="Mozambique" & rack$year==1992] <- 1
rack$ongoing[rack$country=="Mozambique" & rack$year>=1975 & rack$year<=1992] <- 1
rack$type[rack$country=="Mozambique" & rack$year==1975] <- 1
rack$ever[rack$country=="Mozambique" & rack$year>=1975] <- 1

# Angola, 1975-2002 (civil war)
rack$start[rack$country=="Angola" & rack$year==1975] <- 1
rack$end[rack$country=="Angola" & rack$year==2002] <- 1
rack$ongoing[rack$country=="Angola" & rack$year>=1975 & rack$year<=2002] <- 1
rack$type[rack$country=="Angola" & rack$year==1975] <- 1
rack$ever[rack$country=="Angola" & rack$year>=1975] <- 1

# Argentina, 1976-1983
rack$start[rack$country=="Argentina" & rack$year==1976] <- 1
rack$end[rack$country=="Argentina" & rack$year==1983] <- 1
rack$ongoing[rack$country=="Argentina" & rack$year>=1976 & rack$year<=1983] <- 1
rack$type[rack$country=="Argentina" & rack$year==1976] <- 1
rack$ever[rack$country=="Argentina" & rack$year>=1976] <- 1

# South Africa, 1976-1994
rack$start[rack$country=="South Africa" & rack$year==1976] <- 1
rack$end[rack$country=="South Africa" & rack$year==1994] <- 1
rack$ongoing[rack$country=="South Africa" & rack$year>=1976 & rack$year<=1994] <- 1
rack$type[rack$country=="South Africa" & rack$year==1976] <- 1
rack$ever[rack$country=="South Africa" & rack$year>=1976] <- 1

# El Salvador, 1977-1992 [leftist guerrillas & supposed sympathizers]
rack$start[rack$country=="El Salvador" & rack$year==1977] <- 1
rack$end[rack$country=="El Salvador" & rack$year==1992] <- 1
rack$ongoing[rack$country=="El Salvador" & rack$year>=1977 & rack$year<=1992] <- 1
rack$type[rack$country=="El Salvador" & rack$year==1977] <- 1
rack$ever[rack$country=="El Salvador" & rack$year>=1977] <- 1

# Iran, 1978-1979 (political repression)
rack$start[rack$country=="Iran" & rack$year==1978] <- 1
rack$end[rack$country=="Iran" & rack$year==1979] <- 1
rack$ongoing[rack$country=="Iran" & rack$year>=1978 & rack$year<=1979] <- 1
rack$type[rack$country=="Iran" & rack$year==1978] <- 3
rack$ever[rack$country=="Iran" & rack$year>=1978] <- 1

# Iran, 1979- (post-revolution, Kurds)
rack$start[rack$country=="Iran" & rack$year==1979] <- 1
rack$ongoing[rack$country=="Iran" & rack$year>=1979] <- 1
rack$type[rack$country=="Iran" & rack$year==1979] <- 3

# Afghanistan, 1978-1992
rack$start[rack$country=="Afghanistan" & rack$year==1978] <- 1
rack$end[rack$country=="Afghanistan" & rack$year==1992] <- 1
rack$ongoing[rack$country=="Afghanistan" & rack$year>=1978 & rack$year<=1992] <- 1
rack$type[rack$country=="Afghanistan" & rack$year==1978] <- 1
rack$ever[rack$country=="Afghanistan" & rack$year>=1978] <- 1

# Syria, 1979-1985 (Muslim Brotherhood)
rack$start[rack$country=="Syria" & rack$year==1979] <- 1
rack$end[rack$country=="Syria" & rack$year==1985] <- 1
rack$ongoing[rack$country=="Syria" & rack$year>=1979 & rack$year<=1985] <- 1
rack$type[rack$country=="Syria" & rack$year==1979] <- 1
rack$ever[rack$country=="Syria" & rack$year>=1979] <- 1

# Bangladesh, 1980-1997 (Chittagong Hills insurgency)
rack$start[rack$country=="Bangladesh" & rack$year==1980] <- 1
rack$end[rack$country=="Bangladesh" & rack$year==1997] <- 1
rack$ongoing[rack$country=="Bangladesh" & rack$year>=1980 & rack$year<=1997] <- 1
rack$type[rack$country=="Bangladesh" & rack$year==1980] <- 1
rack$ever[rack$country=="Bangladesh" & rack$year>=1980] <- 1

# Peru, 1980-1992 (Shining Path)
rack$start[rack$country=="Peru" & rack$year==1980] <- 1
rack$end[rack$country=="Peru" & rack$year==1992] <- 1
rack$ongoing[rack$country=="Peru" & rack$year>=1980 & rack$year<=1992] <- 1
rack$type[rack$country=="Peru" & rack$year==1980] <- 1
rack$ever[rack$country=="Peru" & rack$year>=1980] <- 1

# Somalia, 1982-1990 (Barre vs. Issaqs & others)
rack$start[rack$country=="Somalia" & rack$year==1982] <- 1
rack$end[rack$country=="Somalia" & rack$year==1990] <- 1
rack$ongoing[rack$country=="Somalia" & rack$year>=1982 & rack$year<=1990] <- 1
rack$type[rack$country=="Somalia" & rack$year==1982] <- 1
rack$ever[rack$country=="Somalia" & rack$year>=1982] <- 1

# Chad, 1982-1990 (political repression/civil war Habre regime)
rack$start[rack$country=="Chad" & rack$year==1982] <- 1
rack$end[rack$country=="Chad" & rack$year==1990] <- 1
rack$ongoing[rack$country=="Chad" & rack$year>=1982 & rack$year<=1990] <- 1
rack$type[rack$country=="Chad" & rack$year==1982] <- 1
rack$ever[rack$country=="Chad" & rack$year>=1982] <- 1

# India, 1984-1994 (Punjab-Sikh) [insurgency]
rack$start[rack$country=="India" & rack$year==1984] <- 1
rack$end[rack$country=="India" & rack$year==1994] <- 1
rack$ongoing[rack$country=="India" & rack$year>=1984 & rack$year<=1994] <- 1
rack$type[rack$country=="India" & rack$year==1984] <- 1
rack$ever[rack$country=="India" & rack$year>=1984] <- 1

# Turkey, 1984-1999 (Kurds)
rack$start[rack$country=="Turkey" & rack$year==1984] <- 1
rack$end[rack$country=="Turkey" & rack$year==1999] <- 1
rack$ongoing[rack$country=="Turkey" & rack$year>=1984 & rack$year<=1999] <- 1
rack$type[rack$country=="Turkey" & rack$year==1984] <- 1
rack$ever[rack$country=="Turkey" & rack$year>=1984] <- 1

# South Yemen, 1986
rack$start[rack$country=="South Yemen" & rack$year==1986] <- 1
rack$end[rack$country=="South Yemen" & rack$year==1986] <- 1
rack$ongoing[rack$country=="South Yemen" & rack$year==1986] <- 1
rack$type[rack$country=="South Yemen" & rack$year==1986] <- 1
rack$ever[rack$country=="South Yemen" & rack$year>=1986] <- 1

# Uganda, 1986- (LRA)
rack$start[rack$country=="Uganda" & rack$year==1986] <- 1
rack$ongoing[rack$country=="Uganda" & rack$year>=1986] <- 1
rack$type[rack$country=="Uganda" & rack$year==1986] <- 1

# Burma, 1988 (political repression)
rack$start[rack$country=="Burma" & rack$year==1986] <- 1
rack$ongoing[rack$country=="Burma" & rack$year==1986] <- 1
rack$type[rack$country=="Burma" & rack$year==1986] <- 2

# Burundi, 1988-2005
rack$start[rack$country=="Burundi" & rack$year==1988] <- 1
rack$end[rack$country=="Burundi" & rack$year==2005] <- 1
rack$ongoing[rack$country=="Burundi" & rack$year>=1988 & rack$year<=2005] <- 1
rack$type[rack$country=="Burundi" & rack$year==1988] <- 1

# Papua New Guinea, 1988-1998 (Bouganville)
rack$start[rack$country=="Papua New Guinea" & rack$year==1988] <- 1
rack$end[rack$country=="Papua New Guinea" & rack$year==1998] <- 1
rack$ongoing[rack$country=="Papua New Guinea" & rack$year>=1988 & rack$year<=1998] <- 1
rack$type[rack$country=="Papua New Guinea" & rack$year==1988] <- 1
rack$ever[rack$country=="Papua New Guinea" & rack$year>=1988] <- 1

# Indonesia, 1989-2005 (Aceh)
rack$start[rack$country=="Indonesia" & rack$year==1989] <- 1
rack$end[rack$country=="Indonesia" & rack$year==2005] <- 1
rack$ongoing[rack$country=="Indonesia" & rack$year>=1989 & rack$year<=2005] <- 1
rack$type[rack$country=="Indonesia" & rack$year==1989] <- 1

# Sri Lanka, 1989-1992 (JVP2)
rack$start[rack$country=="Sri Lanka" & rack$year==1989] <- 1
rack$end[rack$country=="Sri Lanka" & rack$year==1992] <- 1
rack$ongoing[rack$country=="Sri Lanka" & rack$year>=1989 & rack$year<=1992] <- 1
rack$type[rack$country=="Sri Lanka" & rack$year==1992] <- 1

# Romania, 1989
rack$start[rack$country=="Romania" & rack$year==1989] <- 1
rack$end[rack$country=="Romania" & rack$year==1989] <- 1
rack$ongoing[rack$country=="Romania" & rack$year==1989] <- 1
rack$type[rack$country=="Romania" & rack$year==1989] <- 2

# Liberia, 1989-1990 (civil war)
rack$start[rack$country=="Liberia" & rack$year==1989] <- 1
rack$end[rack$country=="Liberia" & rack$year==1990] <- 1
rack$ongoing[rack$country=="Liberia" & rack$year>=1989 & rack$year<=1990] <- 1
rack$type[rack$country=="Liberia" & rack$year==1989] <- 1
rack$ever[rack$country=="Liberia" & rack$year>=1989] <- 1

# India, 1990-2012 (Kashmir)
rack$start[rack$country=="India" & rack$year==1990] <- 1
rack$end[rack$country=="India" & rack$year==2012] <- 1
rack$ongoing[rack$country=="India" & rack$year>=1990 & rack$year<=2012] <- 1
rack$type[rack$country=="India" & rack$year==1990] <- 1

# Rwanda, 1990-1994 (Hutu-Tutsi)
rack$start[rack$country=="Rwanda" & rack$year==1990] <- 1
rack$end[rack$country=="Rwanda" & rack$year==1994] <- 1
rack$ongoing[rack$country=="Rwanda" & rack$year>=1990 & rack$year<=1994] <- 1
rack$type[rack$country=="Rwanda" & rack$year==1990] <- 1

# Nigeria, 1990-2012 (Niger Delta)
rack$start[rack$country=="Nigeria" & rack$year==1990] <- 1
rack$end[rack$country=="Nigeria" & rack$year==2012] <- 1
rack$ongoing[rack$country=="Nigeria" & rack$year>=1990 & rack$year<=2012] <- 1
rack$type[rack$country=="Nigeria" & rack$year==1990] <- 1

# India, 1990-1991 (Assam)
rack$start[rack$country=="India" & rack$year==1990] <- 1
rack$end[rack$country=="India" & rack$year==1991] <- 1
rack$ongoing[rack$country=="India" & rack$year>=1990 & rack$year<=1991] <- 1
rack$type[rack$country=="India" & rack$year==1990] <- 1

# Chad, 1991-2003 (repression/war)
rack$start[rack$country=="Chad" & rack$year==1991] <- 1
rack$end[rack$country=="Chad" & rack$year==2003] <- 1
rack$ongoing[rack$country=="Chad" & rack$year>=1991 & rack$year<=2003] <- 1
rack$type[rack$country=="Chad" & rack$year==1991] <- 1

# Sierra Leone, 1991-2002
rack$start[rack$country=="Sierra Leone" & rack$year==1991] <- 1
rack$end[rack$country=="Sierra Leone" & rack$year==2002] <- 1
rack$ongoing[rack$country=="Sierra Leone" & rack$year>=1991 & rack$year<=2002] <- 1
rack$type[rack$country=="Sierra Leone" & rack$year==1991] <- 1
rack$ever[rack$country=="Sierra Leone" & rack$year>=1991] <- 1

# Iraq, 1991-2003 (Shiites)
rack$start[rack$country=="Iraq" & rack$year==1991] <- 1
rack$end[rack$country=="Iraq" & rack$year==2003] <- 1
rack$ongoing[rack$country=="Iraq" & rack$year>=1991 & rack$year<=2003] <- 1
rack$type[rack$country=="Iraq" & rack$year==1991] <- 1

# Yugoslavia, 1991-1992 (Croatian civil war)
rack$start[rack$country=="Yugoslavia" & rack$year==1991] <- 1
rack$end[rack$country=="Yugoslavia" & rack$year==1992] <- 1
rack$ongoing[rack$country=="Yugoslavia" & rack$year>=1991 & rack$year<=1992] <- 1
rack$type[rack$country=="Yugoslavia" & rack$year==1991] <- 1

# Algeria, 1991-2005 (Islamists)
rack$start[rack$country=="Algeria" & rack$year==1991] <- 1
rack$end[rack$country=="Algeria" & rack$year==2005] <- 1
rack$ongoing[rack$country=="Algeria" & rack$year>=1991 & rack$year<=2005] <- 1
rack$type[rack$country=="Algeria" & rack$year==1991] <- 1

# Azerbaijan, 1991-1994 (Nagorny Karabakh)
rack$start[rack$country=="Azerbaijan" & rack$year==1991] <- 1
rack$end[rack$country=="Azerbaijan" & rack$year==1994] <- 1
rack$ongoing[rack$country=="Azerbaijan" & rack$year>=1991 & rack$year<=1994] <- 1
rack$type[rack$country=="Azerbaijan" & rack$year==1991] <- 1
rack$ever[rack$country=="Azerbaijan" & rack$year>=1991] <- 1

# Haiti, 1991-1994 (political repression)
rack$start[rack$country=="Haiti" & rack$year==1991] <- 1
rack$end[rack$country=="Haiti" & rack$year==1994] <- 1
rack$ongoing[rack$country=="Haiti" & rack$year>=1991 & rack$year<=1994] <- 1
rack$type[rack$country=="Haiti" & rack$year==1991] <- 3

# Bosnia and Herzegovina, 1992-1995 (Bosnia)
rack$start[rack$country=="Bosnia and Herzegovina" & rack$year==1992] <- 1
rack$end[rack$country=="Bosnia and Herzegovina" & rack$year==1995] <- 1
rack$ongoing[rack$country=="Bosnia and Herzegovina" & rack$year>=1992 & rack$year<=1995] <- 1
rack$type[rack$country=="Bosnia and Herzegovina" & rack$year==1992] <- 1
rack$ever[rack$country=="Bosnia and Herzegovina" & rack$year>=1992] <- 1

# Afghanistan, 1992-1996 (Rabbani/Massoud v. Taliban et al.)
rack$start[rack$country=="Afghanistan" & rack$year==1992] <- 1
rack$end[rack$country=="Afghanistan" & rack$year==1996] <- 1
rack$ongoing[rack$country=="Afghanistan" & rack$year>=1992 & rack$year<=1996] <- 1
rack$type[rack$country=="Afghanistan" & rack$year==1992] <- 1

# Tajikistan, 1992-1997 (United Opposition)
rack$start[rack$country=="Tajikistan" & rack$year==1992] <- 1
rack$end[rack$country=="Tajikistan" & rack$year==1997] <- 1
rack$ongoing[rack$country=="Tajikistan" & rack$year>=1992 & rack$year<=1997] <- 1
rack$type[rack$country=="Tajikistan" & rack$year==1992] <- 1
rack$ever[rack$country=="Tajikistan" & rack$year==1992] <- 1

# Georgia, 1992-1993 (Abkhazia)
rack$start[rack$country=="Georgia" & rack$year==1992] <- 1
rack$end[rack$country=="Georgia" & rack$year==1993] <- 1
rack$ongoing[rack$country=="Georgia" & rack$year>=1992 & rack$year<=1993] <- 1
rack$type[rack$country=="Georgia" & rack$year==1992] <- 1
rack$ever[rack$country=="Georgia" & rack$year>=1992] <- 1

# Rep. of Congo, 1992-1997 (Lissouba regime)
rack$start[rack$country=="Congo-Brazzaville" & rack$year==1992] <- 1
rack$end[rack$country=="Congo-Brazzaville" & rack$year==1997] <- 1
rack$ongoing[rack$country=="Congo-Brazzaville" & rack$year>=1992 & rack$year<=1997] <- 1
rack$type[rack$country=="Congo-Brazzaville" & rack$year==1992] <- 3
rack$ever[rack$country=="Congo-Brazzaville" & rack$year>=1992] <- 1

# Congo-Kinshasa, 1993-1997 (Kabila vs. Mobutu)
rack$start[rack$country=="Congo-Kinshasa" & rack$year==1993] <- 1
rack$end[rack$country=="Congo-Kinshasa" & rack$year==1997] <- 1
rack$ongoing[rack$country=="Congo-Kinshasa" & rack$year>=1993 & rack$year<=1997] <- 1
rack$type[rack$country=="Congo-Kinshasa" & rack$year==1993] <- 1

# Rwanda, 1994-1999 (Tutsi vs. Hutu)
rack$start[rack$country=="Rwanda" & rack$year==1994] <- 1
rack$end[rack$country=="Rwanda" & rack$year==1999] <- 1
rack$ongoing[rack$country=="Rwanda" & rack$year>=1994 & rack$year<=1999] <- 1
rack$type[rack$country=="Rwanda" & rack$year==1994] <- 1

# Russia, 1994-2009 (Chechnya) [end date added]
rack$start[rack$country=="Russia" & rack$year==1994] <- 1
rack$end[rack$country=="Russia" & rack$year==2009] <- 1
rack$ongoing[rack$country=="Russia" & rack$year>=1994 & rack$year<=2009] <- 1
rack$type[rack$country=="Russia" & rack$year==1994] <- 1
rack$ever[rack$country=="Russia" & rack$year>=1994] <- 1

# Nepal, 1995-2006 (Maoists) [end date added]
rack$start[rack$country=="Nepal" & rack$year==1995] <- 1
rack$end[rack$country=="Nepal" & rack$year==2006] <- 1
rack$ongoing[rack$country=="Nepal" & rack$year>=1995 & rack$year<=2006] <- 1
rack$type[rack$country=="Nepal" & rack$year==1995] <- 1
rack$ever[rack$country=="Nepal" & rack$year>=1995] <- 1

# Afghanistan, 1996-2001 (Taliban v. United Front)
rack$start[rack$country=="Afghanistan" & rack$year==1996] <- 1
rack$end[rack$country=="Afghanistan" & rack$year==2001] <- 1
rack$ongoing[rack$country=="Afghanistan" & rack$year>=1996 & rack$year<=2001] <- 1
rack$type[rack$country=="Afghanistan" & rack$year==1996] <- 1

# Congo-Brazzaville, 1997-2003 (Sassou regime)
rack$start[rack$country=="Congo-Brazzaville" & rack$year==1997] <- 1
rack$end[rack$country=="Congo-Brazzaville" & rack$year==2003] <- 1
rack$ongoing[rack$country=="Congo-Brazzaville" & rack$year>=1997 & rack$year<=2003] <- 1
rack$type[rack$country=="Congo-Brazzaville" & rack$year==1997] <- 1

# Former Yugoslavia, 1998-1999 (Kosovo)
rack$start[rack$country=="Serbia and Montenegro" & rack$year==1998] <- 1
rack$end[rack$country=="Serbia and Montenegro" & rack$year==1999] <- 1
rack$ongoing[rack$country=="Serbia and Montenegro" & rack$year>=1998 & rack$year<=1999] <- 1
rack$type[rack$country=="Serbia and Montenegro" & rack$year==1998] <- 1
rack$ever[rack$country=="Serbia and Montenegro" & rack$year>=1998] <- 1

# Congo-Kinshasa, 1998-
rack$start[rack$country=="Congo-Kinshasa" & rack$year==1998] <- 1
rack$ongoing[rack$country=="Congo-Kinshasa" & rack$year>=1998] <- 1
rack$type[rack$country=="Congo-Kinshasa" & rack$year==1998] <- 1

# Liberia, 2000-2003 (civil war-LURD & MODEL)
rack$start[rack$country=="Liberia" & rack$year==2000] <- 1
rack$end[rack$country=="Liberia" & rack$year==2003] <- 1
rack$ongoing[rack$country=="Liberia" & rack$year>=2000 & rack$year<=2003] <- 1
rack$type[rack$country=="Liberia" & rack$year==2000] <- 1

# Sudan, 2003-2009 (Darfur) [end date added]
rack$start[rack$country=="Sudan" & rack$year==2003] <- 1
rack$end[rack$country=="Sudan" & rack$year==2009] <- 1
rack$ongoing[rack$country=="Sudan" & rack$year>=2003 & rack$year<=2009] <- 1
rack$type[rack$country=="Sudan" & rack$year==2003] <- 1

# Sri Lanka, 2009 (northern offensive) [episode added]
rack$start[rack$country=="Sri Lanka" & rack$year==2009] <- 1
rack$end[rack$country=="Sri Lanka" & rack$year==2009] <- 1
rack$ongoing[rack$country=="Sri Lanka" & rack$year==2009] <- 1
rack$type[rack$country=="Sri Lanka" & rack$year==2009] <- 1

# Syria, 2011- (repression of civil resistance) [episode added]
rack$start[rack$country=="Syria" & rack$year==2011] <- 1
rack$ongoing[rack$country=="Syria" & rack$year>=2011] <- 1
rack$type[rack$country=="Syria" & rack$year==2011] <- 2

# Sudan, 2011- (South Kordofan) [episode added]
rack$start[rack$country=="Sudan" & rack$year==2011] <- 1
rack$ongoing[rack$country=="Sudan" & rack$year>=2011] <- 1
rack$type[rack$country=="Sudan" & rack$year==2011] <- 1

# Egypt, 2013- (Muslim Brotherhood) [episode added]
rack$start[rack$country=="Egypt" & rack$year==2013] <- 1
rack$ongoing[rack$country=="Egypt" & rack$year>=2013] <- 1
rack$type[rack$country=="Egypt" & rack$year==2011] <- 2

# Nigeria, 2013- (Boko Haram) [episode added]
rack$start[rack$country=="Nigeria" & rack$year==2013] <- 1
rack$ongoing[rack$country=="Nigeria" & rack$year>=2013] <- 1
rack$type[rack$country=="Nigeria" & rack$year==2013] <- 1

# CAR, 2013- (anti-balaka and bystanders) [episode added]
rack$start[rack$country=="Central African Republic" & rack$year==2013] <- 1
rack$ongoing[rack$country=="Central African Republic" & rack$year>=2013] <- 1
rack$type[rack$country=="Central African Republic" & rack$year==2013] <- 1
rack$ever[rack$country=="Central African Republic" & rack$year>=2013] <- 1

# South Sudan, 2013- (Nuer and others) [episode added]
rack$start[rack$country=="South Sudan" & rack$year==2013] <- 1
rack$ongoing[rack$country=="South Sudan" & rack$year>=2013] <- 1
rack$type[rack$country=="South Sudan" & rack$year==2013] <- 1
rack$ever[rack$country=="South Sudan" & rack$year>=2013] <- 1

# Order by country name and year.
rack <- rack[order(rack$country, rack$year),]

# Write out the file.
write.csv(rack, "c:/users/jay/documents/ushmm/statrisk2014/outdata/masskillings.csv", row.names = FALSE)
