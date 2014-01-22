# COUNTRY-YEAR TSCS RACK MAKER
# Jay Ulfelder
# 2014-01-19

kuntry <- function(country, birth, death) {
              year <- seq(birth, ifelse(death<as.numeric(substr(Sys.Date(),1,4)), death, as.numeric(substr(Sys.Date(),1,4))), 1)
              country <- rep(country, times = length(year))
              yrborn <- rep(birth, times = length(year))
              yrdied <- rep(death, times = length(year))
              z <- cbind(country, year, yrborn, yrdied)
              return(z)
}

# Country list sourced to:
# Wikipedia: http://en.wikipedia.org/wiki/List_of_sovereign_states_by_date_of_formation
# CIA Factbook: https://www.cia.gov/library/publications/the-world-factbook/fields/2088.html

# AFRICA

Algeria <- kuntry("Algeria", 1962, 9999)
Angola <- kuntry("Angola", 1975, 9999)
Benin <- kuntry("Benin", 1960, 9999)
Botswana <- kuntry("Botswana", 1966, 9999)
BurkinaFaso <- kuntry("Burkina Faso", 1960, 9999)
Burundi <- kuntry("Burundi", 1962, 9999) 
Cameroon <- kuntry("Cameroon", 1960, 9999)
CapeVerde <- kuntry("Cape Verde", 1975, 9999)
CAR <- kuntry("Central African Republic", 1960, 9999)
Chad <- kuntry("Chad", 1960, 9999)
Comoros <- kuntry("Comoros", 1975, 9999)
DROC <- kuntry("Congo-Kinshasa", 1960, 9999)
Congo <- kuntry("Congo-Brazzaville", 1960, 9999)
IvoryCoast <- kuntry("Ivory Coast", 1960, 9999)
Djibouti <- kuntry("Djibouti", 1977, 9999)
Egypt <- kuntry("Egypt", 1953, 9999)
EquatorialGuinea <- kuntry("Equatorial Guinea", 1968, 9999)
Eritrea <- kuntry("Eritrea", 1993, 9999)
Ethiopia <- kuntry("Ethiopia", -100, 9999)
Gabon <- kuntry("Gabon", 1960, 9999)
Gambia <- kuntry("Gambia", 1965, 9999)
Ghana <- kuntry("Ghana", 1957, 9999)
Guinea <- kuntry("Guinea", 1958, 9999)
GuineaBissau <- kuntry("Guinea-Bissau", 1974, 9999)
Kenya <- kuntry("Kenya", 1963, 9999)
Lesotho <- kuntry("Lesotho", 1966, 9999)
Liberia <- kuntry("Liberia", 1847, 9999)
Libya <- kuntry("Libya", 1951, 9999)
Madagascar <- kuntry("Madagascar", 1960, 9999)
Malawi <- kuntry("Malawi", 1964, 9999)
Mali <- kuntry("Mali", 1960, 9999)
Mauritania <- kuntry("Mauritania", 1960, 9999)
Mauritius <- kuntry("Mauritius", 1968, 9999)
Morocco <- kuntry("Morocco", 1956, 9999)
Mozambique <- kuntry("Mozambique", 1975, 9999)
Namibia <- kuntry("Namibia", 1990, 9999)
Niger <- kuntry("Niger", 1960, 9999)
Nigeria <- kuntry("Nigeria", 1960, 9999)
Rwanda <- kuntry("Rwanda", 1962, 9999)
SaoTome <- kuntry("Sao Tome and Principe", 1975, 9999)
Senegal <- kuntry("Senegal", 1960, 9999)
Seychelles <- kuntry("Seychelles", 1976, 9999)
SierraLeone <- kuntry("Sierra Leone", 1961, 9999)
Somalia <- kuntry("Somalia", 1960, 9999)
SouthAfrica <- kuntry("South Africa", 1910, 9999)
SouthSudan <- kuntry("South Sudan", 2011, 9999)
Sudan <- kuntry("Sudan", 1956, 9999)
Swaziland <- kuntry("Swaziland", 1968, 9999)
Tanzania <- kuntry("Tanzania", 1961, 9999)
Togo <- kuntry("Togo", 1960, 9999)
Tunisia <- kuntry("Tunisia", 1956, 9999)
Uganda <- kuntry("Uganda", 1962, 9999)
Zambia <- kuntry("Zambia", 1964, 9999)
Zimbabwe <- kuntry("Zimbabwe", 1980, 9999)

africa <- rbind(Algeria, Angola, Benin, Botswana, BurkinaFaso, Burundi, Cameroon, CapeVerde, CAR,
                Chad, Comoros, DROC, Congo, IvoryCoast, Djibouti, Egypt, EquatorialGuinea,
                Eritrea, Ethiopia, Gabon, Gambia, Ghana, Guinea, GuineaBissau, Kenya,
                Lesotho, Liberia, Libya, Madagascar, Malawi, Mali, Mauritania, Mauritius,
                Morocco, Mozambique, Namibia, Niger, Nigeria, Rwanda, Senegal, SierraLeone,
                Somalia, SouthAfrica, SouthSudan, Sudan, Swaziland, Tanzania, Togo, Tunisia,
                Uganda, Zambia, Zimbabwe)

# AMERICAS

Antigua <- kuntry("Antigua and Barbuda", 1981, 9999)
Argentina <- kuntry("Argentina", 1816, 9999)
Bahamas <- kuntry("Bahamas", 1973, 9999)
Barbados <- kuntry("Barbados", 1966, 9999)
Belize <- kuntry("Belize", 1981, 9999)
Bolivia <- kuntry("Bolivia", 1825, 9999)
Brazil <- kuntry("Brazil", 1825, 9999)
Canada <- kuntry("Canada", 1867, 9999)
Chile <- kuntry("Chile", 1818, 9999)
Colombia <- kuntry("Colombia", 1819, 9999)
CostaRica <- kuntry("Costa Rica", 1821, 9999)
Cuba <- kuntry("Cuba", 1868, 9999)
Dominica <- kuntry("Dominica", 1978, 9999)
DominicanRepublic <- kuntry("Dominican Republic", 1865, 9999)
Ecuador <- kuntry("Ecuador", 1830, 9999)
ElSalvador <- kuntry("El Salvador", 1841, 9999)
Grenada <- kuntry("Grenada", 1974, 9999)
Guatemala <- kuntry("Guatemala", 1839, 9999)
Guyana <- kuntry("Guyana", 1966, 9999)
Haiti <- kuntry("Haiti", 1804, 9999)
Honduras <- kuntry("Honduras", 1838, 9999)
Jamaica <- kuntry("Jamaica", 1962, 9999)
Mexico <- kuntry("Mexico", 1821, 9999)
Nicaragua <- kuntry("Nicaragua", 1838, 9999)
Panama <- kuntry("Panama", 1903, 9999)
Paraguay <- kuntry("Paraguay", 1811, 9999)
Peru <- kuntry("Peru", 1879, 9999)
StKitts <- kuntry("Saint Kitts and Nevis", 1983, 9999)
StLucia <- kuntry("Saint Lucia", 1979, 9999)
StVincent <- kuntry("Saint Vincent and the Grenadines", 1979, 9999)
Suriname <- kuntry("Suriname", 1975, 9999)
Trinidad <- kuntry("Trinidad and Tobago", 1962, 9999)
USA <- kuntry("United States", 1783, 9999)
Uruguay <- kuntry("Uruguay", 1825, 9999)
Venezuela <- kuntry("Venezuela", 1830, 9999)

americas <- rbind(Argentina, Bahamas, Barbados, Belize, Bolivia, Brazil, Canada, Chile,
                  Colombia, CostaRica, Cuba, DominicanRepublic, Ecuador, ElSalvador,
                  Guatemala, Guyana, Haiti, Honduras, Jamaica, Mexico, Nicaragua,
                  Panama, Paraguay, Peru, Trinidad, USA, Uruguay, Venezuela)

# ASIA

Afghanistan <- kuntry("Afghanistan", 1919, 9999)
Bahrain <- kuntry("Bahrain", 1971, 9999)
Bangladesh <- kuntry("Bangladesh", 1971, 9999)
Bhutan <- kuntry("Bhutan", 1885, 9999)
Brunei <- kuntry("Brunei", 1984, 9999)
Cambodia <- kuntry("Cambodia", 1953, 9999)
China <- kuntry("China", -221, 9999)
India <- kuntry("India", 1947, 9999)
Indonesia <- kuntry("Indonesia", 1949, 9999)
Iran <- kuntry("Iran", 1501, 9999)
Iraq <- kuntry("Iraq", 1932, 9999)
Israel <- kuntry("Israel", 1948, 9999)
Japan <- kuntry("Japan", 660, 9999)
Jordan <- kuntry("Jordan", 1946, 9999)
Kuwait <- kuntry("Kuwait", 1961, 9999)
Laos <- kuntry("Laos", 1949, 9999)
Lebanon <- kuntry("Lebanon", 1943, 9999)
Malaysia <- kuntry("Malaysia", 1957, 9999)
Maldives <- kuntry("Maldives", 1965, 9999)
Mongolia <- kuntry("Mongolia", 1911, 9999)
Myanmar <- kuntry("Myanmar", 1948, 9999)
Nepal <- kuntry("Nepal", 1768, 9999)
NorthKorea <- kuntry("North Korea", 1948, 9999)
Oman <- kuntry("Oman", 1650, 9999)
Pakistan <- kuntry("Pakistan", 1947, 9999)
Philippines <- kuntry("Philippines", 1898, 9999)
Qatar <- kuntry("Qatar", 1971, 9999)
SaudiArabia <- kuntry("Saudi Arabia", 1932, 9999)
Singapore <- kuntry("Singapore", 1965, 9999)
SouthKorea <- kuntry("South Korea", 1948, 9999)
SriLanka <- kuntry("Sri Lanka", 1972, 9999)
Syria <- kuntry("Syria", 1946, 9999)
Taiwan <- kuntry("Taiwan", 1949, 9999)
Thailand <- kuntry("Thailand", 1350, 9999)
TimorLeste <- kuntry("Timor Leste", 2002, 9999)
UAE <- kuntry("United Arab Emirates", 1971, 9999)
Vietnam <- kuntry("Vietnam", 1976, 9999)
Yemen <- kuntry("Yemen", 1990, 9999)

asia <- rbind(Afghanistan, Bahrain, Bangladesh, Bhutan, Cambodia, China, India,
              Indonesia, Iran, Iraq, Israel, Japan, Jordan, Kuwait, Laos, Lebanon,
              Malaysia, Mongolia, Myanmar, Nepal, NorthKorea, Oman, Pakistan,
              Philippines, Qatar, SaudiArabia, Singapore, SouthKorea, SriLanka,
              Syria, Taiwan, Thailand, TimorLeste, UAE, Vietnam, Yemen) 

# EUROPE

Albania <- kuntry("Albania", 1912, 9999)
Andorra <- kuntry("Andorra", 1813, 9999)
Austria <- kuntry("Austria", 1918, 9999)
Belarus <- kuntry("Belarus", 1991, 9999)
Belgium <- kuntry("Belgium", 1830, 9999)
Bosnia <- kuntry("Bosnia and Herzegovina", 1992, 9999)
Bulgaria <- kuntry("Bulgaria", 1878, 9999)
Croatia <- kuntry("Croatia", 1991, 9999)
Cyprus <- kuntry("Cyprus", 1960, 9999)
CzechRepublic <- kuntry("Czech Republic", 1993, 9999)
Denmark <- kuntry("Denmark", 965, 9999)
Estonia <- kuntry("Estonia", 1991, 9999)
Finland <- kuntry("Finland", 1918, 9999)
France <- kuntry("France", 843, 9999)
Germany <- kuntry("Germany", 1990, 9999)
Greece <- kuntry("Greece", 1832, 9999)
Hungary <- kuntry("Hungary", 1849, 9999)
Iceland <- kuntry("Iceland", 1944, 9999)
Ireland <- kuntry("Ireland", 1922, 9999)
Italy <- kuntry("Italy", 1861, 9999)
Kosovo <- kuntry("Kosovo", 2008, 9999)
Latvia <- kuntry("Latvia", 1991, 9999)
Liechtenstein <- kuntry("Liechtenstein", 1813, 9999)
Lithuania <- kuntry("Lithuania", 1991, 9999)
Luxembourg <- kuntry("Luxembourg", 1890, 9999)
Macedonia <- kuntry("Macedonia", 1991, 9999)
Malta <- kuntry("Malta", 1964, 9999)
Moldova <- kuntry("Moldova", 1991, 9999)
Monaco <- kuntry("Monaco", 1861, 9999)
Montenegro <- kuntry("Montenegro", 2006, 9999)
Netherlands <- kuntry("Netherlands", 1568, 9999)
Norway <- kuntry("Norway", 1905, 9999)
Poland <- kuntry("Poland", 1918, 9999)
Portugal <- kuntry("Portugal", 1143, 9999)
Romania <- kuntry("Romania", 1877, 9999)
SanMarino <- kuntry("San Marino", 301, 9999)
Serbia <- kuntry("Serbia", 2006, 9999)
Slovakia <- kuntry("Slovakia", 1993, 9999)
Slovenia <- kuntry("Slovenia", 1991, 9999)
Spain <- kuntry("Spain", 1492, 9999)
Sweden <- kuntry("Sweden", 1523, 9999)
Switzerland <- kuntry("Switzerland", 1291, 9999)
Turkey <- kuntry("Turkey", 1923, 9999)
Ukraine <- kuntry("Ukraine", 1991, 9999)
UnitedKingdom <- kuntry("United Kingdom", 1536, 9999)
Armenia <- kuntry("Armenia", 1991, 9999)
Azerbaijan <- kuntry("Azerbaijan", 1991, 9999)
Georgia <- kuntry("Georgia", 1991, 9999)
Kazakhstan <- kuntry("Kazakhstan", 1991, 9999)
Kyrgyzstan <- kuntry("Kyrgyzstan", 1991, 9999)
Russia <- kuntry("Russia", 1991, 9999)
Tajikistan <- kuntry("Tajikistan", 1991, 9999)
Turkmenistan <- kuntry("Turkmenistan", 1991, 9999)
Uzbekistan <- kuntry("Uzbekistan", 1991, 9999)

europe <- rbind(Albania, Austria, Belgium, Bosnia, Bulgaria, Croatia, Cyprus,
                CzechRepublic, Denmark, Finland, France, Germany, Greece, Hungary,
                Ireland, Italy, Macedonia, Montenegro, Netherlands, Norway, Poland,
                Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Switzerland,
                Sweden, Turkey, UnitedKingdom)

fsu <- rbind(Armenia, Azerbaijan, Belarus, Estonia, Georgia, Kazakhstan,
             Kyrgyzstan, Latvia, Lithuania, Moldova, Russia, Tajikistan,
             Turkmenistan, Ukraine, Uzbekistan) 

# OCEANIA

Australia <- kuntry("Australia", 1901, 9999)
Fiji <- kuntry("Fiji", 1970, 9999)
Kiribati <- kuntry("Kiribati", 1979, 9999)
MarshallIslands <- kuntry("Marshall Islands", 1979, 9999)
Micronesia <- kuntry("Federated States of Micronesia", 1979, 9999)
Nauru <- kuntry("Nauru", 1968, 9999)
NewZealand <- kuntry("New Zealand", 1907, 9999)
Palau <- kuntry("Palau", 1981, 9999)
PapuaNewGuinea <- kuntry("Papua New Guinea", 1975, 9999)
Samoa <- kuntry("Samoa", 1962, 9999)
SolomonIslands <- kuntry("Solomon Islands", 1978, 9999)
Tonga <- kuntry("Tonga", 1970, 9999)
Tuvalu <- kuntry("Tuvalu", 1978, 9999)
Vanuatu <- kuntry("Vanuatu", 1980, 9999)

oceania <- rbind(Australia, Fiji, NewZealand, PapuaNewGuinea, SolomonIslands)

# DEFUNCT

Czechoslovakia <- kuntry("Czechoslovakia", 1918, 1992)
Yugoslavia <- kuntry("Yugoslavia", 1918, 1992)
FedRepYugoslavia <- kuntry("Federal Republic of Yugoslavia", 1992, 2002)
SerbiaMontenegro <- kuntry("Serbia and Montenegro", 2003, 2006)
WestGermany <- kuntry("West Germany", 1945, 1989)
EastGermany <- kuntry("East Germany", 1945, 1989)
USSR <- kuntry("Soviet Union", 1922, 1991)
NorthYemen <- kuntry("North Yemen", 1918, 1990)
SouthYemen <- kuntry("South Yemen", 1967, 1990)
NorthVietnam <- kuntry("North Vietnam", 1954, 1976)
SouthVietnam <- kuntry("South Vietnam", 1954, 1976)

defunct <- rbind(Czechoslovakia, Yugoslavia, FedRepYugoslavia, SerbiaMontenegro,
                 WestGermany, EastGermany, USSR, NorthYemen, SouthYemen,
                 NorthVietnam, SouthVietnam)

# Aggregate and fix types
rack <- as.data.frame(rbind(africa, americas, asia, europe, fsu, oceania, defunct))
rack$country <- as.character(rack$country)
rack$year <- as.numeric(as.character(rack$year))
rack$yrborn <- as.numeric(as.character(rack$yrborn))
rack$yrdied <- as.numeric(as.character(rack$yrdied))

# Trim at 1945
rack <- rack[which(rack$year>=1945),]

###################################
# Add codes
###################################

# PITF Country Code
rack$sftgcode <- NA
rack$sftgcode[rack$country=="Afghanistan"] <- 'AFG'
rack$sftgcode[rack$country=="Albania"] <- 'ALB'
rack$sftgcode[rack$country=="Algeria"] <- 'ALG'
rack$sftgcode[rack$country=="Angola"] <- 'ANG'
rack$sftgcode[rack$country=="Argentina"] <- 'ARG'
rack$sftgcode[rack$country=="Armenia"] <- 'ARM'
rack$sftgcode[rack$country=="Australia"] <- 'AUL'
rack$sftgcode[rack$country=="Austria"] <- 'AUS'
rack$sftgcode[rack$country=="Azerbaijan"] <- 'AZE'
rack$sftgcode[rack$country=="Bahamas"] <- 'BHM'
rack$sftgcode[rack$country=="Bahamas, The"] <- 'BHM'
rack$sftgcode[rack$country=="The Bahamas"] <- 'BHM'
rack$sftgcode[rack$country=="Bahrain"] <- 'BAH'
rack$sftgcode[rack$country=="Bangladesh"] <- 'BNG'
rack$sftgcode[rack$country=="Barbados"] <- 'BAR'
rack$sftgcode[rack$country=="Belarus"] <- 'BLR'
rack$sftgcode[rack$country=="Belgium"] <- 'BEL'
rack$sftgcode[rack$country=="Belize"] <- 'BLZ'
rack$sftgcode[rack$country=="Benin"] <- 'BEN'
rack$sftgcode[rack$country=="Bhutan"] <- 'BHU'
rack$sftgcode[rack$country=="Bolivia"] <- 'BOL'
rack$sftgcode[rack$country=="Bosnia"] <- 'BOS'
rack$sftgcode[rack$country=="Bosnia and Herzegovina"] <- 'BOS'
rack$sftgcode[rack$country=="Bosnia-Herzegovina"] <- 'BOS'
rack$sftgcode[rack$country=="Botswana"] <- 'BOT'
rack$sftgcode[rack$country=="Brazil"] <- 'BRA'
rack$sftgcode[rack$country=="Brunei"] <- 'BRU'
rack$sftgcode[rack$country=="Bulgaria"] <- 'BUL'
rack$sftgcode[rack$country=="Burkina Faso"] <- 'BFO'
rack$sftgcode[rack$country=="Burma"] <- 'MYA'
rack$sftgcode[rack$country=="Myanmar"] <- 'MYA'
rack$sftgcode[rack$country=="Myanmar (Burma)"] <- 'MYA'
rack$sftgcode[rack$country=="Burundi"] <- 'BUI'
rack$sftgcode[rack$country=="Bangladesh"] <- 'BNG'
rack$sftgcode[rack$country=="Cambodia"] <- 'CAM'
rack$sftgcode[rack$country=="Cape Verde"] <- 'CAP'
rack$sftgcode[rack$country=="Cameroon"] <- 'CAO'
rack$sftgcode[rack$country=="Canada"] <- 'CAN'
rack$sftgcode[rack$country=="Central African Republic"] <- 'CEN'
rack$sftgcode[rack$country=="Chad"] <- 'CHA'
rack$sftgcode[rack$country=="Chile"] <- 'CHL'
rack$sftgcode[rack$country=="China"] <- 'CHN'
rack$sftgcode[rack$country=="Colombia"] <- 'COL'
rack$sftgcode[rack$country=="Comoros"] <- 'COM'
rack$sftgcode[rack$country=="Congo-Brazzaville"] <- 'CON'
rack$sftgcode[rack$country=="Republic of Congo"] <- 'CON'
rack$sftgcode[rack$country=="Congo, Republic of"] <- 'CON'
rack$sftgcode[rack$country=="Congo (Brazzaville)"] <- 'CON'
rack$sftgcode[rack$country=="Republic of the Congo"] <- 'CON'
rack$sftgcode[rack$country=="Congo Brazzaville"] <- 'CON'
rack$sftgcode[rack$country=="Congo"] <- 'CON'
rack$sftgcode[rack$country=="Congo, Rep."] <- 'CON'
rack$sftgcode[rack$country=="Congo-Kinshasa"] <- 'ZAI'
rack$sftgcode[rack$country=="Zaire"] <- 'ZAI'
rack$sftgcode[rack$country=="Zaire/DRC"] <- 'ZAI'
rack$sftgcode[rack$country=="Democratic Republic of Congo"] <- 'ZAI'
rack$sftgcode[rack$country=="Democratic Republic of the Congo"] <- 'ZAI'
rack$sftgcode[rack$country=="Congo, Democratic Republic of"] <- 'ZAI'
rack$sftgcode[rack$country=="Congo, Dem. Rep."] <- 'ZAI'
rack$sftgcode[rack$country=="Congo (Kinshasa)"] <- 'ZAI'
rack$sftgcode[rack$country=="Congo Kinshasa"] <- 'ZAI'
rack$sftgcode[rack$country=="DROC"] <- 'ZAI'
rack$sftgcode[rack$country=="Costa Rica"] <- 'COS'
rack$sftgcode[rack$country=="Cote d'Ivoire"] <- 'IVO'
rack$sftgcode[rack$country=="Ivory Coast"] <- 'IVO'
rack$sftgcode[rack$country=="Cote d Ivoire"] <- 'IVO'
rack$sftgcode[rack$country=="Ivory Coast (Cote d'Ivoire)"] <- 'IVO'
rack$sftgcode[rack$country=="Côte d'Ivoire"] <- 'IVO'
rack$sftgcode[rack$country=="Croatia"] <- 'CRO'
rack$sftgcode[rack$country=="Cuba"] <- 'CUB'
rack$sftgcode[rack$country=="Cyprus"] <- 'CYP'
rack$sftgcode[rack$country=="Czech Republic"] <- 'CZR'
rack$sftgcode[rack$country=="Czechoslovakia"] <- 'CZE'
rack$sftgcode[rack$country=="Denmark"] <- 'DEN'
rack$sftgcode[rack$country=="Djibouti"] <- 'DJI'
rack$sftgcode[rack$country=="Dominican Republic"] <- 'DOM'
rack$sftgcode[rack$country=="Dominican Rep"] <- 'DOM'
rack$sftgcode[rack$country=="East Timor"] <- 'ETM'
rack$sftgcode[rack$country=="Timor"] <- 'ETM'
rack$sftgcode[rack$country=="Timor Leste"] <- 'ETM'
rack$sftgcode[rack$country=="East Timor (Timor L'este)"] <- 'ETM'
rack$sftgcode[rack$country=="Timor-Leste"] <- 'ETM'
rack$sftgcode[rack$country=="Ecuador"] <- 'ECU'
rack$sftgcode[rack$country=="Egypt"] <- 'EGY'
rack$sftgcode[rack$country=="Egypt, Arab Rep."] <- 'EGY'
rack$sftgcode[rack$country=="El Salvador"] <- 'SAL'
rack$sftgcode[rack$country=="Equatorial Guinea"] <- 'EQG'
rack$sftgcode[rack$country=="Eritrea"] <- 'ERI'
rack$sftgcode[rack$country=="Estonia"] <- 'EST'
rack$sftgcode[rack$country=="Ethiopia" & rack$year >= 1993] <- 'ETI'
rack$sftgcode[rack$country=="Ethiopia" & rack$year < 1993] <- 'ETH'
rack$sftgcode[rack$country=="Fiji"] <- 'FJI'
rack$sftgcode[rack$country=="Finland"] <- 'FIN'
rack$sftgcode[rack$country=="Finland "] <- 'FIN'
rack$sftgcode[rack$country=="France"] <- 'FRN'
rack$sftgcode[rack$country=="Estonia"] <- 'EST'
rack$sftgcode[rack$country=="Gabon"] <- 'GAB'
rack$sftgcode[rack$country=="Gambia"] <- 'GAM'
rack$sftgcode[rack$country=="The Gambia"] <- 'GAM'
rack$sftgcode[rack$country=="Gambia, the"] <- 'GAM'
rack$sftgcode[rack$country=="Gambia, The"] <- 'GAM'
rack$sftgcode[rack$country=="Georgia"] <- 'GRG'
rack$sftgcode[rack$country=="Germany"] <- 'GER'
rack$sftgcode[rack$country=="East Germany"] <- 'GDR'
rack$sftgcode[rack$country=="Germany, East"] <- 'GDR'
rack$sftgcode[rack$country=="German Democratic Republic"] <- 'GDR'
rack$sftgcode[rack$country=="Germany East"] <- 'GDR'
rack$sftgcode[rack$country=="Germany, E. "] <- 'GDR'
rack$sftgcode[rack$country=="West Germany"] <- 'GFR'
rack$sftgcode[rack$country=="Germany, West"] <- 'GFR'
rack$sftgcode[rack$country=="Federal Republic of Germany"] <- 'GFR'
rack$sftgcode[rack$country=="German Federal Republic"] <- 'GFR'
rack$sftgcode[rack$country=="Germany West"] <- 'GFR'
rack$sftgcode[rack$country=="Germany, W. "] <- 'GFR'
rack$sftgcode[rack$country=="Germany" & rack$year <= 1989] <- 'GFR'
rack$sftgcode[rack$country=="Ghana"] <- 'GHA'
rack$sftgcode[rack$country=="Greece"] <- 'GRC'
rack$sftgcode[rack$country=="Guatemala"] <- 'GUA'
rack$sftgcode[rack$country=="Guinea"] <- 'GUI'
rack$sftgcode[rack$country=="Guinea-Bissau"] <- 'GNB'
rack$sftgcode[rack$country=="Guinea Bissau"] <- 'GNB'
rack$sftgcode[rack$country=="West Germany"] <- 'GFR'
rack$sftgcode[rack$country=="Guyana"] <- 'GUY'
rack$sftgcode[rack$country=="Haiti"] <- 'HAI'
rack$sftgcode[rack$country=="Honduras"] <- 'HON'
rack$sftgcode[rack$country=="Hungary"] <- 'HUN'
rack$sftgcode[rack$country=="Iceland"] <- 'ICE'
rack$sftgcode[rack$country=="India"] <- 'IND'
rack$sftgcode[rack$country=="Indonesia"] <- 'INS'
rack$sftgcode[rack$country=="Guyana"] <- 'GUY'
rack$sftgcode[rack$country=="Iran"] <- 'IRN'
rack$sftgcode[rack$country=="Iran, Islamic Rep."] <- 'IRN'
rack$sftgcode[rack$country=="Islamic Republic of Iran"] <- 'IRN'
rack$sftgcode[rack$country=="Iraq"] <- 'IRQ'
rack$sftgcode[rack$country=="Ireland"] <- 'IRE'
rack$sftgcode[rack$country=="Israel"] <- 'ISR'
rack$sftgcode[rack$country=="Israel and Occupied Territories**"] <- 'ISR'
rack$sftgcode[rack$country=="Israel, pre-1967 borders"] <- 'ISR'
rack$sftgcode[rack$country=="Italy"] <- 'ITA'
rack$sftgcode[rack$country=="Jamaica"] <- 'JAM'
rack$sftgcode[rack$country=="Japan"] <- 'JPN'
rack$sftgcode[rack$country=="Jordan"] <- 'JOR'
rack$sftgcode[rack$country=="Kazakhstan"] <- 'KZK'
rack$sftgcode[rack$country=="Kenya"] <- 'KEN'
rack$sftgcode[rack$country=="Korea, North"] <- 'PRK'
rack$sftgcode[rack$country=="North Korea"] <- 'PRK'
rack$sftgcode[rack$country=="People's Republic of Korea"] <- 'PRK'
rack$sftgcode[rack$country=="Korea North"] <- 'PRK'
rack$sftgcode[rack$country=="Korea, Dem. Rep."] <- 'PRK'
rack$sftgcode[rack$country=="North Korea (Democratc People's Republic of Korea)"] <- 'PRK'
rack$sftgcode[rack$country=="Korea, South"] <- 'ROK'
rack$sftgcode[rack$country=="South Korea"] <- 'ROK'
rack$sftgcode[rack$country=="Korea South"] <- 'ROK'
rack$sftgcode[rack$country=="Republic of Korea"] <- 'ROK'
rack$sftgcode[rack$country=="Korea, Rep."] <- 'ROK'
rack$sftgcode[rack$country=="South Korea (Republic of Korea)"] <- 'ROK'
rack$sftgcode[rack$country=="Korea"] <- 'ROK'
rack$sftgcode[rack$country=="Kuwait"] <- 'KUW'
rack$sftgcode[rack$country=="Kyrgyzstan"] <- 'KYR'
rack$sftgcode[rack$country=="Kyrgyz Republic"] <- 'KYR'
rack$sftgcode[rack$country=="Laos"] <- 'LAO'
rack$sftgcode[rack$country=="Lao PDR"] <- 'LAO'
rack$sftgcode[rack$country=="Lao P.D.R."] <- 'LAO'
rack$sftgcode[rack$country=="Latvia"] <- 'LAT'
rack$sftgcode[rack$country=="Lebanon"] <- 'LEB'
rack$sftgcode[rack$country=="Lesotho"] <- 'LES'
rack$sftgcode[rack$country=="Liberia"] <- 'LBR'
rack$sftgcode[rack$country=="Libya"] <- 'LIB'
rack$sftgcode[rack$country=="Lithuania"] <- 'LIT'
rack$sftgcode[rack$country=="Luxembourg"] <- 'LUX'
rack$sftgcode[rack$country=="Macedonia"] <- 'MAC'
rack$sftgcode[rack$country=="Macedonia, FYR"] <- 'MAC'
rack$sftgcode[rack$country=="FYR Macedonia"] <- 'MAC'
rack$sftgcode[rack$country=="Madagascar"] <- 'MAG'
rack$sftgcode[rack$country=="Malawi"] <- 'MAW'
rack$sftgcode[rack$country=="Malaysia"] <- 'MAL'
rack$sftgcode[rack$country=="Maldives"] <- 'MAD'
rack$sftgcode[rack$country=="Mali"] <- 'MLI'
rack$sftgcode[rack$country=="Malta"] <- 'MLT'
rack$sftgcode[rack$country=="Mauritania"] <- 'MAA'
rack$sftgcode[rack$country=="Mauritius"] <- 'MAS'
rack$sftgcode[rack$country=="Mexico"] <- 'MEX'
rack$sftgcode[rack$country=="Moldova"] <- 'MLD'
rack$sftgcode[rack$country=="Mongolia"] <- 'MON'
rack$sftgcode[rack$country=="Montenegro"] <- 'MNE'
rack$sftgcode[rack$country=="Morocco"] <- 'MOR'
rack$sftgcode[rack$country=="Mozambique"] <- 'MZM'
rack$sftgcode[rack$country=="Namibia"] <- 'NAM'
rack$sftgcode[rack$country=="Nepal"] <- 'NEP'
rack$sftgcode[rack$country=="Netherlands"] <- 'NTH'
rack$sftgcode[rack$country=="New Zealand"] <- 'NEW'
rack$sftgcode[rack$country=="Nicaragua"] <- 'NIC'
rack$sftgcode[rack$country=="Nigeria"] <- 'NIG'
rack$sftgcode[rack$country=="Niger"] <- 'NIR'
rack$sftgcode[rack$country=="Norway"] <- 'NOR'
rack$sftgcode[rack$country=="Oman"] <- 'OMA'
rack$sftgcode[rack$country=="Panama"] <- 'PAN'
rack$sftgcode[rack$country=="Pakistan" & rack$year >= 1972] <- 'PAK'
rack$sftgcode[rack$country=="Pakistan" & rack$year < 1972] <- 'PKS'
rack$sftgcode[rack$country=="Papua New Guinea"] <- 'PNG'
rack$sftgcode[rack$country=="Paraguay"] <- 'PAR'
rack$sftgcode[rack$country=="Peru"] <- 'PER'
rack$sftgcode[rack$country=="Philippines"] <- 'PHI'
rack$sftgcode[rack$country=="Poland"] <- 'POL'
rack$sftgcode[rack$country=="Portugal"] <- 'POR'
rack$sftgcode[rack$country=="Qatar"] <- 'QAT'
rack$sftgcode[rack$country=="Romania"] <- 'RUM'
rack$sftgcode[rack$country=="Russia"] <- 'RUS'
rack$sftgcode[rack$country=="Russian Federation"] <- 'RUS'
rack$sftgcode[rack$country=="Soviet Union"] <- 'USS'
rack$sftgcode[rack$country=="USSR"] <- 'USS'
rack$sftgcode[rack$country=="USSR(Soviet Union)"] <- 'USS'
rack$sftgcode[rack$country=="U.S.S.R."] <- 'USS'
rack$sftgcode[rack$country=="Rwanda"] <- 'RWA'
rack$sftgcode[rack$country=="Sao Tome and Principe"] <- 'STP'
rack$sftgcode[rack$country=="Saudi Arabia"] <- 'SAU'
rack$sftgcode[rack$country=="Senegal"] <- 'SEN'
rack$sftgcode[rack$country=="Sierra Leone"] <- 'SIE'
rack$sftgcode[rack$country=="Singapore"] <- 'SIN'
rack$sftgcode[rack$country=="Slovakia"] <- 'SLO'
rack$sftgcode[rack$country=="Slovak Republic"] <- 'SLO'
rack$sftgcode[rack$country=="Slovenia"] <- 'SLV'
rack$sftgcode[rack$country=="Solomon Islands"] <- 'SOL'
rack$sftgcode[rack$country=="Somalia"] <- 'SOM'
rack$sftgcode[rack$country=="South Africa"] <- 'SAF'
rack$sftgcode[rack$country=="Spain"] <- 'SPN'
rack$sftgcode[rack$country=="Sri Lanka"] <- 'SRI'
rack$sftgcode[rack$country=="Sudan"] <- 'SUD'
rack$sftgcode[rack$country=="South Sudan"] <- 'SSD'
rack$sftgcode[rack$country=="Suriname"] <- 'SUR'
rack$sftgcode[rack$country=="Swaziland"] <- 'SWA'
rack$sftgcode[rack$country=="Sweden"] <- 'SWD'
rack$sftgcode[rack$country=="Switzerland"] <- 'SWZ'
rack$sftgcode[rack$country=="Syria"] <- 'SYR'
rack$sftgcode[rack$country=="Syrian Arab Republic"] <- 'SYR'
rack$sftgcode[rack$country=="Taiwan"] <- 'TAW'
rack$sftgcode[rack$country=="Taiwan Province of China"] <- 'TAW'
rack$sftgcode[rack$country=="Tajikistan"] <- 'TAJ'
rack$sftgcode[rack$country=="Tanzania"] <- 'TAZ'
rack$sftgcode[rack$country=="Thailand"] <- 'THI'
rack$sftgcode[rack$country=="Togo"] <- 'TOG'
rack$sftgcode[rack$country=="Trinidad"] <- 'TRI'
rack$sftgcode[rack$country=="Trinidad and Tobago"] <- 'TRI'
rack$sftgcode[rack$country=="Trinidad & Tobago"] <- 'TRI'
rack$sftgcode[rack$country=="Tunisia"] <- 'TUN'
rack$sftgcode[rack$country=="Turkey"] <- 'TUR'
rack$sftgcode[rack$country=="Turkmenistan"] <- 'TKM'
rack$sftgcode[rack$country=="Uganda"] <- 'UGA'
rack$sftgcode[rack$country=="Ukraine"] <- 'UKR'
rack$sftgcode[rack$country=="United Arab Emirates"] <- 'UAE'
rack$sftgcode[rack$country=="UAE"] <- 'UAE'
rack$sftgcode[rack$country=="United Kingdom"] <- 'UK '
rack$sftgcode[rack$country=="United States"] <- 'USA'
rack$sftgcode[rack$country=="United States of America"] <- 'USA'
rack$sftgcode[rack$country=="Uruguay"] <- 'URU'
rack$sftgcode[rack$country=="Uzbekistan"] <- 'UZB'
rack$sftgcode[rack$country=="Venezuela"] <- 'VEN'
rack$sftgcode[rack$country=="Venezuela, RB"] <- 'VEN'
rack$sftgcode[rack$country=="Vietnam"] <- 'VIE'
rack$sftgcode[rack$country=="Vietnam, Socialist Republic of"] <- 'VIE'
rack$sftgcode[rack$country=="North Vietnam"] <- 'DRV'
rack$sftgcode[rack$country=="Democratic Republic of Vietnam"] <- 'DRV'
rack$sftgcode[rack$country=="DRVN"] <- 'DRV'
rack$sftgcode[rack$country=="Vietnam, North"] <- 'DRV'
rack$sftgcode[rack$country=="Vietnam North"] <- 'DRV'
rack$sftgcode[rack$country=="Vietnam, N."] <- 'DRV'
rack$sftgcode[rack$country=="South Vietnam"] <- 'RVN'
rack$sftgcode[rack$country=="Republic of Vietnam"] <- 'RVN'
rack$sftgcode[rack$country=="Vietnam, South"] <- 'RVN'
rack$sftgcode[rack$country=="Vietnam South"] <- 'RVN'
rack$sftgcode[rack$country=="Vietnam, S."] <- 'RVN'
rack$sftgcode[rack$country=="Yemen"] <- 'YEM'
rack$sftgcode[rack$country=="Yemen, Rep."] <- 'YEM'
rack$sftgcode[rack$country=="Yemen Arab Republic" & rack$year>=1990] <- 'YEM'
rack$sftgcode[rack$country=="North Yemen"] <- 'YAR'
rack$sftgcode[rack$country=="Yemen, North"] <- 'YAR'
rack$sftgcode[rack$country=="Yemen North"] <- 'YAR'
rack$sftgcode[rack$country=="Yemen Arab Republic"] <- 'YAR'
rack$sftgcode[rack$country=="Yemen, N."] <- 'YAR'
rack$sftgcode[rack$country=="South Yemen"] <- 'YPR'
rack$sftgcode[rack$country=="Yemen, South"] <- 'YPR'
rack$sftgcode[rack$country=="Yemen South"] <- 'YPR'
rack$sftgcode[rack$country=="Yemen People's Republic"] <- 'YPR'
rack$sftgcode[rack$country=="SouthYemen"] <- 'YPR'
rack$sftgcode[rack$country=="Yemen, S."] <- 'YPR'
rack$sftgcode[rack$country=="Yugoslavia"] <- 'YGS'
rack$sftgcode[rack$country=="Yugoslavia" & rack$year >= 1992] <- 'YGS'
rack$sftgcode[rack$country=="Serbia and Montenegro" & rack$year >= 1992] <- 'YGS'
rack$sftgcode[rack$country=="Yugoslavia, Fed. Rep." & rack$year >= 1992] <- 'YGS'
rack$sftgcode[rack$country=="Federal Republic of Yugoslavia" & rack$year >= 1992] <- 'YGS'
rack$sftgcode[rack$country=="Yugoslavia (Serbia & Montenegro)"] <- 'YGS'
rack$sftgcode[rack$country=="Yugoslavia" & rack$year < 1992] <- 'YUG'
rack$sftgcode[rack$country=="Former Yugoslavia" & rack$year < 1992] <- 'YUG'
rack$sftgcode[rack$country=="Yugoslavia, Fed. Rep." & rack$year < 1992] <- 'YUG'
rack$sftgcode[rack$country=="Serbia"] <- 'SRB'
rack$sftgcode[rack$country=="Yugoslavia" & rack$year > 2005] <- 'SRB'
rack$sftgcode[rack$country=="Zambia"] <- 'ZAM'
rack$sftgcode[rack$country=="Zimbabwe"] <- 'ZIM'
rack$sftgcode[rack$country=="Yemen"] <- 'YEM'
rack$sftgcode[rack$country=="Kosovo"] <- 'KOS'
rack$sftgcode[rack$country=="Palestinian Authority"] <- 'PAL'

# COW sftgcode from PITF sftgcode
rack$ccode <- NA
rack$ccode[rack$sftgcode=="AFG"] <- 700
rack$ccode[rack$sftgcode=="ALB"] <- 339
rack$ccode[rack$sftgcode=="ALG"] <- 615
rack$ccode[rack$sftgcode=="ANG"] <- 540
rack$ccode[rack$sftgcode=="ARG"] <- 160
rack$ccode[rack$sftgcode=="ARM"] <- 371
rack$ccode[rack$sftgcode=="AUL"] <- 900
rack$ccode[rack$sftgcode=="AUS"] <- 305
rack$ccode[rack$sftgcode=="AZE"] <- 373
rack$ccode[rack$sftgcode=="BAH"] <- 692
rack$ccode[rack$sftgcode=="BAR"] <- 53
rack$ccode[rack$sftgcode=="BEL"] <- 211
rack$ccode[rack$sftgcode=="BEN"] <- 434
rack$ccode[rack$sftgcode=="BFO"] <- 439
rack$ccode[rack$sftgcode=="BHM"] <- 31
rack$ccode[rack$sftgcode=="BHU"] <- 760
rack$ccode[rack$sftgcode=="BLR"] <- 370
rack$ccode[rack$sftgcode=="BLZ"] <- 80
rack$ccode[rack$sftgcode=="BNG"] <- 771
rack$ccode[rack$sftgcode=="BOL"] <- 145
rack$ccode[rack$sftgcode=="BOS"] <- 346
rack$ccode[rack$sftgcode=="BOT"] <- 571
rack$ccode[rack$sftgcode=="BRA"] <- 140
rack$ccode[rack$sftgcode=="BUI"] <- 516
rack$ccode[rack$sftgcode=="BUL"] <- 355
rack$ccode[rack$sftgcode=="CAM"] <- 811
rack$ccode[rack$sftgcode=="CAN"] <- 20
rack$ccode[rack$sftgcode=="CAO"] <- 471
rack$ccode[rack$sftgcode=="CAP"] <- 402
rack$ccode[rack$sftgcode=="CEN"] <- 482
rack$ccode[rack$sftgcode=="CHA"] <- 483
rack$ccode[rack$sftgcode=="CHL"] <- 155
rack$ccode[rack$sftgcode=="CHN"] <- 710
rack$ccode[rack$sftgcode=="COL"] <- 100
rack$ccode[rack$sftgcode=="COM"] <- 581
rack$ccode[rack$sftgcode=="CON"] <- 484
rack$ccode[rack$sftgcode=="COS"] <- 94
rack$ccode[rack$sftgcode=="CRO"] <- 344
rack$ccode[rack$sftgcode=="CUB"] <- 40
rack$ccode[rack$sftgcode=="CYP"] <- 352
rack$ccode[rack$sftgcode=="CZE"] <- 315
rack$ccode[rack$sftgcode=="CZR"] <- 316
rack$ccode[rack$sftgcode=="DEN"] <- 390
rack$ccode[rack$sftgcode=="DJI"] <- 522
rack$ccode[rack$sftgcode=="DOM"] <- 42
rack$ccode[rack$sftgcode=="ZAI"] <- 490
rack$ccode[rack$sftgcode=="DRV" & rack$year<=1975] <- 816
rack$ccode[rack$sftgcode=="VIE" & rack$year>=1976] <- 816
rack$ccode[rack$sftgcode=="ECU"] <- 130
rack$ccode[rack$sftgcode=="EGY"] <- 651
rack$ccode[rack$sftgcode=="EQG"] <- 411
rack$ccode[rack$sftgcode=="ERI"] <- 531
rack$ccode[rack$sftgcode=="EST"] <- 366
rack$ccode[rack$sftgcode=="ETH" & rack$year<=1992] <- 530
rack$ccode[rack$sftgcode=="ETI" & rack$year>=1993] <- 530
rack$ccode[rack$sftgcode=="ETM"] <- 860
rack$ccode[rack$sftgcode=="FIN"] <- 375
rack$ccode[rack$sftgcode=="FJI"] <- 950
rack$ccode[rack$sftgcode=="FRN"] <- 220
rack$ccode[rack$sftgcode=="GAB"] <- 481
rack$ccode[rack$sftgcode=="GAM"] <- 420
rack$ccode[rack$sftgcode=="GDR"] <- 265
rack$ccode[rack$sftgcode=="GFR" & rack$year<=1989] <- 260
rack$ccode[rack$sftgcode=="GER" & rack$year>=1990] <- 260
rack$ccode[rack$sftgcode=="GHA"] <- 452
rack$ccode[rack$sftgcode=="GNB"] <- 404
rack$ccode[rack$sftgcode=="GRC"] <- 350
rack$ccode[rack$sftgcode=="GUA"] <- 90
rack$ccode[rack$sftgcode=="GUI"] <- 438
rack$ccode[rack$sftgcode=="GUY"] <- 110
rack$ccode[rack$sftgcode=="HAI"] <- 41
rack$ccode[rack$sftgcode=="HON"] <- 91
rack$ccode[rack$sftgcode=="HUN"] <- 310
rack$ccode[rack$sftgcode=="ICE"] <- 395
rack$ccode[rack$sftgcode=="IND"] <- 750
rack$ccode[rack$sftgcode=="INS"] <- 850
rack$ccode[rack$sftgcode=="IRE"] <- 205
rack$ccode[rack$sftgcode=="IRN"] <- 630
rack$ccode[rack$sftgcode=="IRQ"] <- 645
rack$ccode[rack$sftgcode=="ISR"] <- 666
rack$ccode[rack$sftgcode=="ITA"] <- 325
rack$ccode[rack$sftgcode=="JAM"] <- 51
rack$ccode[rack$sftgcode=="JOR"] <- 663
rack$ccode[rack$sftgcode=="JPN"] <- 740
rack$ccode[rack$sftgcode=="KEN"] <- 501
rack$ccode[rack$sftgcode=="KUW"] <- 690
rack$ccode[rack$sftgcode=="KYR"] <- 703
rack$ccode[rack$sftgcode=="KZK"] <- 705
rack$ccode[rack$sftgcode=="LAO"] <- 812
rack$ccode[rack$sftgcode=="LAT"] <- 367
rack$ccode[rack$sftgcode=="LBR"] <- 450
rack$ccode[rack$sftgcode=="LEB"] <- 660
rack$ccode[rack$sftgcode=="LES"] <- 570
rack$ccode[rack$sftgcode=="LIB"] <- 620
rack$ccode[rack$sftgcode=="LIT"] <- 368
rack$ccode[rack$sftgcode=="MAA"] <- 435
rack$ccode[rack$sftgcode=="MAC"] <- 343
rack$ccode[rack$sftgcode=="MAG"] <- 580
rack$ccode[rack$sftgcode=="MAL"] <- 820
rack$ccode[rack$sftgcode=="MAS"] <- 590
rack$ccode[rack$sftgcode=="MAW"] <- 553
rack$ccode[rack$sftgcode=="MEX"] <- 70
rack$ccode[rack$sftgcode=="MLD"] <- 359
rack$ccode[rack$sftgcode=="MLI"] <- 432
rack$ccode[rack$sftgcode=="MON"] <- 712
rack$ccode[rack$sftgcode=="MOR"] <- 600
rack$ccode[rack$sftgcode=="MYA"] <- 775
rack$ccode[rack$sftgcode=="MZM"] <- 541
rack$ccode[rack$sftgcode=="NAM"] <- 565
rack$ccode[rack$sftgcode=="NEP"] <- 790
rack$ccode[rack$sftgcode=="NEW"] <- 920
rack$ccode[rack$sftgcode=="NIC"] <- 93
rack$ccode[rack$sftgcode=="NIG"] <- 475
rack$ccode[rack$sftgcode=="NIR"] <- 436
rack$ccode[rack$sftgcode=="NOR"] <- 385
rack$ccode[rack$sftgcode=="NTH"] <- 210
rack$ccode[rack$sftgcode=="OMA"] <- 698
rack$ccode[rack$sftgcode=="PKS" & rack$year<=1971] <- 770
rack$ccode[rack$sftgcode=="PAK" & rack$year>=1972] <- 770
rack$ccode[rack$sftgcode=="PAN"] <- 95
rack$ccode[rack$sftgcode=="PAR"] <- 150
rack$ccode[rack$sftgcode=="PER"] <- 135
rack$ccode[rack$sftgcode=="PHI"] <- 840
rack$ccode[rack$sftgcode=="PNG"] <- 910
rack$ccode[rack$sftgcode=="POL"] <- 290
rack$ccode[rack$sftgcode=="POR"] <- 235
rack$ccode[rack$sftgcode=="QAT"] <- 694
rack$ccode[rack$sftgcode=="ROK"] <- 732
rack$ccode[rack$sftgcode=="RUM"] <- 360
rack$ccode[rack$sftgcode=="USS" & rack$year<=1991] <- 365
rack$ccode[rack$sftgcode=="RUS" & rack$year>=1992] <- 365
rack$ccode[rack$sftgcode=="RVN"] <- 817
rack$ccode[rack$sftgcode=="RWA"] <- 517
rack$ccode[rack$sftgcode=="SAF"] <- 560
rack$ccode[rack$sftgcode=="SAL"] <- 92
rack$ccode[rack$sftgcode=="SAU"] <- 670
rack$ccode[rack$sftgcode=="SEN"] <- 433
rack$ccode[rack$sftgcode=="SIE"] <- 451
rack$ccode[rack$sftgcode=="SIN"] <- 830
rack$ccode[rack$sftgcode=="SLO"] <- 317
rack$ccode[rack$sftgcode=="SLV"] <- 349
rack$ccode[rack$sftgcode=="SOL"] <- 940
rack$ccode[rack$sftgcode=="SOM"] <- 520
rack$ccode[rack$sftgcode=="SPN"] <- 230
rack$ccode[rack$sftgcode=="SRI"] <- 780
rack$ccode[rack$sftgcode=="SUD"] <- 625
rack$ccode[rack$sftgcode=="SUR"] <- 115
rack$ccode[rack$sftgcode=="SWA"] <- 572
rack$ccode[rack$sftgcode=="SWD"] <- 380
rack$ccode[rack$sftgcode=="SWZ"] <- 225
rack$ccode[rack$sftgcode=="SYR"] <- 652
rack$ccode[rack$sftgcode=="TAJ"] <- 702
rack$ccode[rack$sftgcode=="TAW"] <- 713
rack$ccode[rack$sftgcode=="TAZ"] <- 510
rack$ccode[rack$sftgcode=="THI"] <- 800
rack$ccode[rack$sftgcode=="TKM"] <- 701
rack$ccode[rack$sftgcode=="TOG"] <- 461
rack$ccode[rack$sftgcode=="TRI"] <- 52
rack$ccode[rack$sftgcode=="TUN"] <- 616
rack$ccode[rack$sftgcode=="TUR"] <- 640
rack$ccode[rack$sftgcode=="UAE"] <- 696
rack$ccode[rack$sftgcode=="UGA"] <- 500
rack$ccode[rack$sftgcode=="UK"] <- 200
rack$ccode[rack$sftgcode=="UKR"] <- 369
rack$ccode[rack$sftgcode=="URU"] <- 165
rack$ccode[rack$sftgcode=="USA"] <- 2
rack$ccode[rack$sftgcode=="UZB"] <- 704
rack$ccode[rack$sftgcode=="VEN"] <- 101
rack$ccode[rack$sftgcode=="YEM" & rack$year>=1991] <- 678
rack$ccode[rack$sftgcode=="YAR" & rack$year<=1990] <- 678
rack$ccode[rack$sftgcode=="YPR"] <- 680
rack$ccode[rack$sftgcode=="YUG" & rack$year<=1991] <- 345
rack$ccode[rack$sftgcode=="YGS" & rack$year>=1992 & rack$year<=2006] <- 345
rack$ccode[rack$sftgcode=="SRB" & rack$year>=2006] <- 345
rack$ccode[rack$sftgcode=="ZAM"] <- 551
rack$ccode[rack$sftgcode=="ZIM"] <- 552
rack$ccode[rack$sftgcode=="MNG"] <- 341
rack$ccode[rack$sftgcode=="KOS"] <- 347
rack$ccode[rack$sftgcode=="IVO"] <- 437
rack$ccode[rack$sftgcode=="PRK"] <- 731
rack$ccode[rack$sftgcode=="GRG"] <- 372


# DEPT OF STATE REGIONS

rack$reg.eap <- ifelse(rack$country=="Australia" | rack$country=="Brunei" | rack$country=="Burma" | rack$country=="Cambodia" |
                       rack$country=="China" | rack$country=="East Timor" | rack$country=="Fiji" | rack$country=="Indonesia" |
                       rack$country=="Japan" | rack$country=="Kiribati" | rack$country=="Laos" | rack$country=="Malaysia" |
                       rack$country=="Marshall Islands" | rack$country=="Micronesia" | rack$country=="Mongolia" | rack$country=="Nauru" |
                       rack$country=="New Zealand" | rack$country=="North Korea" | rack$country=="Palau" | rack$country=="Papua New Guinea" |
                       rack$country=="Philippines" | rack$country=="Samoa" | rack$country=="Singapore" | rack$country=="Solomon Islands" |
                       rack$country=="South Korea" | rack$country=="Taiwan" | rack$country=="Thailand" | rack$country=="Tonga" |
                       rack$country=="Tuvalu" | rack$country=="Vanuatu" | rack$country=="Vietnam" |
                       rack$country=="South Vietnam" | rack$country=="North Vietnam" | rack$country=="Myanmar" | rack$country=="Timor Leste",
                       1, 0)

rack$reg.afr <- ifelse(rack$country=="Angola" | rack$country=="Benin" | rack$country=="Botswana" | rack$country=="Burkina Faso" |
                       rack$country=="Burundi" | rack$country=="Cameroon" | rack$country=="Cape Verde" | rack$country=="Central African Republic" |
                       rack$country=="Chad" | rack$country=="Comoros" | rack$country=="Congo-Kinshasa" | rack$country=="Congo-Brazzaville" |
                       rack$country=="Ivory Coast" | rack$country=="Djibouti" | rack$country=="Equatorial Guinea" | rack$country=="Eritrea" |
                       rack$country=="Ethiopia" | rack$country=="Gabon" | rack$country=="Gambia" | rack$country=="Ghana" |
                       rack$country=="Guinea" | rack$country=="Guinea-Bissau" | rack$country=="Kenya" | rack$country=="Lesotho" |
                       rack$country=="Liberia" | rack$country=="Madagascar" | rack$country=="Malawi" | rack$country=="Mali" |
                       rack$country=="Mauritania" | rack$country=="Mauritius" | rack$country=="Mozambique" | rack$country=="Namibia" |
                       rack$country=="Niger" | rack$country=="Nigeria" | rack$country=="Rwanda" | rack$country=="Sao Tome and Principe" |
                       rack$country=="Senegal" | rack$country=="Seychelles" | rack$country=="Sierra Leone" | rack$country=="Somalia" |
                       rack$country=="South Africa" | rack$country=="Sudan" | rack$country=="South Sudan" | rack$country=="Swaziland" |
                       rack$country=="Tanzania" | rack$country=="Togo" | rack$country=="Uganda" | rack$country=="Zambia" |
                       rack$country=="Zimbabwe",
                       1, 0)

rack$reg.eur <- ifelse(rack$country=="Albania" | rack$country=="Armenia" | rack$country=="Austria" | rack$country=="Azerbaijan" |
                       rack$country=="Belarus" | rack$country=="Belgium" | rack$country=="Bosnia and Herzegovina" | rack$country=="Bulgaria" |
                       rack$country=="Croatia" | rack$country=="Czech Republic" | rack$country=="Cyprus" | rack$country=="Denmark" |
                       rack$country=="Estonia" | rack$country=="Finland" | rack$country=="France" | rack$country=="Georgia" |
                       rack$country=="Germany" | rack$country=="Greece" | rack$country=="Hungary" | rack$country=="Iceland" |
                       rack$country=="Ireland" | rack$country=="Italy" | rack$country=="Kosovo" | rack$country=="Latvia" |
                       rack$country=="Lithuania" | rack$country=="Liechtenstein" | rack$country=="Macedonia" | rack$country=="Malta" |
                       rack$country=="Moldova" | rack$country=="Monaco" | rack$country=="Montenegro" | rack$country=="Netherlands" |
                       rack$country=="Norway" | rack$country=="Poland" | rack$country=="Portugal" | rack$country=="Romania" |
                       rack$country=="Russia" | rack$country=="San Marino" | rack$country=="Serbia" | rack$country=="Slovakia" |
                       rack$country=="Slovenia" | rack$country=="Spain" | rack$country=="Sweden" | rack$country=="Switzerland" |
                       rack$country=="Turkey" | rack$country=="Ukraine" | rack$country=="United Kingdom" |
                       rack$country=="East Germany" | rack$country=="West Germany" | rack$country=="Soviet Union" | rack$country=="Yugoslavia" |
                       rack$country=="Federal Republic of Yugoslavia" | rack$country=="Serbia and Montenegro" | rack$country=="Czechoslovakia",
                       1, 0)

rack$reg.mna <- ifelse(rack$country=="Algeria" | rack$country=="Bahrain" | rack$country=="Egypt" | rack$country=="Iran" |
                       rack$country=="Iraq" | rack$country=="Israel" | rack$country=="Jordan" | rack$country=="Kuwait" |
                       rack$country=="Lebanon" | rack$country=="Libya" | rack$country=="Morocco" | rack$country=="Oman" |
                       rack$country=="Palestinian Territories" | rack$country=="Qatar" | rack$country=="Saudi Arabia" | rack$country=="Syria" |
                       rack$country=="Tunisia" | rack$country=="United Arab Emirates" | rack$country=="Yemen" |
                       rack$country=="North Yemen" | rack$country=="South Yemen",
                       1, 0)

rack$reg.sca <- ifelse(rack$country=="Afghanistan" | rack$country=="Bangladesh" | rack$country=="Bhutan" | rack$country=="India" |
                       rack$country=="Kazakhstan" | rack$country=="Kyrgyzstan" | rack$country=="Maldives" | rack$country=="Nepal" |
                       rack$country=="Pakistan" | rack$country=="Sri Lanka" | rack$country=="Tajikistan" | rack$country=="Turkmenistan" |
                       rack$country=="Uzbekistan",
                       1, 0)

rack$reg.amr <- ifelse(rack$country=="Antigua and Barbuda" | rack$country=="Argentina" | rack$country=="Bahamas" | rack$country=="Barbados" |
                       rack$country=="Belize" | rack$country=="Bolivia" | rack$country=="Brazil" | rack$country=="Canada" |
                       rack$country=="Cayman Islands" | rack$country=="Chile" | rack$country=="Colombia" | rack$country=="Costa Rica" |
                       rack$country=="Cuba" | rack$country=="Dominica" | rack$country=="Dominican Republic" | rack$country=="Ecuador" |
                       rack$country=="El Salvador" | rack$country=="Grenada" | rack$country=="Guatemala" | rack$country=="Guyana" |
                       rack$country=="Haiti" | rack$country=="Honduras" | rack$country=="Jamaica" | rack$country=="Mexico" |
                       rack$country=="Nicaragua" | rack$country=="Panama" | rack$country=="Paraguay" | rack$country=="Peru" |
                       rack$country=="Saint Kitts and Nevis" | rack$country=="Saint Lucia" | rack$country=="Saint Vincent and the Grenadines" |
                       rack$country=="Suriname" | rack$country=="Trinidad and Tobago" | rack$country=="United States" | rack$country=="Uruguay" |
                       rack$country=="Venezuela",
                       1, 0)

rack$dosreg <- NA
rack$dosreg[rack$reg.afr==1] <- "Africa"
rack$dosreg[rack$reg.eap==1] <- "East Asia & Pacific"
rack$dosreg[rack$reg.eur==1] <- "Europe & Eurasia"
rack$dosreg[rack$reg.mna==1] <- "Middle East & North Africa"
rack$dosreg[rack$reg.sca==1] <- "South & Central Asia"
rack$dosreg[rack$reg.amr==1] <- "Americas"

# COUNTRY AGE AND POST-COLD WAR DUMMY
# Country age
rack$age <- rack$year - rack$yrborn
rack$ageln <- log1p(rack$age)

# Create post-Cold War indicator
rack$postcw <- ifelse(rack$year>=1991, 1, 0)


# ISO3 CODE
rack$iso3code <- NA
rack$iso3code[rack$country=="Afghanistan"] <- 'AFG'
rack$iso3code[rack$country=="Albania"] <- 'ALB'
rack$iso3code[rack$country=="Algeria"] <- 'DZA'
rack$iso3code[rack$country=="Angola"] <- 'AGO'
rack$iso3code[rack$country=="Argentina"] <- 'ARG'
rack$iso3code[rack$country=="Armenia"] <- 'ARM'
rack$iso3code[rack$country=="Australia"] <- 'AUS'
rack$iso3code[rack$country=="Austria"] <- 'AUT'
rack$iso3code[rack$country=="Azerbaijan"] <- 'AZE'
rack$iso3code[rack$country=="Bahamas"] <- 'BHS'
rack$iso3code[rack$country=="Bahamas, The"] <- 'BHS'
rack$iso3code[rack$country=="The Bahamas"] <- 'BHS'
rack$iso3code[rack$country=="Bahrain"] <- 'BHR'
rack$iso3code[rack$country=="Bangladesh"] <- 'BGD'
rack$iso3code[rack$country=="Barbados"] <- 'BRB'
rack$iso3code[rack$country=="Belarus"] <- 'BLR'
rack$iso3code[rack$country=="Belgium"] <- 'BEL'
rack$iso3code[rack$country=="Belize"] <- 'BLZ'
rack$iso3code[rack$country=="Benin"] <- 'BEN'
rack$iso3code[rack$country=="Bhutan"] <- 'BTN'
rack$iso3code[rack$country=="Bolivia"] <- 'BOL'
rack$iso3code[rack$country=="Bosnia"] <- 'BIH'
rack$iso3code[rack$country=="Bosnia and Herzegovina"] <- 'BIH'
rack$iso3code[rack$country=="Bosnia-Herzegovina"] <- 'BIH'
rack$iso3code[rack$country=="Botswana"] <- 'BWA'
rack$iso3code[rack$country=="Brazil"] <- 'BRA'
rack$iso3code[rack$country=="Brunei"] <- 'BRN'
rack$iso3code[rack$country=="Bulgaria"] <- 'BGR'
rack$iso3code[rack$country=="Burkina Faso"] <- 'BFA'
rack$iso3code[rack$country=="Burma"] <- 'MMR'
rack$iso3code[rack$country=="Myanmar"] <- 'MMR'
rack$iso3code[rack$country=="Myanmar (Burma)"] <- 'MMR'
rack$iso3code[rack$country=="Burundi"] <- 'BDI'
rack$iso3code[rack$country=="Bangladesh"] <- 'BGD'
rack$iso3code[rack$country=="Cambodia"] <- 'KHM'
rack$iso3code[rack$country=="Cape Verde"] <- 'CPV'
rack$iso3code[rack$country=="Cameroon"] <- 'CMR'
rack$iso3code[rack$country=="Canada"] <- 'CAN'
rack$iso3code[rack$country=="Central African Republic"] <- 'CAF'
rack$iso3code[rack$country=="Chad"] <- 'TCD'
rack$iso3code[rack$country=="Chile"] <- 'CHL'
rack$iso3code[rack$country=="China"] <- 'CHN'
rack$iso3code[rack$country=="Colombia"] <- 'COL'
rack$iso3code[rack$country=="Comoros"] <- 'COM'
rack$iso3code[rack$country=="Congo-Brazzaville"] <- 'COG'
rack$iso3code[rack$country=="Republic of Congo"] <- 'COG'
rack$iso3code[rack$country=="Congo, Republic of"] <- 'COG'
rack$iso3code[rack$country=="Congo (Brazzaville)"] <- 'COG'
rack$iso3code[rack$country=="Republic of the Congo"] <- 'COG'
rack$iso3code[rack$country=="Congo Brazzaville"] <- 'COG'
rack$iso3code[rack$country=="Congo"] <- 'COG'
rack$iso3code[rack$country=="Congo, Rep."] <- 'COG'
rack$iso3code[rack$country=="Congo-Kinshasa"] <- 'COD'
rack$iso3code[rack$country=="Zaire"] <- 'COD'
rack$iso3code[rack$country=="Zaire/DRC"] <- 'COD'
rack$iso3code[rack$country=="Democratic Republic of Congo"] <- 'COD'
rack$iso3code[rack$country=="Democratic Republic of the Congo"] <- 'COD'
rack$iso3code[rack$country=="Congo, Democratic Republic of"] <- 'COD'
rack$iso3code[rack$country=="Congo, Dem. Rep."] <- 'COD'
rack$iso3code[rack$country=="Congo (Kinshasa)"] <- 'COD'
rack$iso3code[rack$country=="Congo Kinshasa"] <- 'COD'
rack$iso3code[rack$country=="DROC"] <- 'COD'
rack$iso3code[rack$country=="Costa Rica"] <- 'CRI'
rack$iso3code[rack$country=="Cote d'Ivoire"] <- 'CIV'
rack$iso3code[rack$country=="Ivory Coast"] <- 'CIV'
rack$iso3code[rack$country=="Cote d Ivoire"] <- 'CIV'
rack$iso3code[rack$country=="Ivory Coast (Cote d'Ivoire)"] <- 'CIV'
rack$iso3code[rack$country=="Côte d'Ivoire"] <- 'CIV'
rack$iso3code[rack$country=="Croatia"] <- 'HRV'
rack$iso3code[rack$country=="Cuba"] <- 'CUB'
rack$iso3code[rack$country=="Cyprus"] <- 'CYP'
rack$iso3code[rack$country=="Czech Republic"] <- 'CZE'
rack$iso3code[rack$country=="Czechoslovakia"] <- NA
rack$iso3code[rack$country=="Denmark"] <- 'DNK'
rack$iso3code[rack$country=="Djibouti"] <- 'DJI'
rack$iso3code[rack$country=="Dominican Republic"] <- 'DOM'
rack$iso3code[rack$country=="Dominican Rep"] <- 'DOM'
rack$iso3code[rack$country=="East Timor"] <- 'TLS'
rack$iso3code[rack$country=="Timor"] <- 'TLS'
rack$iso3code[rack$country=="Timor Leste"] <- 'TLS'
rack$iso3code[rack$country=="East Timor (Timor L'este)"] <- 'TLS'
rack$iso3code[rack$country=="Timor-Leste"] <- 'TLS'
rack$iso3code[rack$country=="Ecuador"] <- 'ECU'
rack$iso3code[rack$country=="Egypt"] <- 'EGY'
rack$iso3code[rack$country=="Egypt, Arab Rep."] <- 'EGY'
rack$iso3code[rack$country=="El Salvador"] <- 'SLV'
rack$iso3code[rack$country=="Equatorial Guinea"] <- 'GNQ'
rack$iso3code[rack$country=="Eritrea"] <- 'ERI'
rack$iso3code[rack$country=="Estonia"] <- 'EST'
rack$iso3code[rack$country=="Ethiopia"] <- 'ETH'
rack$iso3code[rack$country=="Fiji"] <- 'FJI'
rack$iso3code[rack$country=="Finland"] <- 'FIN'
rack$iso3code[rack$country=="Finland "] <- 'FIN'
rack$iso3code[rack$country=="France"] <- 'FRA'
rack$iso3code[rack$country=="Gabon"] <- 'GAB'
rack$iso3code[rack$country=="Gambia"] <- 'GMB'
rack$iso3code[rack$country=="The Gambia"] <- 'GMB'
rack$iso3code[rack$country=="Gambia, the"] <- 'GMB'
rack$iso3code[rack$country=="Gambia, The"] <- 'GMB'
rack$iso3code[rack$country=="Georgia"] <- 'GEO'
rack$iso3code[rack$country=="Germany"] <- 'DEU'
rack$iso3code[rack$country=="East Germany"] <- NA
rack$iso3code[rack$country=="Germany, East"] <- NA
rack$iso3code[rack$country=="German Democratic Republic"] <- NA
rack$iso3code[rack$country=="Germany East"] <- NA
rack$iso3code[rack$country=="Germany, E. "] <- NA
rack$iso3code[rack$country=="West Germany"] <- 'DEU'
rack$iso3code[rack$country=="Germany, West"] <- 'DEU'
rack$iso3code[rack$country=="Federal Republic of Germany"] <- 'DEU'
rack$iso3code[rack$country=="German Federal Republic"] <- 'DEU'
rack$iso3code[rack$country=="Germany West"] <- 'DEU'
rack$iso3code[rack$country=="Germany, W. "] <- 'DEU'
rack$iso3code[rack$country=="Germany" & rack$year <= 1989] <- 'DEU'
rack$iso3code[rack$country=="Ghana"] <- 'GHA'
rack$iso3code[rack$country=="Greece"] <- 'GRC'
rack$iso3code[rack$country=="Guatemala"] <- 'GTM'
rack$iso3code[rack$country=="Guinea"] <- 'GIN'
rack$iso3code[rack$country=="Guinea-Bissau"] <- 'GNB'
rack$iso3code[rack$country=="Guinea Bissau"] <- 'GNB'
rack$iso3code[rack$country=="Guyana"] <- 'GUY'
rack$iso3code[rack$country=="Haiti"] <- 'HTI'
rack$iso3code[rack$country=="Honduras"] <- 'HND'
rack$iso3code[rack$country=="Hungary"] <- 'HUN'
rack$iso3code[rack$country=="Iceland"] <- 'ISL'
rack$iso3code[rack$country=="India"] <- 'IND'
rack$iso3code[rack$country=="Indonesia"] <- 'IDN'
rack$iso3code[rack$country=="Iran"] <- 'IRN'
rack$iso3code[rack$country=="Iran, Islamic Rep."] <- 'IRN'
rack$iso3code[rack$country=="Islamic Republic of Iran"] <- 'IRN'
rack$iso3code[rack$country=="Iraq"] <- 'IRQ'
rack$iso3code[rack$country=="Ireland"] <- 'IRL'
rack$iso3code[rack$country=="Israel"] <- 'ISR'
rack$iso3code[rack$country=="Israel and Occupied Territories**"] <- 'ISR'
rack$iso3code[rack$country=="Israel, pre-1967 borders"] <- 'ISR'
rack$iso3code[rack$country=="Italy"] <- 'ITA'
rack$iso3code[rack$country=="Jamaica"] <- 'JAM'
rack$iso3code[rack$country=="Japan"] <- 'JPN'
rack$iso3code[rack$country=="Jordan"] <- 'JOR'
rack$iso3code[rack$country=="Kazakhstan"] <- 'KAZ'
rack$iso3code[rack$country=="Kenya"] <- 'KEN'
rack$iso3code[rack$country=="Korea, North"] <- 'PRK'
rack$iso3code[rack$country=="North Korea"] <- 'PRK'
rack$iso3code[rack$country=="People's Republic of Korea"] <- 'PRK'
rack$iso3code[rack$country=="Korea North"] <- 'PRK'
rack$iso3code[rack$country=="Korea, Dem. Rep."] <- 'PRK'
rack$iso3code[rack$country=="North Korea (Democratc People's Republic of Korea)"] <- 'PRK'
rack$iso3code[rack$country=="Korea, South"] <- 'KOR'
rack$iso3code[rack$country=="South Korea"] <- 'KOR'
rack$iso3code[rack$country=="Korea South"] <- 'KOR'
rack$iso3code[rack$country=="Republic of Korea"] <- 'KOR'
rack$iso3code[rack$country=="Korea, Rep."] <- 'KOR'
rack$iso3code[rack$country=="South Korea (Republic of Korea)"] <- 'KOR'
rack$iso3code[rack$country=="Korea"] <- 'KOR'
rack$iso3code[rack$country=="Kuwait"] <- 'KWT'
rack$iso3code[rack$country=="Kyrgyzstan"] <- 'KGZ'
rack$iso3code[rack$country=="Kyrgyz Republic"] <- 'KGZ'
rack$iso3code[rack$country=="Laos"] <- 'LAO'
rack$iso3code[rack$country=="Lao PDR"] <- 'LAO'
rack$iso3code[rack$country=="Lao P.D.R."] <- 'LAO'
rack$iso3code[rack$country=="Latvia"] <- 'LVA'
rack$iso3code[rack$country=="Lebanon"] <- 'LBN'
rack$iso3code[rack$country=="Lesotho"] <- 'LSO'
rack$iso3code[rack$country=="Liberia"] <- 'LBR'
rack$iso3code[rack$country=="Libya"] <- 'LBY'
rack$iso3code[rack$country=="Lithuania"] <- 'LTU'
rack$iso3code[rack$country=="Luxembourg"] <- 'LUX'
rack$iso3code[rack$country=="Macedonia"] <- 'MKD'
rack$iso3code[rack$country=="Macedonia, FYR"] <- 'MKD'
rack$iso3code[rack$country=="FYR Macedonia"] <- 'MKD'
rack$iso3code[rack$country=="Madagascar"] <- 'MDG'
rack$iso3code[rack$country=="Malawi"] <- 'MWI'
rack$iso3code[rack$country=="Malaysia"] <- 'MYS'
rack$iso3code[rack$country=="Maldives"] <- 'MDV'
rack$iso3code[rack$country=="Mali"] <- 'MLI'
rack$iso3code[rack$country=="Malta"] <- 'MLT'
rack$iso3code[rack$country=="Mauritania"] <- 'MRT'
rack$iso3code[rack$country=="Mauritius"] <- 'MUS'
rack$iso3code[rack$country=="Mexico"] <- 'MEX'
rack$iso3code[rack$country=="Moldova"] <- 'MDA'
rack$iso3code[rack$country=="Mongolia"] <- 'MNG'
rack$iso3code[rack$country=="Montenegro"] <- 'MNE'
rack$iso3code[rack$country=="Morocco"] <- 'MAR'
rack$iso3code[rack$country=="Mozambique"] <- 'MOZ'
rack$iso3code[rack$country=="Namibia"] <- 'NAM'
rack$iso3code[rack$country=="Nepal"] <- 'NPL'
rack$iso3code[rack$country=="Netherlands"] <- 'NLD'
rack$iso3code[rack$country=="New Zealand"] <- 'NZL'
rack$iso3code[rack$country=="Nicaragua"] <- 'NIC'
rack$iso3code[rack$country=="Nigeria"] <- 'NGA'
rack$iso3code[rack$country=="Niger"] <- 'NER'
rack$iso3code[rack$country=="Norway"] <- 'NOR'
rack$iso3code[rack$country=="Oman"] <- 'OMN'
rack$iso3code[rack$country=="Panama"] <- 'PAN'
rack$iso3code[rack$country=="Pakistan"] <- 'PAK'
rack$iso3code[rack$country=="Papua New Guinea"] <- 'PNG'
rack$iso3code[rack$country=="Paraguay"] <- 'PRY'
rack$iso3code[rack$country=="Peru"] <- 'PER'
rack$iso3code[rack$country=="Philippines"] <- 'PHL'
rack$iso3code[rack$country=="Poland"] <- 'POL'
rack$iso3code[rack$country=="Portugal"] <- 'PRT'
rack$iso3code[rack$country=="Qatar"] <- 'QAT'
rack$iso3code[rack$country=="Romania"] <- 'ROU'
rack$iso3code[rack$country=="Russia"] <- 'RUS'
rack$iso3code[rack$country=="Russian Federation"] <- 'RUS'
rack$iso3code[rack$country=="Soviet Union"] <- NA
rack$iso3code[rack$country=="USSR"] <- NA
rack$iso3code[rack$country=="USSR(Soviet Union)"] <- NA
rack$iso3code[rack$country=="U.S.S.R."] <- NA
rack$iso3code[rack$country=="Rwanda"] <- 'RWA'
rack$iso3code[rack$country=="Sao Tome and Principe"] <- 'STP'
rack$iso3code[rack$country=="Saudi Arabia"] <- 'SAU'
rack$iso3code[rack$country=="Senegal"] <- 'SEN'
rack$iso3code[rack$country=="Sierra Leone"] <- 'SLE'
rack$iso3code[rack$country=="Singapore"] <- 'SGP'
rack$iso3code[rack$country=="Slovakia"] <- 'SVK'
rack$iso3code[rack$country=="Slovak Republic"] <- 'SVK'
rack$iso3code[rack$country=="Slovenia"] <- 'SVN'
rack$iso3code[rack$country=="Solomon Islands"] <- 'SLB'
rack$iso3code[rack$country=="Somalia"] <- 'SOM'
rack$iso3code[rack$country=="South Africa"] <- 'ZAF'
rack$iso3code[rack$country=="Spain"] <- 'ESP'
rack$iso3code[rack$country=="Sri Lanka"] <- 'LKA'
rack$iso3code[rack$country=="Sudan"] <- 'SDN'
rack$iso3code[rack$country=="South Sudan"] <- 'SSD'
rack$iso3code[rack$country=="Suriname"] <- 'SUR'
rack$iso3code[rack$country=="Swaziland"] <- 'SWZ'
rack$iso3code[rack$country=="Sweden"] <- 'SWE'
rack$iso3code[rack$country=="Switzerland"] <- 'CHE'
rack$iso3code[rack$country=="Syria"] <- 'SYR'
rack$iso3code[rack$country=="Syrian Arab Republic"] <- 'SYR'
rack$iso3code[rack$country=="Taiwan"] <- 'TWN'
rack$iso3code[rack$country=="Taiwan Province of China"] <- 'TWN'
rack$iso3code[rack$country=="Tajikistan"] <- 'TJK'
rack$iso3code[rack$country=="Tanzania"] <- 'TZA'
rack$iso3code[rack$country=="Thailand"] <- 'THA'
rack$iso3code[rack$country=="Togo"] <- 'TGO'
rack$iso3code[rack$country=="Trinidad"] <- 'TTO'
rack$iso3code[rack$country=="Trinidad and Tobago"] <- 'TTO'
rack$iso3code[rack$country=="Trinidad & Tobago"] <- 'TTO'
rack$iso3code[rack$country=="Tunisia"] <- 'TUN'
rack$iso3code[rack$country=="Turkey"] <- 'TUR'
rack$iso3code[rack$country=="Turkmenistan"] <- 'TKM'
rack$iso3code[rack$country=="Uganda"] <- 'UGA'
rack$iso3code[rack$country=="Ukraine"] <- 'UKR'
rack$iso3code[rack$country=="United Arab Emirates"] <- 'ARE'
rack$iso3code[rack$country=="UAE"] <- 'ARE'
rack$iso3code[rack$country=="United Kingdom"] <- 'GBR'
rack$iso3code[rack$country=="United States"] <- 'USA'
rack$iso3code[rack$country=="United States of America"] <- 'USA'
rack$iso3code[rack$country=="Uruguay"] <- 'URY'
rack$iso3code[rack$country=="Uzbekistan"] <- 'UZB'
rack$iso3code[rack$country=="Venezuela"] <- 'VEN'
rack$iso3code[rack$country=="Venezuela, RB"] <- 'VEN'
rack$iso3code[rack$country=="Vietnam"] <- 'VNM'
rack$iso3code[rack$country=="Vietnam, Socialist Republic of"] <- 'VNM'
rack$iso3code[rack$country=="North Vietnam"] <- NA
rack$iso3code[rack$country=="Democratic Republic of Vietnam"] <- NA
rack$iso3code[rack$country=="DRVN"] <- NA
rack$iso3code[rack$country=="Vietnam, North"] <- NA
rack$iso3code[rack$country=="Vietnam North"] <- NA
rack$iso3code[rack$country=="Vietnam, N."] <- NA
rack$iso3code[rack$country=="South Vietnam"] <- NA
rack$iso3code[rack$country=="Republic of Vietnam"] <- NA
rack$iso3code[rack$country=="Vietnam, South"] <- NA
rack$iso3code[rack$country=="Vietnam South"] <- NA
rack$iso3code[rack$country=="Vietnam, S."] <- NA
rack$iso3code[rack$country=="Yemen"] <- 'YEM'
rack$iso3code[rack$country=="Yemen, Rep."] <- 'YEM'
rack$iso3code[rack$country=="Yemen Arab Republic" & rack$year >= 1990] <- 'YEM'
rack$iso3code[rack$country=="North Yemen"] <- NA
rack$iso3code[rack$country=="Yemen, North"] <- NA
rack$iso3code[rack$country=="Yemen North"] <- NA
rack$iso3code[rack$country=="Yemen Arab Republic"] <- NA
rack$iso3code[rack$country=="Yemen, N."] <- NA
rack$iso3code[rack$country=="Yemen Arab Republic; N. Yemen"] <- NA
rack$iso3code[rack$country=="South Yemen"] <- NA
rack$iso3code[rack$country=="Yemen, South"] <- NA
rack$iso3code[rack$country=="Yemen South"] <- NA
rack$iso3code[rack$country=="Yemen People's Republic"] <- NA
rack$iso3code[rack$country=="SouthYemen"] <- NA
rack$iso3code[rack$country=="Yemen, S."] <- NA
rack$iso3code[rack$country=="Yemen People's Republic; S. Yemen"] <- NA
rack$iso3code[rack$country=="Yugoslavia"] <- NA
rack$iso3code[rack$country=="Serbia and Montenegro"] <- NA
rack$iso3code[rack$country=="Yugoslavia, Fed. Rep."] <- NA
rack$iso3code[rack$country=="Federal Republic of Yugoslavia"] <- NA
rack$iso3code[rack$country=="Yugoslavia (Serbia & Montenegro)"] <- NA
rack$iso3code[rack$country=="Former Yugoslavia"] <- NA
rack$iso3code[rack$country=="Serbia"] <- 'SRB'
rack$iso3code[rack$country=="Zambia"] <- 'ZMB'
rack$iso3code[rack$country=="Zimbabwe"] <- 'ZWE'
rack$iso3code[rack$country=="Kosovo"] <- NA
rack$iso3code[rack$country=="Palestinian Authority"] <- NA
