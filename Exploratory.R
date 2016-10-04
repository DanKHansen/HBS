
library(tidyr)
library(dplyr)
#library(stringi)


setwd("~/GIT/HBS")

ds1 <- read.csv("IT0808.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "Latin1", na.strings = c('', ' '))
ds2 <- read.csv("IT0910.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "Latin1", na.strings = c('', ' '))
ds3 <- read.csv("IT1112.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "Latin1", na.strings = c('', ' '))
ds4 <- read.csv("IT1314.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "Latin1", na.strings = c('', ' '))
ds <- rbind(ds1,ds2,ds3,ds4)
#fjern variablerne kilde og uklar
ds <- subset(ds, select = -c(kilde,uklar))

#fjern alle hvor længden af postnummer er forskellig fra 4
#I de rækker der indeholder mere end et postnummer kan arbejdstedet ikke bestemmes entydigt.
#postnummer værdier som f. eks. københavn, skaane osv. kan ikke ikke bestemmes entydigt.
ds <- filter(ds, nchar(postnummer) == 4 | is.na(postnummer))

#formater dato variablen
ds$dato <- as.Date(as.character(ds$dato),'%Y%m%d')

#fjern alle hvor virksomhed ikke er angivet
ds <- subset(ds, !(is.na(virksomhed)))

#fjern alle rækker hvor postnummer indikerer udlandet
#og sæt virksomhed til lower case
ds <- subset(ds, !(postnummer %in% c('0000','0001', '0100')))
ds$virksomhed <- tolower(ds$virksomhed)

#externt ds som benyttes til at finde eksakt adresse og postnummer/kommune
#tag kun firmaer der er registreret før 31-dec-2008
#og sæt navn_tekst til lower case
ds_cvr <- read.csv("39265897_93207_20160808134952_VIRKSOMHEDER.csv", header=TRUE, stringsAsFactors=FALSE)
ds_cvr$livsforloeb_startdato <- as.Date(ds_cvr$livsforloeb_startdato,'%d-%m-%Y')
ds_cvr <- filter(ds_cvr, as.numeric(format(livsforloeb_startdato,'%Y')) < 2009)
ds_cvr$navn_tekst <- tolower(ds_cvr$navn_tekst)

#fjern evt. leading og trailing whitespaces
ds$virksomhed <- trimws(ds$virksomhed, which = 'both')
ds_cvr$navn_tekst <- trimws(ds_cvr$navn_tekst, which = 'both')

#ds_postnr <- read.csv('http://dawa.aws.dk/postnumre?format=csv')

#dplyr
ds_match_cvr <- semi_join(ds,ds_cvr, by = c('virksomhed' = 'navn_tekst'))
ds_nomatch_cvr <- anti_join(ds,ds_cvr, by = c('virksomhed' = 'navn_tekst'))

#benyt adist() til at finde bedste match mellem de to datasæt
#Levenshtein Distance:
#check entry i ds_nomatch_virk$virksomhed op imod ds_virk$navn_tekst
n=6
    testdist <- adist(ds_nomatch_cvr$virksomhed[n], ds_cvr$navn_tekst, fixed = T, useBytes = TRUE)
    print(ds_nomatch_cvr[n,"virksomhed"])
    print(ds_cvr[which.min(testdist),"navn_tekst"])



#skift invalid input ud i basis datasættet:
#ds$virksomhed <- replace(ds$virksomhed, ds$virksomhed == '1508 a/s','1508.dk a/s')

#test <- unite(test,postnr,postnummer,beliggenhedsadresse_postnr,sep = '.',remove = TRUE)

#test2 <- subset(test,select = c(virksomhed,titel,postnr,kommune,funktion,dato,cvrnr,beliggenhedsadresse_vejnavn,beliggenhedsadresse_husnummerFra))

#tmp <- subset(test, startsWith(virksomhed,'alfa'))
#tmp2 <- subset(ds_virk, startsWith(navn_tekst,'alfa'))