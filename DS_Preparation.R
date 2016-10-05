# 
ds1 <- read.delim("IT0808.txt", encoding = 'UTF-8')
ds2 <- read.delim("IT0910.txt", encoding = 'UTF-8')
ds3 <- read.delim("IT1112.txt", encoding = 'UTF-8')
ds4 <- read.delim("IT1314.txt", encoding = 'UTF-8')
ds <- rbind(ds1,ds2,ds3,ds4)
# 
# #fjern variablerne kilde og uklar
 ds <- subset(ds, select = -c(url,kilde,undertitel,annoncetekst,uklar))
#gem som csv
write.csv(ds,'ds.csv', row.names = FALSE)

remove(ds1,ds2,ds3,ds4)
