
# Prøv også stringdist for at finde approximeret trenglighed.
library(stringdist)

# It creates a matrix with the Standard Levenshtein distance between the name fields of both sources
n=1000
dist.name<-adist(head(ds_nomatch_cvr$virksomhed,n),ds_cvr$navn_tekst ,partial = TRUE, ignore.case = TRUE, useBytes = TRUE)
# We now take the pairs with the minimum distance
min.name<-apply(dist.name, 1, min)

match.s1.s2<-NULL  
for(i in 1:nrow(dist.name))
{
    s2.i<-match(min.name[i],dist.name[i,])
    s1.i<-i
    match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,NOMATCH=ds_nomatch_cvr[s1.i,]$virksomhed,CVR=ds_cvr[s2.i,]$navn_tekst, adist=min.name[i]),match.s1.s2)
}

# and we then can have a look at the results
View(match.s1.s2)
# select only those where adist == 0
match.s1.s2 <- filter(match.s1.s2, adist == 0)