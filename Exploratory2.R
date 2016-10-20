
# It creates a matrix with the Standard Levenshtein distance between the name fields of both sources
Matches <- NULL
for (n in 1:10)
    {
    dist.name<-adist(distinct_nm[n,],ds_cvr$navn_tekst, partial = TRUE, ignore.case = TRUE, useBytes = TRUE)
    if (min(dist.name) <= 1) Matches <- rbind(data.frame(counter=n, no.match.name=distinct_nm[n,],cvr.name=ds_cvr[which.min(dist.name),"navn_tekst"],levenshtein.distance=min(dist.name)),Matches)
}

View(Matches)

# We now take the pairs with the minimum distance
# min.name<-apply(dist.name, 1, min)
# 
# match.s1.s2<-NULL
# for(i in 1:nrow(dist.name))
# {
#     s2.i<-match(min.name[i],dist.name[i,])
#     s1.i<-i
#     match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,NOMATCH=distinct_nm[s1.i,],CVR=ds_cvr[s2.i,]$navn_tekst, adist=min.name[i]),match.s1.s2)
# }
# 
# # select only those where adist == 0
# match.s1.s2 <- filter(match.s1.s2, adist == 0)
# # and we then can have a look at the results
# View(match.s1.s2)
