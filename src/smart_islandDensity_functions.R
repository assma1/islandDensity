#rm(list=ls())

#removal nights
t <- c(1:6) 

#individuals removed each night
ct<- removalData$removalNumbers[1:6]

#row length of text model
nrows<-length(ct)

#removal sum each night
sumct<-c(0, cumsum(ct)[-nrows])

#assign jags model for horan removal data
horan<-jags.model(file= "src/removalModel.txt", n.chains=3, data=environment(), quiet=TRUE)

#update model
update(horan, n.iter=30000)

#sample posterior and save output
horanOutput<-coda.samples(horan, c("p", "lambda", "days"), thin = 2, n.iter=10000, quiet=TRUE)

#as above with indian island data
ct<- removalData$removalNumbers[7:12] #assign indian island removal nights
sumct<-c(0, cumsum(ct)[-nrows])
indian<-jags.model(file= "src/removalModel_II.txt", n.chains=3, data=environment(), quiet=TRUE)
update(indian, n.iter=30000)
indianOutput<-coda.samples(indian, c("p", "lambda", "days"), thin = 2, n.iter=10000, quiet=TRUE)
