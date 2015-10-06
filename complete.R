complete<-function(directory,id=1:332){
	files_full<-list.files(directory,full.names=TRUE)
	dat<-data.frame()
	for(i in id){
		nobs<-sum(complete.cases(read.csv(files_full[i])))
            complete_observe<-c(i,nobs)
		dat<-rbind(dat,complete_observe)
	}
	names(dat)<-c("id","nobs")	
	dat
}