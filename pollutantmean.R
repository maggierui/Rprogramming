pollutantmean<-function(directory,pollutant,id=1:332){
	files.full<-list.files(directory,full.names=TRUE)
	dat<-data.frame()
	for(i in id){
		dat<-rbind(dat,read.csv(files.full[i]))
	}
	mean(dat[,pollutant],na.rm=TRUE)
}