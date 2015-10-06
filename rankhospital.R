rankhospital<-function(state,outcome,num="best"){
	##read table
	dat<-read.csv("outcome-of-care-measures.csv")
	
	##define variables
	statevalid<-FALSE ##statevalid initialized as "No"
	outcomevalid<-FALSE ##outcomevalid initialized as "No"
	outcomelist<-c("heart attack", "heart failure", "pneumonia")## outcome list

	## check if state input is valid, by going through all state names in the data
	n<-nrow(dat)	
	for(i in 1:n){
		if(state==dat[i,"State"]){
			statevalid<-TRUE
			break
		}
	}

	##check if outcome input is valid, by going through all outcome names;decide outcome column number	
	for(i in 1:3){
		if(outcome==outcomelist[i]){
			outcomevalid<-TRUE
			outcome_col<-(11+6*(i-1))
			break
		}
	}
	
	##stop the function and throw error messages if state or outcome is not valid.
	if (statevalid==FALSE)
	stop("invalid state")
	if (outcomevalid==FALSE)
	stop("invalid outcome")
	
	##subset rows where state is the input state and only have columns of "Hospital Name" and "outcome"
	dat<-dat[dat$State==state,c(2,outcome_col)]
	colnames(dat)<-c("hospital", "rate")
	##only retain complete cases	
	dat<-dat[complete.cases(dat),]

	##convert all factors in dat dataframe into characters
	dat[]<-lapply(dat,as.character)

	##remove all rows with rate "Not Available"
	dat<-dat[as.character(dat[,2])!="Not Available",]
	
	##dat[,"rank"]<-NA

	n<-nrow(dat)
	for(i in 1:n){
		dat[i,1]<-as.character(dat[i,1])## convert hospital from factor to character
		dat[i,2]<-as.numeric(as.character(dat[i,2]))##convert rate from factor to numeric
	}
	dat<-dat[order(dat$rate,dat$hospital),]
	if(num=="best"){
		result_n<-1
	}
	else if(num=="worst"){
		result_n<-n
	}
	else {
		result_n<-num
	}
	dat[result_n,1]
}