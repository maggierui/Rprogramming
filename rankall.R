rankall<-function(outcome,num="best"){
	##read table
	dat<-read.csv("outcome-of-care-measures.csv")
	
	##define variables
	statevalid<-FALSE ##statevalid initialized as "No"
	outcomevalid<-FALSE ##outcomevalid initialized as "No"
	outcomelist<-c("heart attack", "heart failure", "pneumonia")## outcome list

	
	##check if outcome input is valid, by going through all outcome names;decide outcome column number	
	for(i in 1:3){
		if(outcome==outcomelist[i]){
			outcomevalid<-TRUE
			outcome_col<-(11+6*(i-1))
			break
		}
	}
	
	##stop the function and throw error messages if outcome is not valid.
	if (outcomevalid==FALSE)
	stop("invalid outcome")
	
	##subset rows where state is the input state and only have columns of "Hospital Name" and "outcome"
	dat<-dat[,c(2,7,outcome_col)]
	colnames(dat)<-c("hospital","state", "rate")

	##convert all factors in dat dataframe into characters
	dat[,1]<-as.character(dat[,1])
	dat[,3]<-as.character(dat[,3])

	##remove all rows with rate "Not Available"
	dat<-dat[as.character(dat[,3])!="Not Available",]

	dat[,3]<-as.numeric(dat[,3])
				
	##only retain complete cases	
	dat<-dat[complete.cases(dat),]
	dat<-dat[order(dat$state,dat$rate,dat$hospital),]

	##define result data frame
	##result<-data.frame(hospital=character(0),state=character(0),rate=numeric(0))
	result<-data.frame()
	dat_bystate<-split(dat,dat$state)
	##print(dat_bystate)
	##print(dat_bystate[[(length(dat_bystate)-1)]])
	##print(length(dat_bystate))

	for(i in 1:length(dat_bystate)){
		temp_state<-dat_bystate[[i]]
		n<-nrow(temp_state)
		##make sure num is valid
		if(num=="best"){
			result_n<-1
		}
		else if(num=="worst"){
			result_n<-n
		}
		else {
			result_n<-num
		}
		temp_rank<-temp_state[result_n,]
		if(result_n>n){
			temp_rank[,2]<-temp_state[1,2]
		}
		result<-rbind(result,temp_rank)
	}
	
	result[,c(1:2)]
}