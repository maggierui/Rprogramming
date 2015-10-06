best<-function(state,outcome){
	##read table
	dat<-read.csv("outcome-of-care-measures.csv")
	
	##define variables
	hospital<-NULL ##to be returned hospital name
	statevalid<-"No" ##statevalid initialized as "No"
	outcomevalid<-"No" ##outcomevalid initialized as "No"
	outcomelist<-c("heart attack", "heart failure", "pneumonia")## outcome list

	## check if state input is valid, by going through all state names in the data
	n<-nrow(dat)	
	for(i in 1:n){
		if(state==dat[i,"State"]){
			statevalid<-"Yes"
			break
		}
	}

	##check if outcome input is valid, by going through all outcome names;decide outcome column number	
	for(i in 1:3){
		if(outcome==outcomelist[i]){
			outcomevalid<-"Yes"
			outcome_col<-(11+6*(i-1))
		}
	}
	
	##stop the function and throw error messages if state or outcome is not valid.
	if (statevalid=="No")
	stop("invalid state")
	if (outcomevalid=="No")
	stop("invalid outcome")
	
	##subset rows where state is the input state and only columns of "Hospital Name" and "outcome"
	dat<-dat[dat$State==state,c(2,outcome_col)]
	lowest_rate<-dat[1,2] ##initial lowest thirtyday outcome as first row's outcome
	lowest_rate<-as.numeric(as.character(lowest_rate))
	best_hospital<-dat[1,1] ##a vector contains the initial best hospital name
	##best_hospital<-as.character(best_hospital)
	dat<-dat[complete.cases(dat),]
	
	##going through all outcome values in the data, and compare it with lowest_rate
	n<-nrow(dat)
	for(i in 1:n){
		temp<-dat[i,2]
		if(temp=="Not Available"){
			next
		}
		temp<-as.numeric(as.character(temp))
		if(temp<lowest_rate){
			lowest_rate<-temp
			best_hospital<-dat[i,1]
		}
		else if(temp==lowest_rate){
			temp_best<-dat[i,1]
			if (as.character(temp_best)<as.character(best_hospital)){
				best_hospital<-temp_best
			}
		}
	}
	
	best_hospital<-as.character(best_hospital)
	best_hospital
	##print(lowest_rate)
}