data<-read.csv("CustChangeCat.csv")
data<-data[data$SUM.completed_booking_email<=data$completed_booking_all+5,]
data$Email<-as.character(data$Email)
#data<-data[data$completed_booking_all!=0,]
data$NC[1]=0
data$EC[1]=0
data$VIP[1]=0

email<-unique(data$Email)
status<-rep(1,length(email))
truth_table<-cbind(as.character(email),status)
colnames(truth_table)<-c("email","status")
truth_table<-as.data.frame(truth_table)
truth_table$email<-as.character(truth_table$email)
truth_table$status<-as.integer(truth_table$status)

for(i in 2:nrow(data)){
	if(truth_table[truth_table$email==data$Email[i],]$status==0){
		data$NC[i]=data$NC[i-1]
		data$EC[i]=data$EC[i-1]
		data$VIP[i]=data$VIP[i-1]
	}

	if(truth_table[truth_table$email==data$Email[i],]$status==1){
		if(data$completed_booking_all[i]==0){
			data$NC[i]=data$NC[i-1]
			data$EC[i]=data$EC[i-1]
			data$VIP[i]=data$VIP[i-1]
		}
	
		if(data$completed_booking_all[i]==1){
			if(data$SUM.completed_booking_email[i]==1){
				data$NC[i]=data$NC[i-1]+1
				data$EC[i]=data$EC[i-1]
				data$VIP[i]=data$VIP[i-1]
			}
	
			if(data$SUM.completed_booking_email[i]==2){
				data$NC[i]=data$NC[i-1]-1
				data$EC[i]=data$EC[i-1]+1
				data$VIP[i]=data$VIP[i-1]
			}
	
			if(data$SUM.completed_booking_email[i]==3){
				data$NC[i]=data$NC[i-1]
				data$EC[i]=data$EC[i-1]
				data$VIP[i]=data$VIP[i-1]
			}
	
			if(data$SUM.completed_booking_email[i]==4){
				data$NC[i]=data$NC[i-1]
				data$EC[i]=data$EC[i-1]
				data$VIP[i]=data$VIP[i-1]
			}
	
			if(data$SUM.completed_booking_email[i]>=5){
				data$NC[i]=data$NC[i-1]
				data$EC[i]=data$EC[i-1]-1
				data$VIP[i]=data$VIP[i-1]+1
				truth_table[truth_table$email==data$Email[i],]$status=0
			}	
		}

		if(data$completed_booking_all[i]==2){
			if(data$SUM.completed_booking_email[i]==2){
				data$NC[i]=data$NC[i-1]
				data$EC[i]=data$EC[i-1]+1
				data$VIP[i]=data$VIP[i-1]
			}
	
			if(data$SUM.completed_booking_email[i]==3){
				data$NC[i]=data$NC[i-1]-1
				data$EC[i]=data$EC[i-1]+1
				data$VIP[i]=data$VIP[i-1]
			}
	
			if(data$SUM.completed_booking_email[i]==4){
				data$NC[i]=data$NC[i-1]
				data$EC[i]=data$EC[i-1]
				data$VIP[i]=data$VIP[i-1]
			}
	
			if(data$SUM.completed_booking_email[i]>=5){
				data$NC[i]=data$NC[i-1]
				data$EC[i]=data$EC[i-1]-1
				data$VIP[i]=data$VIP[i-1]+1
				truth_table[truth_table$email==data$Email[i],]$status=0
			}	
		}

		if(data$completed_booking_all[i]==3){
			if(data$SUM.completed_booking_email[i]==3){
				data$NC[i]=data$NC[i-1]
				data$EC[i]=data$EC[i-1]+1
				data$VIP[i]=data$VIP[i-1]
			}
	
			if(data$SUM.completed_booking_email[i]==4){
				data$NC[i]=data$NC[i-1]-1
				data$EC[i]=data$EC[i-1]+1
				data$VIP[i]=data$VIP[i-1]
			}
	
			if(data$SUM.completed_booking_email[i]>=5){
				data$NC[i]=data$NC[i-1]
				data$EC[i]=data$EC[i-1]-1
				data$VIP[i]=data$VIP[i-1]+1
				truth_table[truth_table$email==data$Email[i],]$status=0
			}	
		}

		if(data$completed_booking_all[i]==4){
			if(data$SUM.completed_booking_email[i]==4){
				data$NC[i]=data$NC[i-1]
				data$EC[i]=data$EC[i-1]+1
				data$VIP[i]=data$VIP[i-1]
			}
	
			if(data$SUM.completed_booking_email[i]==5){
				data$NC[i]=data$NC[i-1]-1
				data$EC[i]=data$EC[i-1]
				data$VIP[i]=data$VIP[i-1]+1
				truth_table[truth_table$email==data$Email[i],]$status=0
			}
	
			if(data$SUM.completed_booking_email[i]>5){
				data$NC[i]=data$NC[i-1]
				data$EC[i]=data$EC[i-1]-1
				data$VIP[i]=data$VIP[i-1]+1
				truth_table[truth_table$email==data$Email[i],]$status=0
			}		
		}
	
		if(data$completed_booking_all[i]>=5){
			if(data$SUM.completed_booking_email[i]>=5){
				data$NC[i]=data$NC[i-1]
				data$EC[i]=data$EC[i-1]
				data$VIP[i]=data$VIP[i-1]+1
				truth_table[truth_table$email==data$Email[i],]$status=0
			}		
		}
	}
}

write.csv(data,"clean.csv",row.names=FALSE)