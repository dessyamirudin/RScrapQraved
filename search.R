library("RCurl")
library("XML")

front_file_title<-"searchpage"
ext<-".html"
datepost<-""
searchword<-""

for (num in 1:33){
	file<-paste(front_file_title,num,sep="")
	filename<-paste(file,ext,sep="")
	trim <- function(x) gsub("^\\s+|\\s+$", "", x)
	web.html<-htmlTreeParse(filename,useInternalNodes=T)
	search<-xpathSApply(web.html,"//div[@class='log-referred']",xmlValue)
	postdate<-xpathSApply(web.html,"//div[@class='log-ip']",xmlValue)
	postdatesplit<-strsplit(postdate,split=" ")
	date<-""

	for (i in 1:length(postdatesplit)){
		date=c(date,postdatesplit[[i]][1])
	}
	date<-date[2:length(date)]
	datepost<-c(datepost,date)
	searchword<-c(searchword,search)
}

data<-cbind(searchword,datepost)
colnames(data)<-c("Word","date")
data<-as.data.frame(data)

#category recipe
for(i in 1:nrow(data)){
	if(grepl("\\bresep\\b",data$Word[i])|grepl("\\bcara\\b",data$Word[i])|grepl("\\brecipe\\b",data$Word[i]))
		{
		data$category[i]<-"recipe"
	}
}

data<-data[2:nrow(data),]
write.csv(data,"data.csv",row.names=FALSE)


