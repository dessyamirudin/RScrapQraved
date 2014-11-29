library("RCurl")
library("XML")

front_file_title<-"viewpage"
ext<-".html"
titlepost<-""
numberviewpost<-""
authorpost<-""
datepost<-""

for (num in 1:13){
	file<-paste(front_file_title,num,sep="")
	filename<-paste(file,ext,sep="")
	trim <- function(x) gsub("^\\s+|\\s+$", "", x)
	web.html<-htmlTreeParse(filename,useInternalNodes=T)
	
	title<-xpathSApply(web.html,"//div[@class='post_title']",xmlValue)
	author<-xpathSApply(web.html,"//td[@class='author column-author']",xmlValue)
	viewnumber<-xpathSApply(web.html,"//td[@class='wp-statistics column-wp-statistics']",xmlValue)
	
	#date
	datenum<-xpathSApply(web.html,"//div[@class='jj']",xmlValue)
	month<-xpathSApply(web.html,"//div[@class='mm']",xmlValue)
	year<-xpathSApply(web.html,"//div[@class='aa']",xmlValue)

	date_a<-paste(year,month,sep="-")
	date<-paste(date_a,datenum,sep="-")
	
	titlepost<-c(titlepost,title)
	numberviewpost<-c(numberviewpost,viewnumber)
	authorpost<-c(authorpost,author)
	datepost<-c(datepost,date)
}

view<-cbind(titlepost,authorpost,numberviewpost,datepost)
colnames(view)<-c("title","author","views","date")
view<-as.data.frame(view)

view<-view[2:nrow(view),]
write.csv(view,"view.csv",row.names=FALSE)


