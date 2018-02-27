install.packages("stringdist")
model<-function(x){
  library(stringdist)
  data<-x
  data$S.no.<-c(seq(1,nrow(data)))
  data$dob<-as.character(data$dob,format='%d-%m-%Y')
  data$gn<-as.character(data$gn)
  data$fn<-as.character(data$fn)
  data$ln<-as.character(data$ln)
  data$ln<-tolower(data$ln)
  data$gn<-tolower(data$gn)
  data$fn<-tolower(data$fn)
  output<-data.frame(ln=character(),dob=character(),gn=character(),fn=character())
  repeat{
    dob=data$dob[1]
    gn=data$gn[1]
    fn=data$fn[1]
    ln=data$ln[1]
    fn_firststring<-unique(strsplit(fn,split = "")[[1]])[1]
    ln_firststring<-unique(strsplit(ln,split = "")[[1]])[1]
    newdata<-data[data$dob==dob & data$gn==gn,]
    newdata$fn<-as.character(newdata$fn)
    newdata$ln<-as.character(newdata$ln)
    newdata$gn<-as.character(newdata$gn)
    newdata1<-data.frame(ln=character(),dob=character(),gn=character(),fn=character())
    for(i in seq(1,nrow(newdata))){
      if(unique(strsplit(newdata[i,]$fn,split = "")[[1]])[1]==fn_firststring){
        newdata1<-rbind(newdata1,newdata[i,])
      }}
    similarity_fn<-stringsim(fn,newdata1$fn,method = 'cosine')
    lamda=c(.7) ## Setting cutoff range for similarity
    logical<-c(similarity_fn>=lamda)
    updated_newdata<-newdata1[logical,]
    updated_newdata1<-data.frame(ln=character(),dob=character(),gn=character(),fn=character())
    for(i in seq(1,nrow(updated_newdata))){
      if(unique(strsplit(updated_newdata[i,]$ln,split = "")[[1]])[1]==ln_firststring){
        updated_newdata1<-rbind(updated_newdata1,updated_newdata[i,])
      }}
    similarity_ln<-stringsim(ln,updated_newdata1$ln,method = 'cosine')
    beta<-c(.7)
    logical1<-c(similarity_ln>=beta)
    updated_newdata1<-updated_newdata1[logical1,]
    output<-rbind(output,updated_newdata1[1,])
    eliminate<-updated_newdata1$S.no.
    data<-data[!(data$S.no.%in%eliminate),]
    if(nrow(data)==0){break}
  }
  return(output)
}  
    
    
    
                
    
    
    
    
    
    