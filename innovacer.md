Aim
===

To distinguish the unique data points having duplicate records with
different spelling in First name and Last name.

Dataset
=======

    library(stringdist)

    ## Warning: package 'stringdist' was built under R version 3.4.3

    data<-read.csv("innovacer.csv")
    data[1:10,]

    ##              ln      dob gn      fn
    ## 1      SMITH JR 01/03/68  F WILLIAM
    ## 2  ROTHMEYER JR 01/03/68  F WILLIAM
    ## 3       ASBY JR 01/03/68  F WILLIAM
    ## 4     SALTER JR 01/03/68  F WILLIAM
    ## 5     SALTER JR 01/03/68  F WILLIAM
    ## 6      BLAND JR 21/02/62  F WILLIAM
    ## 7      BLAND JR 21/02/62  F WILLIAM
    ## 8      BLAND JR 21/02/62  F WILLIAM
    ## 9    SHAFFER JR 21/02/62  F WILLIAM
    ## 10   SHAFFER JR 21/02/62  F WILLIAM

-There are Four variable asln(Last name),dob(Date of
birth),gn(Gender),fn(First name) -6,7,8 are duplicate -There are other
duplicate rows with spelling mistake in ln and fn.

Understanding the single iteration of the Model
===============================================

    data$S.no.<-c(seq(1,nrow(data)))
      data$dob<-as.character(data$dob,format='%d-%m-%Y')
      data$gn<-as.character(data$gn)
      data$fn<-as.character(data$fn)
      data$ln<-as.character(data$ln)
      data$ln<-tolower(data$ln)
      data$gn<-tolower(data$gn)
      data$fn<-tolower(data$fn)
      head(data)

    ##             ln      dob gn      fn S.no.
    ## 1     smith jr 01/03/68  f william     1
    ## 2 rothmeyer jr 01/03/68  f william     2
    ## 3      asby jr 01/03/68  f william     3
    ## 4    salter jr 01/03/68  f william     4
    ## 5    salter jr 01/03/68  f william     5
    ## 6     bland jr 21/02/62  f william     6

-Creating unique S.no. for each data point and converting all variables
to character type. -Converting ln and fn variables to lower case to
avoid any case confusion.

    output<-data.frame(ln=character(),dob=character(),gn=character(),fn=character())

-Creating an empty output data frame to store individual records

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
        head(newdata1)

    ##             ln      dob gn      fn S.no.
    ## 1     smith jr 01/03/68  f william     1
    ## 2 rothmeyer jr 01/03/68  f william     2
    ## 3      asby jr 01/03/68  f william     3
    ## 4    salter jr 01/03/68  f william     4
    ## 5    salter jr 01/03/68  f william     5

-Selected the first row and filtered first the data points having same
dob and gnas that of first one. -Then filtered out the data points
within the earlier filtered out having First letter of the fn same as
that of the first data points. -newdata1 consist all the finally
filtered out datapoints.

    similarity_fn<-stringsim(fn,newdata1$fn,method = 'cosine')
        lamda=c(.7) ## Setting cutoff range for similarity
        logical<-c(similarity_fn>=lamda)
        updated_newdata<-newdata1[logical,]
        head(updated_newdata)

    ##             ln      dob gn      fn S.no.
    ## 1     smith jr 01/03/68  f william     1
    ## 2 rothmeyer jr 01/03/68  f william     2
    ## 3      asby jr 01/03/68  f william     3
    ## 4    salter jr 01/03/68  f william     4
    ## 5    salter jr 01/03/68  f william     5

-Filtering out the data points in newdata1 having similarity score of
the first name with the first datapoints greater then 0.7. -Since all
the datapoints in newdata1 have same name in fn variable ,hence for all
the simlarity score is 1 which is greater then 0.7.

    updated_newdata1<-data.frame(ln=character(),dob=character(),gn=character(),fn=character())
        for(i in seq(1,nrow(updated_newdata))){
          if(unique(strsplit(updated_newdata[i,]$ln,split = "")[[1]])[1]==ln_firststring){
            updated_newdata1<-rbind(updated_newdata1,updated_newdata[i,])
          }}
        similarity_ln<-stringsim(ln,updated_newdata1$ln,method = 'cosine')
        beta<-c(.7)
        logical1<-c(similarity_ln>=beta)
        updated_newdata1<-updated_newdata1[logical1,]
        head(updated_newdata1)

    ##         ln      dob gn      fn S.no.
    ## 1 smith jr 01/03/68  f william     1

-Finally filtering out the data points in updated\_newdata with same
steps of checking first letter and similarity score for the Lastname
i.e. ln. -Since only first data point is similar to first,hence only one
record is there in updated\_newdata1. -For points which were there in
updated\_newdata and are not there in updated\_newdata1 have different
ln.

    output<-rbind(output,updated_newdata1[1,])
        eliminate<-updated_newdata1$S.no.
        data<-data[!(data$S.no.%in%eliminate),]
        head(output)

    ##         ln      dob gn      fn S.no.
    ## 1 smith jr 01/03/68  f william     1

        head(data)

    ##             ln      dob gn      fn S.no.
    ## 2 rothmeyer jr 01/03/68  f william     2
    ## 3      asby jr 01/03/68  f william     3
    ## 4    salter jr 01/03/68  f william     4
    ## 5    salter jr 01/03/68  f william     5
    ## 6     bland jr 21/02/62  f william     6
    ## 7     bland jr 21/02/62  f william     7

-Adding the first datapoint in updated\_newdata1 to output.In this case
there is only one datapoint in updated\_newdata1 hence this datapoint is
added to output. -Removing the datapoints which are there in
updated\_newdata1 from the data -This is only one iteration.All these
are repeated until all the data points are eliminated from data

Final code
==========

    data<-read.csv("innovacer.csv")
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
      head(output)

    ##             ln      dob gn      fn S.no.
    ## 1     smith jr 01/03/68  f william     1
    ## 2 rothmeyer jr 01/03/68  f william     2
    ## 3      asby jr 01/03/68  f william     3
    ## 4    salter jr 01/03/68  f william     4
    ## 6     bland jr 21/02/62  f william     6
    ## 9   shaffer jr 21/02/62  f william     9

      head(data)

    ## [1] ln    dob   gn    fn    S.no.
    ## <0 rows> (or 0-length row.names)

-Finally output data frame contain all the unique nams present in data.
-Finall data has 0 rows.
