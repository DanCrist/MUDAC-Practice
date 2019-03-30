#SalesForceHire.R
#@author Dan Crist
#Last Updated 3/25/19
#SalesForceHire.R removes unnessecary columns and creates a csv with the multiple hires first and second jobs respectively
#Documentation of "multiple hire" clients in the SF_Hire csv was inconsistent
#A 'MHErrors.csv' is also made to store those inconsistencies for individual evaluation at a later date

library(readr)
library(dplyr)

#----------------------functions to run before code--------------------------
#used to append prefix/suffix onto column names
appendDataFrameColumns<-function(df, prefix="", suffix="", sep="") {colnames(df) <- paste(prefix, colnames(df), suffix, sep=sep)
return(df) }
#-----------------------program body-----------------------------------------
#load SalesForce_Hire_Information__c.csv into Rstudio
SF_Hire<- read_csv("2018/SalesForce_Hire_Information__c.csv")
SF_Hire<-select(SF_Hire,-Id,-IsDeleted,-Name,-CreatedById,-LastModifiedById,-SystemModstamp,-X7_Day_Review__c,-Hired_With_EO_Assistance__c,-Hire_Confirmed_By__c,-Area_Manager_Approved__c,-PIM_Approved__c,-Ops_Review_Date__c)
View(SF_Hire)

#create csv that has only multiple hires
MHireOnly <- filter(SF_Hire,SF_Hire$Multiple_Hire__c==1)
mhirenames<-unique(MHireOnly$Client_Name__c)
FirstHireOnly<-filter(SF_Hire,Multiple_Hire__c==0 | is.na(Multiple_Hire__c)) #take any multiple hire 1's out
#is there any overlap in names? Can we get only them
sum(FirstHireOnly$Client_Name__c %in% mhirenames) #there are 192 multiple hires, and their names also appear in the table without a 1 for 'multiple hire'
mhFirstHire <- filter(FirstHireOnly,Client_Name__c %in% mhirenames)
duplicatesinnmh<-c('0030z00002QXVYrAAP', '0030z00002RoUh8AAF', '0030z00002StaM9AAJ', '0033800002cveGHAAY', '0033800002eHvnGAAS', '0033800002eyAAzAAM', '0033800002iCoMQAA0', '0033800002n7y7oAAA', '0033800002n8CJ1AAM', '0033800002sJlYfAAK', '0033800002sKJ6OAAW', '0033800002sL7QcAAK', '0033800002sLAaxAAG', '0033800002UHgooAAD', '0033800002UlFQAAA3', '00350000026V7ldAAC', '0035000002JBu0IAAT', '0035000002JSGIzAAP', '0035000002QUDHLAA5')
Nonmultihire <- filter(FirstHireOnly,!(Client_Name__c %in% mhirenames))%>%filter(!(Client_Name__c %in% duplicatesinnmh))
#write.csv(mhFirstHire,"multiplehire_first_hire.csv")
write.csv(Nonmultihire,"NonMultiHire.csv")

#Sort by ID so df can be joined by row
a<-mhFirstHire[order(mhFirstHire$Client_Name__c),]
b<-MHireOnly[order(MHireOnly$Client_Name__c),]
#Put the mismatches and duplicates into their own df for later
d<-filter(a,Client_Name__c=="0035000001WAEiSAAX" | Client_Name__c=="0033800002kCJj9AAG" | Client_Name__c=="0035000002PTiTOAA1" | Client_Name__c=="0035000001nMk2bAAC" | Client_Name__c=="00350000029y0HzAAI")
c<-filter(b,Client_Name__c=="0035000001WAEiSAAX" | Client_Name__c=="0033800002kCJj9AAG" | Client_Name__c=="0035000002PTiTOAA1" | Client_Name__c=="0035000001nMk2bAAC" | Client_Name__c=="00350000029y0HzAAI")
e<-merge(c,d,by = intersect(names(c),names(d)),all=TRUE)
#Remove the duplicates from the two tables to be joined
a<-filter(a,!Client_Name__c %in% e$Client_Name__c)
b<-filter(b,!Client_Name__c %in% e$Client_Name__c)%>%appendDataFrameColumns(prefix="J2")
MHJoined<-left_join(a,b,by=c("Client_Name__c"="J2Client_Name__c"))
MHJoined<-MHJoined[,c("Client_Name__c",setdiff(names(MHJoined), "Client_Name__c"))]
write.csv(MHJoined,"MHJoinedv1.csv")  
#add in the unnmarked duplicates from before
duplinmh <- filter(FirstHireOnly,!(Client_Name__c %in% mhirenames))%>%filter(Client_Name__c %in% duplicatesinnmh)
e<-merge(e,duplinmh,by = intersect(names(c),names(d)),all=TRUE)
write.csv(e,"MHerrors.csv")
