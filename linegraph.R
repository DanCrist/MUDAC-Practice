#@author Dan Crist 3/18/19
#linegraph.R is a simple template for creating linegraphs colored by if their values are increasing or decreasing
#labels and graphic sizing will be case specific

library(ggplot2)

#looking at:
#USATotals
#CanadaTotals

#take each value as a total of the max value achieved during period
USAperctotals<-lapply(USATotals[2:19],divider)
USApercents<-as.data.frame(as.data.frame(USAperctotals)) %>% mutate(year=as.numeric(rownames(df))+1999,country='USA',clothingiord=as.numeric(Clothing-lead(Clothing)>=0))
CAperctotals<-lapply(CanadaTotals[2:19],divider)
CApercents=as.data.frame(as.data.frame(CAperctotals)) %>% mutate(year=as.numeric(rownames(df))+1999,country='Canada',clothingiord=as.numeric(Clothing-lead(Clothing)>=0))
CAUSpercents<-merge(USApercents,CApercents,by=intersect(names(USApercents),names(CApercents)),all=TRUE)

#graphs
ggplot(data=CAUSpercents)+geom_line(mapping=aes(x=year,y=Clothing,color=clothingiord))+facet_grid(rows=vars(country))

#--------functions---------
divider<-function(df){
  maxValue = max(df)
  return(df/maxValue)
}