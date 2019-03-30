#@author Dan Crist 3/12/19
#UN.R cleans 'export' csv pulled from online UN database of uninformative or unnecessary columns
#Uses imagemagick and fmsb radarplot function to visualize changes in export value by developed country via images/gifs
#Code will need to be edited and run for each country of choice. Specific comodities must also be selected to make visual less cluttered.

library(dplyr)
library(tidyverse)
library(fmsb)
library(magick)

ExportsByCountry <-read.csv("exports_by_country.csv")
EBCclean<-select(ExportsByCountry,country_code,country_type,country_english_name,year,period,period_in_date,commodity_group_code,commodity_group_name,partner_country_code,partner_country_english_name,currency_type,value,value_type_desc)

#write.csv(EBCclean,"EBCclean.csv")

#Of the developed economies which one exports the most per year? Most per capita? Most per person in the workforce?
countrySumVal<-group_by(EBCclean,country_english_name) %>% summarise(total_export_value=sum(value))


#Let's break down the commodities exported by each country
totals<-EBCclean %>% group_by(commodity_group_name,year,country_english_name) %>% summarise(total=sum(value))
#Asia-Pacific
APTotals<-yearlyCollapse("Asia-Pacific")
#Australia
AustraliaTotals<-yearlyCollapse("Australia")
#Canada
CanadaTotals<-yearlyCollapse("Canada")
#Developed Economies
DETotals<-yearlyCollapse("Developed Economies")
#Europe
EuropeTotals<-yearlyCollapse("Europe")
#France
FranceTotals<-yearlyCollapse("France")
#Germany
GermanyTotals<-yearlyCollapse("Germany")
#Japan
JapanTotals<-yearlyCollapse("Japan")
#North America
NorthAmericaTotals<-yearlyCollapse("North America")
#United Kingdom
UKTotals<-yearlyCollapse("United Kingdom")
#USA
USATotals<-yearlyCollapse("USA")

#Make Radar Plots and save them as images
for(i in 1:18){
  filename<-paste('images/img',i,'.jpg',sep="")
  jpeg(filename)
  radrplotmkr(USATotals,i)
  dev.off()
}

#reload the plots as image magick elts
img1<-image_read("images/img1.jpg")
img2<-image_read("images/img2.jpg")
img3<-image_read("images/img3.jpg")
img4<-image_read("images/img4.jpg")
img5<-image_read("images/img5.jpg")
img6<-image_read("images/img6.jpg")
img7<-image_read("images/img7.jpg")
img8<-image_read("images/img8.jpg")
img9<-image_read("images/img9.jpg")
img10<-image_read("images/img10.jpg")
img11<-image_read("images/img11.jpg")
img12<-image_read("images/img12.jpg")
img13<-image_read("images/img13.jpg")
img14<-image_read("images/img14.jpg")
img15<-image_read("images/img15.jpg")
img16<-image_read("images/img16.jpg")
img17<-image_read("images/img17.jpg")
img18<-image_read("images/img18.jpg")

temp<-c(img1,img2,img3,img4,img5,img6,img7,img8,img9,img10,img11,img12,img13,img14,img15,img16,img17,img18)
#Animate
myimg<-image_animate(temp,fps=1)
image_write_gif(myimg,"mygif.gif")

#functions:
#yearlyCollapse() takes name of country and collapse to yearly value per export
yearlyCollapse<-function(name){
df<-filter(totals,country_english_name==name) %>% select(-country_english_name) %>% spread(key=commodity_group_name,value=total)
return(df)}

#radrplotmkr() takes country name and year int (number of years after 2000) and returns radar plot
radrplotmkr<-function(df,yearnum){
as.data.frame(rbind(apply(df,2,max)[2:19],rep(0,18),as.matrix(df[yearnum,2:19]))) %>% radarchart(pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4,cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,title=paste('USA Exports',yearnum+1999))
}
