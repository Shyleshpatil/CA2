#Section 2
#To remove Objects

rm(list = ls(all=TRUE))

#To display current working directory

getwd()
setwd("C:/Users/Shylesh Patil/Downloads/NI Crime Data/")

#a)Using R, amalgamate all of the crime data from each csv file into one dataset.
#Save this dataset into a csv file called AllNICrimeData. Count and show the number of rows in the AllNICrimeData dataset

File=list.files(pattern = "[.]csv$",recursive = T)

#Assuming values with Header.

ALLNICrimeData_List=lapply(File,function(x)read.csv(x, header = TRUE))

#Taking same header columns for all files

ALLNICrimeData=do.call("rbind",ALLNICrimeData_List)
write.csv(ALLNICrimeData,"ALLNICrimeData.csv")
nrow(ALLNICrimeData)

#b)Modify the structure of the newly created AllNICrimeData csv file and remove
#the following attributes: CrimeID, Reported by, Falls within, LSOA code, LSOA name,
#last outcome and context. Save this new structure and show the structure of the modified file

NewAllNICrimeData=subset(ALLNICrimeData,select = -c(Crime.ID,Reported.by,Falls.within,LSOA.code,LSOA.name,Last.outcome.category))

#c)In the AllNICrimeData csv file, shorten each crime type as follows:

unique(ALLNICrimeData$Crime.type)
library(dplyr)
NewAllNICrimeData=NewAllNICrimeData%>%mutate(Crime.type=recode_factor(Crime.type,'Anti-social behaviour'='ASBO','Bicycle theft'='BITH','Burglary'='BURG',
                                                                      'Criminal damage and arson'='CDAR','Drugs'='DRUG','Other theft'='OTTH',
                                                                      'Possession of weapons'='POFW','Public order'='PUBO','Robbery'='ROBY','Shoplifting'='SHOP',
                                                                      'Theft from the person'='THPR','Vehicle crime'='VECR',
                                                                      'Violence and sexual offences'='VISO','Other crime'='OTCR'))

#d)Using the plot() function, show a plot of each crime frequency from the crime.type field.
#Specify relevant options such as title, axes labels, bar colours.
#Provide a detailed discussion about what you found from this chart.

counts <- table(NewAllNICrimeData$Crime.type)
My_Plot=barplot(counts,main = "Distribution of Crime Type and frequency of each crime type", ylab = 'frequency',
                col = c(rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.4,0.5,0.6),rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),
                        rgb(0.9,0.9,0.4,0.6),rgb(0.3,0.3,0.4,0.6),rgb(0.7,0.1,0.4,0.6),rgb(0.1,0.5,0.4,0.6),
                        rgb(0.3,0.9,0.9,0.6),rgb(0.3,0.9,0.9,0.6),rgb(0.3,0.1,0.9,0.6),rgb(0.3,0.5,0.4,0.6),
                        rgb(0.3,0.9,0.4,0.9),rgb(0.3,0.9,0.4,0.9)),xlab = 'Crime Type',las=2) 
text(My_Plot,counts/2,paste("",counts,sep = ""),cex=1)

#e) Modify the AllNICrimeData dataset so that the Location attribute contains only a 
#street name. For example, the attribute value “On or near Westrock Square” should
#be modified to only contain “Westrock Square”. Modify the resultant empty location
#attributes with a suitable identifier. Show a sample of your data.

library(stringr)
NewAllNICrimeData$Location <- str_remove(NewAllNICrimeData$Location,"On or near")
NewAllNICrimeData$Location

#f)Choose 5000 random samples of crime data from the AllNICrimeData dataset
#where the location attribute contains location information. This means that the
#location information should NOT contain an NA identifier. Set the seed value to 100.
#Store this data in a data frame called random_crime_sample.... 

NewAllNICrimeData=NewAllNICrimeData[!( NewAllNICrimeData$Location==""), ]
rows=seq(1,nrow(NewAllNICrimeData),1)
set.seed(100)
rows
random_crime_sample=NewAllNICrimeData[sample(rows, 5000), ]
random_crime_sample
CleanNIPostcodeData = read.csv('CleanNIPostcodeData.csv', header = TRUE)
head(CleanNIPostcodeData)
find_a_town <- function(i) {
  random_crime_sample$City_Town_Village<-CleanNIPostcodeData[match(toupper(random_crime_sample$Location),CleanNIPostcodeData$Primary.Thorfare),11]
}
random_crime_sample$City_Town_Village
find_a_town(toupper(random_crime_sample$Location))

#g)Create a function called add_town_data that examines the information from
#each crime record in random_crime_sample and matches each record with relevant data in the VillageList.csv file.
#Add the population attribute to the random_crime_sample data frame.

random_crime_sample$Context = NULL
VillageList = read.csv('VillageList.csv', header = TRUE)
add_town_data <- function(i) {
  random_crime_sample$POPULATION <- VillageList[match(random_crime_sample$City_Town_Village,toupper(VillageList$CITY.TOWN.VILLAGE)),2]
}
add_town_data(random_crime_sample$City_Town_Village)
write.csv(random_crime_sample,'random_crime_sample.csv', row.names = FALSE)
random_crime_sample
