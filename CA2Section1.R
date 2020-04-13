#Section 1
#Read the CSV file.
#Store it in a Data Frame.
postcodeDataframe <- read.csv("/Users/Shylesh Patil/Downloads/NIPostcodes.csv", header = FALSE)

#a)Show the total number of rows, the structure of the data frame,
#and first 10 rows of the data frame containing all of the NIPostcode data.

#1) Show Row

nrow(postcodeDataframe)

#2)Struture of the dataframe.

str(postcodeDataframe)

#3)First and Last 10 rows of dataframe.

head(postcodeDataframe, n=10)
tail(postcodeDataframe, n=10)

#b)Add a suitable title for each attribute of the data.

new_colnames <- c("Organisation_Name","Sub_Building_Name","Building_Name","Number","Primary_Thorfare",
                                  "Alternative_Thorfare","Secondary_Thorfare","Locality","Townland","Town","County","PostCode",
                                  "XCordinates","YCordinates","Primary_Key")

colnames(postcodeDataframe) <- new_colnames
head(postcodeDataframe,10)

#c)Replace and recode all missing entries with a suitable identifier. 
#Decide whether it is best to remove none, some or all of the missing data. 
#Discuss the missing data using suitable graphical output to explain your answer and justify your decision in detail.

postcodeDataframe[postcodeDataframe==""] <- NA
sum(is.na(postcodeDataframe))
sum(!complete.cases(postcodeDataframe))

# For graphical view

#install.packages("mice")
library(mice)
md.pattern(postcodeDataframe)
library("VIM")
missing_values <- aggr(postcodeDataframe, prop = FALSE, numbers = TRUE)


#d)Show the total number of missing values for each column in the postcode data frame both before and after 
#your decision to deal with the missing data variables

Missing_Count <- sapply(postcodeDataframe, function(y) sum(length(which(is.na(y)))))
Missing_Count <- data.frame(Missing_Count)

Missing_Count

#e)Move the primary key identifier to the start of the dataset. 

postcodeDataframe <- postcodeDataframe[, c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
postcodeDataframe

#f)Create a new dataset called Limavady_data. Store within it only information where locality,
#townland and town contain the name Limavady. Count and display the number of rows.
#Store this information in a csv file called Limavady. 

Limavady_data <- postcodeDataframe[which(postcodeDataframe$Locality == "LIMAVADY" | postcodeDataframe$Townland == "LIMAVADY" & postcodeDataframe$Town == "LIMAVADY"),]
Limavady_data
nrow(Limavady_data)

write.csv(Limavady_data,"Limavady.csv")
write.csv(postcodeDataframe,"CleanNIPostcodeData.csv")


