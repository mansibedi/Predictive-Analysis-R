getwd()
train <- read.csv("order_detail_march.csv")
train2 <- read.csv("order_detail_april.csv")
dim(train)
dim(train2)
str(train)  #check datatype
colSums(is.na(train)) #checks the entire data set for NAs and return logical output
summary(train)
install.packages("ggplot2")
library(ggplot2)
ggplot(train, aes(x= city, y = vehicle_type)) + geom_point(size = 2.5, color="navy") + xlab("city") + ylab("vehicle_type") + ggtitle("City Vs Vehicle type")
ggplot(train, aes(x= city, y = Price)) + geom_col(stat="identity", color="navy") + xlab("city") + ylab("price") + ggtitle("City Vs Price") + theme(axis.line = element_line(color="black", size = 2)) 
ggplot(train, aes(city, Price)) + geom_bar(stat = "identity", color = "purple") + xlab("city") + ylab("price") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + ggtitle("city vs Price")
ggplot(train, aes(city, Price)) + geom_bar(stat = "identity", color = "purple") + xlab("city") + ylab("price") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + ggtitle("city vs Price") + scale_x_discrete(limit = c("mumbai", "gurgaon", "pune")) #to show plot for states specified

ggplot(train, aes(GACity, oid)) + geom_bar(stat = "identity", color = "purple") + xlab("city") + ylab("orders") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + ggtitle("city vs orders")

#install.packages("dplyr")
library("dplyr")
# using dplyr function
filter(order_detail_march, Price > 130000) 
summarise(order_detail_march, mean(Price, na.rm = TRUE)) #na.rm removes all null values
summarise(group_by(order_detail_march, vehicle_type), mean(Price, na.rm = TRUE)) #group data besed on vehicle typ


db <- read.csv("ONE.csv")  #contains all data from database for the month of march
summary(db)
ga <- read.csv("TWO.csv")  #contains GA data that matched with db for the month of march
summary(ga)

  
# to combine two files with same header
file1  <- read.csv("HELMETMarch.csv", header=TRUE,sep=",")
file2  <- read.csv("HELMETApril.csv", header=TRUE,sep=",")
COMBINE <- rbind(file1, file2) #combining two files by rows

#library("plyr")
#bind1 <- bind_rows(file1, file2, id=NULL)

# install.packages("gtools")
#library("gtools")
#bind <- smartbind(file1, file2, fill = "None", sep = ',', verbose = FALSE)

library(WriteXLS)
write.csv(COMBINE, file = "helmetdata.csv", append = FALSE, sep=',')
?write.csv


#extract some columns from a file and save in a separate file
file3  <- read.csv("HELMETFeb.csv", header=TRUE,sep=",")
X<-colnames(file3)
typeof(file3)
A=file3[ ,c("oid", "commitment_fee")]  #extracts all rows and oid column
A
write.csv(file3[,c(1:5)], file="helmetdatatest.csv",row.names=FALSE)



