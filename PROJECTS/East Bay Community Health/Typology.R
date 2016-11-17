setwd('I:\\A-Lydia-Work-Plan\\Projects\\CH-Dashboard\\UtilizationTypology')


#http://stackoverflow.com/questions/13706188/importing-csv-file-into-r-numeric-values-read-as-characters
# Imports your data as character columns.
# Creates an instance of your data as numeric columns.
# Identifies which columns from your data are numeric (assuming columns with less than 50% NAs upon converting your data to numeric are indeed numeric).
# Merging the numeric and character columns into a final dataset.

num_data <- data.frame(data.matrix(data))
numeric_columns <- sapply(num_data, function(x)
                                               {mean(as.numeric(is.na(x)))<0.5})
final_data <- data.frame(num_data[,numeric_columns], data[,!numeric_columns])


SummitFiles <- list.files(pattern="Summit.*csv")
SummitFiles
for (i in 1:length(SummitFiles)){
    data <- read.csv(SummitFiles[i], header=T, stringsAsFactors = F)
    
    num_data <- data.frame(data.matrix(data))
    numeric_columns <- sapply(num_data, function(x){mean(as.numeric(is.na(x)))<0.5})
    final_data <- data.frame(num_data[,numeric_columns], data[,!numeric_columns])
    
    gm_mean = function(x, na.rm=TRUE){
        exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
    }
   sLOS <- summary(final_data$IP_LOS_DAYS)
   gLOS <- gm_mean(final_data$IP_LOS_DAYS)
        
    sDRG<-summary(final_data$DRG_EXCESS_GEOMETRIC_MEAN)
    gDRG<-gm_mean(final_data$DRG_EXCESS_GEOMETRIC_MEAN)
        
    sPayLOS<- summary(final_data$PAY_EXCESS_AVG_LOS_G_MEAN)
    gPayLOS<- gm_mean(final_data$PAY_EXCESS_AVG_LOS_G_MEAN)    
    
    print(sLOS)
    print(gLOS)
    print(sDRG)
    print(gDRG)
    print(sPayLOS)
    print(gPayLOS)
    
}

##*****************************************************************************
#data <- read.csv('Ashby.csv', header=T, stringsAsFactors = F)
#data <- read.csv('Summit.csv', header=T, stringsAsFactors = F)
#data <- read.csv('Ashby_year.csv', header=T, stringsAsFactors = F)
data <- read.csv('Summit_year.csv', header=T, stringsAsFactors = F)
#summary(data)
num_data <- data.frame(data.matrix(data))
numeric_columns <- sapply(num_data, function(x){mean(as.numeric(is.na(x)))<0.5})
final_data <- data.frame(num_data[,numeric_columns], data[,!numeric_columns])
class(final_data)

library(dplyr)
gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
    }


f <- group_by(final_data, CNT_ED_IP_ENCS_GROUPED)%>%summarise(N = n(),
                                Geometric_LOS = gm_mean(IP_LOS_DAYS), 
 
                                xsLOS = gm_mean(DRG_EXCESS_GEOMETRIC_MEAN),

                              xsCOST = gm_mean(PAY_EXCESS_AVG_LOS_G_MEAN)

                              )
                         
#write.csv(f, file = "ashby3Q.csv")
# write.csv(f, file = "summit3Q.csv")
#write.csv(f, file = "ashby1yr.csv")
write.csv(f, file = "summit1yr.csv")
