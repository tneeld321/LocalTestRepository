# Package for Oracle SQL Connection
#install.packages('RJDBC')
library('RJDBC')

# Load data from SQL View
drv <- JDBC("oracle.jdbc.driver.OracleDriver","C:/Users/tneeld/Desktop/ojdbc6.jar",identifier.quote="`")

conn.ecreport_mc2 <- dbConnect(drv, "jdbc:oracle:thin:@ecnext172.ecnext.com:1521:mtrpt", "ecreport_mc2", "ecreport_mc2")

data <- dbGetQuery(conn.ecreport_mc2,"select * from TOTAL_MONTH_CLAIM_PBL");
#save(data,"data_temp.rdat")
#load("data_temp.rdat")

dbDisconnect(conn.ecreport_mc2)

# Package to write SQL queries in R - not currently being used
#install.packages('sqldf')
#library(sqldf)

# Load monthly budget from local file
monthly_budget <- read.csv("Budget.csv",header=TRUE)
monthly_budget

# Package for data manipulation
#install.packages('zoo')
library('zoo')

# Figured fields
data$YEARMON <- as.yearmon(paste(data$MONTH,data$YEAR), "%m %Y")
monthly_budget$DateAction <- as.Date(monthly_budget$Date,"%m/%d/%Y")
monthly_budget$MonYear <- as.yearmon(monthly_budget$DateAction, "%m %Y")
data$MonthYearDate <- as.Date(data$MonYear)

# Clean up names
names(data)[c(3,4,5,6,7,8,9,10,11,12,13)] <- c("Claims","PBLs","Registrants","PBL_New_Online","PBL_New_BusCoach","PBL_Overall_Cancels","PBL_Online_Cancels","PBL_New_Revenue","PBL_Renewal","PBL_Renewal_Revenue","MonYear")

# Replace NAs with 0
data[is.na(data)] <- 0 

# Additional figured fields
data$sumclaim <- cumsum(data$Claims)
data$PBL_CustServ_Cancels <- data$PBL_Overall_Cancels - data$PBL_Online_Cancels
data$PBL_OverallRevenue <- data$PBL_New_Revenue + data$PBL_Renewal_Revenue


# Merge data with budget numbers
data_exper <- merge(x=data,y=monthly_budget, by.x = "MonYear", by.y = "MonYear")

# Time to melt and cast
#install.packages('reshape')
library('reshape')

data$MonthYearDate <- as.Date(data$MonYear)

data.reshaped <- melt(data,id="MonthYearDate")
names(data.reshaped)[c(2,3)] <- c("Variable","Value")

# Plotting time
#install.packages('ggplot2')
library('ggplot2')

#install.packages('scales')
library(scales)


# p <- ggplot(subset(data.reshaped,DATE_ACTION>Sys.Date()-30),aes(x=DATE_ACTION,y=Value,group=Variable)) +
#   geom_line(aes(color=Variable)) +
#   geom_point(aes(color=Variable)) +
#   ggtitle("Daily Update: Claims, PBLs, and Member Counts \n") +
#   scale_y_continuous(labels = comma) +
#   xlab("") +
#   ylab("")
# p

# Plot PBL Revenue
p <- ggplot(subset(data.reshaped,data.reshaped[, 2]=='PBL_OverallRevenue' & data.reshaped[, 3]>0),aes(x=MonthYearDate,y=Value,group=Variable)) +
  geom_line(aes(color=Variable)) +
  geom_point(aes(color=Variable)) +
  ggtitle("Daily Update: Claims, PBLs, and Member Counts \n") +
  scale_y_continuous(labels = comma) +
  xlab("") +
  ylab("")
p

#install.packages('googleVis')
library('googleVis')

to.table <- cast(subset(data.reshaped,data.reshaped[, 2]=='PBL_OverallRevenue' & data.reshaped[, 3]>0),MonthYearDate~Variable)
plot(gvisTable(to.table))

Line <- gvisLineChart(to.table)
plot(Line)
