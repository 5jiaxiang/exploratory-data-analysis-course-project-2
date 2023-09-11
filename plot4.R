## Loading required libraries
library(plyr); library(grDevices)

#Loading the main dataset
NEI <- readRDS("summarySCC_PM25.rds")

#Loading the main dataset
NEI <- readRDS("summarySCC_PM25.rds")
#Loading the source dataset in order to filter "coal related" entries
SCC <- readRDS("Source_Classification_Code.rds")
#Creating coal filter by selecting all entries which ends with "Coal" at EI.Sector column
NEI_coal <- SCC %>% filter(grepl("Coal$",EI.Sector))
# Filtering main data with code ids from filtered source database
NEI_total_coal <- ddply( filter(NEI, str_detect(NEI$SCC, paste(NEI_coal[,1], collapse="|"))), .(year), summarize, total=sum(Emissions))


#Plotting a bar chart to show coal emissions for each year
png(filename = "plot4.png", height = 600, width = 600)  
p <- barplot( height =NEI_total_coal[,2]/1000, name= NEI_total_coal[,1],
               main=" Total PM2.5 Emissions in the U.S. due to coal sources ", ylab="PM25 Emission (tonnes) ", xlab="year", ylim=c(0,600)
               )
text ( x=p , y=NEI_total_coal[,2] , label=format(round(NEI_total_coal[,2],1)) , pos=1)
dev.off()
