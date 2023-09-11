
## Loading required libraries
library(plyr); library(grDevices)

#Loading the main dataset
NEI <- readRDS("summarySCC_PM25.rds")
# Obtaining summary dataset for Baltimore by getting the total emissions for each yeari using fips=="24510" argument
NEI_sum_Balt <- ddply(filter(NEI,fips=="24510"), .(year), summarize, sum=sum(Emissions))

# plotting a barchart for total emissions in Baltimore for each year
png(filename = "plot2.png", height = 600, width = 600)  
p <- barplot(height=NEI_sum_Balt[,2], name=NEI_sum[,1],
             main=" Total PM2.5 emission from all sources in Baltimore ", ylab="PM25 Emission", xlab="year", ylim=c(0,4000))
text (x=p,y=NEI_sum_Balt[,2], label=format(round(NEI_sum_Balt[,2],1)),pos=1)
dev.off()      
