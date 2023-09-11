# Loading required libraries
library(plyr); library(grDevices)
#Loading the main dataset
NEI <- readRDS("summarySCC_PM25.rds")

# Filtering vehicle based missions in Baltimore. SSC source data file shows that all vehicle related emissions are recorded as "ON-ROAD" type in the main data
NEI_total_Balt_motors <- ddply(filter(NEI,fips=="24510", type =="ON-ROAD"), .(year), summarize, total=sum(Emissions))

# Filtering vehicle based missions in LA in the same methodology above
NEI_total_LA_motors <- ddply(filter(NEI,fips=="06037", type =="ON-ROAD"), .(year), summarize, total=sum(Emissions))

# Plotting comparison line chart depicting the emission in both cities
png(filename = "plot6.png", height = 600, width = 600)  
plot(NEI_total_LA_motors, type="l", main=" Compairing Vehicle Emisions in LA and Baltimore ", ylab="PM25 Emission (tonnes)", col="red", ylim=c(0,5000))
lines(NEI_total_Balt_motors, col="blue")
legend("topleft", legend=c("Los Angeles", "Baltimore"),
       col=c("red", "blue"), lty=1:1,)
dev.off()
