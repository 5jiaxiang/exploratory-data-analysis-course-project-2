NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
##readData

SubNEI<-subset(NEI,fips=="24510")
aggNEI<-aggregate(Emissions~year,SubNEI,sum)
##subset only year and emission and sum emissions of the same year together

png("plot2.png",width=480,height=480,units="px",bg="transparent")

barplot((aggNEI$Emissions)/10^2,names.arg = aggNEI$year,
        xlab="year",ylab="emissions(10^2 tons)",main="Total Emissions of PM 2.5 in Baltimore City ")
##PLOT

dev.off()
