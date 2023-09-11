# Exploratory-Data-Analysis-Course-Project-2

#### Plot 1

> QUESTION: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? <br><br>
> Using the <b>base</b> plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

``` r
# Prevents histogram from printing in scientific notation
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
##readData

# Aggregate by sum the total emissions by year
aggTotals <- aggregate(Emissions ~ year,NEI, sum)

png("plot1.png",width=480,height=480,units="px",bg="white")

barplot(
  (aggTotals$Emissions)/10^6,
  names.arg=aggTotals$year,
  xlab="Year",
  ylab="PM2.5 Emissions (10^6 Tons)",
  main="Total PM2.5 Emissions From All US Sources"
)

dev.off()

```
[plot1 (1).png](https://github.com/5jiaxiang/exploratory-data-analysis-course-project-2/blob/main/plot1%20(1).png)


****

#### Plot 2

> QUESTION:Have total emissions from PM2.5 decreased in the <b>Baltimore City</b>, Maryland (fips == "24510"fips == "24510") from 1999 to 2008?<br><br>
> Use the <b>base</b> plotting system to make a plot answering this question.


``` r
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
```
[![plot2](https://github.com/kannbaba/Exploratory-Data-Analysis-Course-Project-2/assets/6490466/337f62d5-c966-4e4a-baa0-ec4359561b3e)](https://github.com/5jiaxiang/exploratory-data-analysis-course-project-2/blob/main/plot2%20(1).png)



****

#### Plot 3
> QUESTION: Of the four types of sources indicated by the typetype (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? <br><br>
> Use the ggplot2 plotting system to make a plot answer this question.
``` r
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
##readData

SubNEI<-NEI[NEI$fips=="24510",]
##subset only a Baltimore City data

png("plot3.png",width=480,height=480,units="px",bg="transparent")

library(ggplot2)

ggp <- ggplot(SubNEI,aes(factor(year),Emissions,fill=type)) +
  geom_bar(stat="identity") +
  theme_bw() + guides(fill=FALSE)+
  facet_grid(.~type,scales = "free",space="free") + 
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))
## let facet_grid do the trick for type
print(ggp)

dev.off()
```[
[![plot3](https://github.com/kannbaba/Exploratory-Data-Analysis-Course-Project-2/assets/6490466/f1fd9d7f-a599-456e-9f28-5e95551c6d26)](https://github.com/5jiaxiang/exploratory-data-analysis-course-project-2/blob/main/plot3%20(1).png)](https://github.com/5jiaxiang/exploratory-data-analysis-course-project-2/blob/main/plot3%20(1).png)


****

#### Plot 4
> QUESTION:Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

``` r
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
```
[![plot4](https://github.com/kannbaba/Exploratory-Data-Analysis-Course-Project-2/assets/6490466/c68433d7-757b-412a-8d58-f514ada2f788)
](https://github.com/5jiaxiang/exploratory-data-analysis-course-project-2/blob/main/plot4%20(1).png)



****

#### Plot 5

> QUESTION:How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?


``` r
source("downloadArchive.R")

# Load the NEI & SCC data frames.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Gather the subset of the NEI data which corresponds to vehicles
vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]

# Subset the vehicles NEI data to Baltimore's fip
baltimoreVehiclesNEI <- vehiclesNEI[vehiclesNEI$fips=="24510",]

png("plot5.png",width=480,height=480,units="px",bg="transparent")

library(ggplot2)

ggp <- ggplot(baltimoreVehiclesNEI,aes(factor(year),Emissions)) +
  geom_bar(stat="identity",fill="grey",width=0.75) +
  theme_bw() +  guides(fill=FALSE) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999-2008"))

print(ggp)

dev.off()
```
[![plot5](https://github.com/kannbaba/Exploratory-Data-Analysis-Course-Project-2/assets/6490466/6f3f4456-d59b-4eaf-9953-94a353bd5e55)](https://github.com/5jiaxiang/exploratory-data-analysis-course-project-2/blob/main/plot5.png)


****

#### Plot6

> QUESTION: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

``` r
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
```
[![plot6](https://github.com/kannbaba/Exploratory-Data-Analysis-Course-Project-2/assets/6490466/d29ba447-9022-4793-99b4-e30310266cb9)
](https://github.com/5jiaxiang/exploratory-data-analysis-course-project-2/blob/main/plot6.png)
