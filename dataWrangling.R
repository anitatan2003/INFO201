library(stringr)
library(dplyr)

#Sources Cited: Stack Overflow

medHouseIncome <- read.csv("MEHOINUSA646N.csv")
snapHist <- read.csv("SNAP_history_1969_2019.csv")
gap <- read.csv(file="gapminder.csv", head = TRUE, sep="\t")

#get year
unqdates <- medHouseIncome$DATE
#create numerical year variable extracting and containing only the year
medHouseIncome$year <- substring(unqdates, 0, 4)

#fix 1982 year issue
snapHist$Fiscal.Year[snapHist$Fiscal.Year == "1982 3]"] <- 1982

#join data sets
df <- left_join(x=snapHist, y=medHouseIncome, by = c("Fiscal.Year"= "year"), all.x == TRUE)

#get data for years 1984 and up
df <- filter(df, Fiscal.Year >= 1984)

#get rid of extra date column
df <- df[, -7]

#work with gap data set 
gap <- filter(gap, name == "United States of America")
gap <- select(gap, time, totalPopulation, cerealProduction, GDP_PC, lifeExpectancy)

#join gap and df
df$Fiscal.Year <- as.integer(df$Fiscal.Year)
df <- left_join(x=df, y= gap, by= c("Fiscal.Year"="time"), all.x == TRUE)
df <- df[, c(-5, -6)]

#create categorical column of place 
df <- mutate(df, Place = "United States")


#Summary data frame
#1984-2000
fhalf <- df[df$Fiscal.Year >= 1984 & df$Fiscal.Year <= 2000,]
df$Average.Participation <- as.integer(gsub(",", "", df$Average.Participation))

fhalf <- summarise (fhalf, snap = sum(df$Average.Participation), benefitper = sum(Average.Benefit.Per.Person), 
                    houseVal = sum(MEHOINUSA646N), avgpop = sum(totalPopulation), 
                    avgcerealprod = sum(cerealProduction), gdp = sum(GDP_PC),
                    lifexpect = sum (lifeExpectancy))


#2000-2019
shalf <- df[df$Fiscal.Year >= 2000,]
shalf <- filter(shalf, !is.na(cerealProduction))
shalf <- summarise (shalf, snap = sum(Average.Participation), benefitper = sum(Average.Benefit.Per.Person), 
              houseVal = sum(MEHOINUSA646N), avgpop = sum(totalPopulation), 
              avgcerealprod = sum(cerealProduction), gdp = sum(GDP_PC),
              lifexpect = sum (lifeExpectancy))

#make data frame cols
YearRange <- c("1984-2000", "2000-2019")
avgsnap <- c(fhalf$snap, shalf$snap)
avgSnapBenefitPerPerson <- c(fhalf$benefitper, shalf$benefitper)
avgMedianHouseVal <-c(fhalf$houseVal, shalf$houseVal)
avgPop <-c(fhalf$avgpop, shalf$avgpop)
avgCerealProduct <-c(fhalf$avgcerealprod, shalf$avgcerealprod)
avgGDP <-c(fhalf$gdp, shalf$gdp)
avgLifeExpectancy <-c(fhalf$lifexpect, shalf$lifexpect)

#summary data frame
summarydf <- data.frame(YearRange, avgsnap, avgSnapBenefitPerPerson,
                        avgMedianHouseVal, avgPop, avgCerealProduct,
                        avgGDP, avgLifeExpectancy)

df
colnames(df) <- c("Year",
                  "AVG SNAP Participation",
                  "AVG SNAP Benefit per Person",
                  "Total SNAP Benefits",
                  "Median Household Value",
                  "Population",
                  "Cereal Production",
                  "GDP_PC",
                  "Life Expectancy",
                  "Place")

# Write the modified data frame back to a CSV file
write.csv(df, "finalDataset2.csv")




