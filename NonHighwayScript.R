library(dplyr)

analysis.year <- 2017

#### Global Variables and Functions ###################
EPA <- read.csv("Unknown Update/EPA Results.csv", header = TRUE)

vius <- read.csv("VIUS Ratios.csv", header = TRUE)

state.padd.fips <- read.csv("StatePaddFips.csv", header=TRUE)

#leapyear function from r-bloggers
is.leapyear=function(year){
      #http://en.wikipedia.org/wiki/Leap_year
      return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

## Gasoline Prices for the States #####################
retail.gas.big.states <- read.csv("1 Year Update/Retail Gasoline.csv", header = TRUE)
retail.gas.padd <- read.csv("1 Year Update/PADD Gasoline Data.csv", header = TRUE)

#cretate table with all states that inlcudes ppg columns
retail.gas.all <- merge(x=state.padd.fips,y=retail.gas.big.states[c("State","Year","ppg")], by="State", all.x = TRUE)

#subset based on whether EIA provides report for particular states
retail.gas.report <- subset(retail.gas.all, is.na(retail.gas.all$ppg)==FALSE)
retail.gas.impute <- subset(retail.gas.all, is.na(retail.gas.all$ppg)==TRUE)

#remove NA columns to avoid duplication and we are going to get these values from the PADD Data
retail.gas.impute$ppg <- NULL
retail.gas.impute$Year <- NULL

#substitute a state's price per gallon with that of it's PADD
retail.gas.impute <- merge(x=retail.gas.impute,y=retail.gas.padd[c("PADD", "Year","ppg")], by="PADD", all = TRUE)

#bring all the states back together
retail.gas.all <- rbind(retail.gas.impute,retail.gas.report)

#remove tables that are not likely to be needed later
rm(retail.gas.big.states,retail.gas.impute,retail.gas.padd,retail.gas.report)

#### Agriculture Values ##############################

ag.data.year <- 2016
ag.census.year <- 2012

#Read in the tables
# note: The Five year USDA census contains all the states and will be 
# used to build out all the calculated columns
ag.state.all <- read.csv("FiveYearUpdate/2012AgCensus.csv", header=TRUE)
ag.region <- read.csv("1 Year Update/Ag Fuel Exp by Fuel-Region.csv", header = TRUE)
ag.fe.15.states <- read.csv("1 Year Update/Ag Fuel Expenditures State.csv", header = TRUE)


#Calucate Regional Percentage Field for all years
ag.region$percent.gas.r <- ag.region$Gasoline/ag.region$Total

#Subet Region and top 15 states for census and analysis year
census.ag.fe.region <- subset(ag.region, Year == ag.census.year)
ay.ag.fe.region <- subset(ag.region, Year == ag.data.year)
census.ag.fe.15.states <- subset(ag.fe.15.states, Year == ag.census.year)
ay.ag.fe.15.states <- subset(ag.fe.15.states, Year == analysis.year-1)


#Store the value of US Fuel consumption for the analysis year from the ay region table
control.total <- ay.ag.fe.region$Total[ay.ag.fe.region$Region=="United States"]

#Bring in the state's region's percent gas 
ag.state.all <- merge(x=ag.state.all,y=ay.ag.fe.region[c("Region","percent.gas.r")], by ="Region", all.x=TRUE)

#Calculates an individual state's percent contritubution to total fuela and oil agricultural spend
ag.state.all$percent.total.s <- ag.state.all$CY.FUELS.AND.OILS.EXPENSE/sum(ag.state.all$CY.FUELS.AND.OILS.EXPENSE)

# If a state is in the top 15 and has a reported fuel exp for the analysis year
# then it is added to the table 
ag.state.all <- merge(x=ag.state.all,y=ay.ag.fe.15.states[c("Region","ay.fe")], by.x ="State",by.y="Region",all.x = TRUE)

# This splits up the states with a reported FE from those states whose value will be imputed
ag.state.impute <- subset(ag.state.all, is.na(ag.state.all$ay.fe)==TRUE)
ag.state.report <- subset(ag.state.all, is.na(ag.state.all$ay.fe)==FALSE)

## Calculating FE for states in the analysis year ##

#Performs calculations to get to the 'adjusted' column in the excel file
ag.state.impute <- ag.state.impute %>% mutate(ay.fe = percent.total.s*control.total) %>% mutate(ay.fe = ay.fe + (ay.fe/sum(ay.fe))*(control.total - (sum(ay.fe)+sum(ag.state.report$ay.fe))))

#Bring all of our states back together for the rest of the calc
ag.state.all <- rbind(ag.state.impute,ag.state.report)

#Equivalent to Total Gasoline & Gasohol Expenses in 
#Analysis Year ($1,000) in excel file
ag.state.all$total.gas.s <- ag.state.all$ay.fe*ag.state.all$percent.gas.r

#Bring gas prices into ag table
ag.state.all <- merge(x=ag.state.all ,y=retail.gas.all[c("State","Year","ppg")], by = "State", all.x = TRUE)

# We only want to perform our calculations on the current data for the analysis year
ag.state.all <- subset(ag.state.all, ag.state.all$Year==analysis.year-1)

ag.state.all$agall <- ag.state.all$total.gas.s/ag.state.all$ppg

#Bring in EPA Agricultural Equipment numbers
ag.state.all <- merge(x=ag.state.all , y=EPA[c("State","Agricultural.Equipment")], by= "State", all.x = TRUE)

#Perform calculations to get off highway number for agriculture
ag.state.all <- ag.state.all %>% mutate(truck.on = (agall - Agricultural.Equipment) * vius$ONVMT.Truck) %>% mutate(Agriculture = agall - truck.on)

off.highway.model <- ag.state.all[,c("State","Agriculture")]


#remove tables except for final ag numbers
rm(ag.state.all,ag.fe.15.states,ag.region,ag.state.impute,ag.state.report,census.ag.fe.15.states,census.ag.fe.region,ay.ag.fe.15.states,ay.ag.fe.region,control.total)

#### Aviation Values ############################

aviation.data.year <- 2016

aviation.state <- read.csv("1 Year Update/Aviation Data.csv", header = TRUE)
aviation.padd <- read.csv("1 Year Update/PADD Aviation.csv", header = TRUE)

#Get the PADD for each State
aviation.state <- merge(x = state.padd.fips[c("PADD","State")],y = aviation.state, by = "State", all.y = TRUE)

#Calculate the volume of aviation in thousands of gallons per year (tgy) taking into account whether or not the data comes from a leap year
aviation.state <- aviation.state %>% mutate(aviation.tgy.s = ifelse(is.leapyear(YEAR),aviation.tgd.s*366,aviation.tgd.s*365)) 

aviation.padd <- aviation.padd %>% mutate(aviation.tgy.p = ifelse(is.leapyear(YEAR),aviation.tgd.p*366,aviation.tgd.p*365)) 

#Sum up hours and thousand gallons year for each PADD and put it in the aviation.padd table, note: using most recent year hours, for 2016 it appears FAA changed it's numbers so 2015's numbers have been substituted for 2016

aviation.padd.sum <- aggregate(hours ~ YEAR + PADD, aviation.state, sum)

aviation.padd.sum <- merge(x = aviation.padd.sum, y = aviation.padd[c("PADD","YEAR","aviation.tgy.p")], by = c("PADD","YEAR"), all.x = TRUE )

tempsum <- aggregate(aviation.tgy.s ~ YEAR + PADD, aviation.state, sum)

aviation.padd.sum <- aviation.padd.sum %>% mutate(tot.tgy.s = tempsum$aviation.tgy.s) %>% mutate(remainder = aviation.tgy.p - tot.tgy.s)

rm(tempsum)

#Subset the state aviation data to get the states we need to impute the tgy number

aviation.state.impute <- subset(aviation.state, is.na(aviation.state$aviation.tgy.s) == TRUE)
aviation.state.report <- subset(aviation.state, is.na(aviation.state$aviation.tgy.s) == FALSE)

aviation.state.impute$aviation.tgy.s <- NULL

# Below are the calculations, merges, and subsets to calculate the values for states with missing yearly aviation figures

aviation.missing <- aggregate(hours ~ YEAR + PADD, aviation.state.impute, sum)

aviation.state.impute1 <- merge(x = aviation.state.impute, y = aviation.missing, by = c("PADD","YEAR"), all.x= TRUE)

aviation.state.impute1 <- aviation.state.impute1 %>% mutate(percent.missing = hours.x / hours.y)

aviation.state.impute1 <- merge(x = aviation.state.impute1, y=aviation.padd.sum[c("PADD","YEAR","remainder")], by = c("PADD","YEAR"), all.x = TRUE)

aviation.state.impute1 <- aviation.state.impute1 %>% mutate(aviation.tgy.s = percent.missing * remainder)

aviation.state.impute <- merge(x = aviation.state.impute, y = aviation.state.impute1[c("State", "PADD","YEAR","aviation.tgy.s")], by = c("State", "PADD", "YEAR"), all.x = TRUE)

#put everything together and output the numbers for the off highway model 
aviation.state <- rbind(aviation.state.impute,aviation.state.report)

aviation.offhwy <- filter(aviation.state[c("State","aviation.tgy.s")], aviation.state$YEAR == aviation.data.year)

aviation.offhwy <- arrange(aviation.offhwy,State)

names(aviation.offhwy)[names(aviation.offhwy)=="aviation.tgy.s"] <- "Aviation"

off.highway.model <- merge(off.highway.model, aviation.offhwy, by="State")

#remove all the tables we don't need going forward
rm(aviation.missing, aviation.padd, aviation.padd.sum, aviation.state, aviation.state.impute, aviation.state.impute1, aviation.state.report, aviation.offhwy)

#### Recreational Boating Values #################

boat.data.year <- 2016
NRBS.survey.year <- 2012

DPI <- read.csv("1 Year Update/BEA DPI.csv", header = TRUE)
boating <- read.csv("1 Year Update/State Rec Boating.csv", header = TRUE)
boat.types <- read.csv("Unknown Update/Boat MPG.csv", header = TRUE)

boating <- subset(boating, boating$Year == boat.data.year)

boat.types <- subset(boat.types, boat.types$Year == NRBS.survey.year)

#VERY IMPORTANT make sure everything is arranged correctly for the next operations
retail.gas.all <- arrange(retail.gas.all, State, Year)
DPI <- arrange(DPI, State, Year)
boating <- arrange(boating, State, Year)

#get the value of boat.data.year gas prices divided by NRBS.survey.year gas prices
gas.growth <- retail.gas.all$ppg[retail.gas.all$Year == boat.data.year]/retail.gas.all$ppg[retail.gas.all$Year == NRBS.survey.year]

#same for DPI
dpi.growth <- DPI$dpi[DPI$Year == boat.data.year]/DPI$dpi[DPI$Year == NRBS.survey.year]

#Finally bring everything together, and determine the growth factor
boating <- cbind(boating, gas.growth, dpi.growth)

boating$growth.rate <- boating$dpi.growth/boating$gas.growth

#Get the unadjusted gasoline numbers

boat.types <- merge(x = boat.types, y=boating[c("State","Registered.Boats","Percent.Gasoline.Boats","growth.rate")], by = "State", all.x = TRUE)

#Get the offhighway numbers

boat.types <- boat.types %>% mutate(number.boats = Registered.Boats * Percent.Gasoline.Boats * Percent.State.Total) %>% mutate(unadjusted.gallons = (number.boats * Gallon.Boat)/1000) %>% mutate(Boating = unadjusted.gallons * growth.rate) 

boating.offhwy <- aggregate(Boating ~ State, boat.types, sum)

off.highway.model <- merge(off.highway.model,boating.offhwy, by = "State")

#remove the intermediatary tables
rm(boat.types,boating,dpi.growth, gas.growth, DPI, boating.offhwy)
