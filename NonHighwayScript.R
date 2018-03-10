library(dplyr)

analysis.year <- 2017
ag.census.year <- 2012

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
retail.gas.all <- merge(x=state.padd.fips,y=retail.gas.big.states[c("STATE","ppg.2012","ppg.2016")], by="STATE",all.x = TRUE)

#subset based on whether EIA provides report for particular states
retail.gas.report <- subset(retail.gas.all, is.na(retail.gas.all$ppg.2016)==FALSE)
retail.gas.impute <- subset(retail.gas.all, is.na(retail.gas.all$ppg.2016)==TRUE)

#remove NA columns to avoid duplication
retail.gas.impute$ppg.2012 <- NULL
retail.gas.impute$ppg.2016 <- NULL

#substitute a state's price per gallon with that of it's PADD
retail.gas.impute <- merge(x=retail.gas.impute,y=retail.gas.padd[c("PADD","ppg.2012","ppg.2016")], by="PADD", all = TRUE)

#bring all the states back together
retail.gas.all <- rbind(retail.gas.impute,retail.gas.report)

#remove tables that are not likely to be needed later
rm(retail.gas.big.states,retail.gas.impute,retail.gas.padd,retail.gas.report)

#### Agriculture Values ##############################
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
ay.ag.fe.region <- subset(ag.region, Year == analysis.year - 1)
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
ag.state.all <- merge(x=ag.state.all ,y=retail.gas.all[c("STATE","ppg.2012","ppg.2016")], by.x= "State", by.y = "STATE", all.x = TRUE)

ag.state.all$agall <- ag.state.all$total.gas.s/ag.state.all$ppg.2016

#Bring in EPA Agricultural Equipment numbers
ag.state.all <- merge(x=ag.state.all , y=EPA[c("State","Agricultural.Equipment")], by= "State", all.x = TRUE)

#Perform calculations to get off highway number for agriculture
ag.state.all <- ag.state.all %>% mutate(truck.on = (agall - Agricultural.Equipment) * vius$ONVMT.Truck) %>% mutate(AG.OFF.HWY = agall - truck.on)

offhighway.model <- ag.state.all[,c("State","AG.OFF.HWY")]

#remove tables except for final ag numbers
rm(ag.state.all,ag.fe.15.states,ag.region,ag.census.year,ag.state.impute,ag.state.report,census.ag.fe.15.states,census.ag.fe.region,ay.ag.fe.15.states,ay.ag.fe.region,control.total)

write.csv(offhighway.model,"offhighwayscript.csv")
#### Aviation Values ############################