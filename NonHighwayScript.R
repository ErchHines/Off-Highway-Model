library(dplyr)
library(data.table)

analysis.year <- 2017

#### Global Variables and Functions ###################
EPA <- read.csv("Unknown Update/EPA Results.csv", header = TRUE)

vius <- read.csv("VIUS (2002).csv", header = TRUE)

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
retail.gas.all <- merge(
      x=state.padd.fips,
      y=retail.gas.big.states[c("State","Year","ppg")], 
      by="State", all.x = TRUE
)

#subset based on whether EIA provides report for particular states
retail.gas.report <- subset(retail.gas.all, is.na(retail.gas.all$ppg)==FALSE)
retail.gas.impute <- subset(retail.gas.all, is.na(retail.gas.all$ppg)==TRUE)

#remove NA columns to avoid duplication and we are going to 
#get these values from the PADD Data
retail.gas.impute$ppg <- NULL
retail.gas.impute$Year <- NULL

#substitute a state's price per gallon with that of it's PADD
retail.gas.impute <- merge(
      x=retail.gas.impute,
      y=retail.gas.padd[c("PADD", "Year","ppg")],
      by="PADD", all = TRUE
)

#bring all the states back together
retail.gas.all <- rbind(retail.gas.impute,retail.gas.report)

#remove tables that are not likely to be needed later
rm(retail.gas.big.states,retail.gas.impute,retail.gas.padd,retail.gas.report)

#### Agriculture Values ##############################

ag.data.year <- 2016
ag.census.year <- 2012

#Remove DC from our VIUS table
ag.vius <- subset(vius, !(vius$State == "District of Columbia"))

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
ag.state.all <- merge(
      x=ag.state.all,y=ay.ag.fe.region[c("Region",
      "percent.gas.r")], by ="Region", all.x=TRUE
)

#Calculates an individual state's percent contritubution to total fuel
#and oil agricultural spend
ag.state.all$percent.total.s <- ag.state.all$CY.FUELS.AND.OILS.EXPENSE/
      sum(ag.state.all$CY.FUELS.AND.OILS.EXPENSE)

# If a state is in the top 15 and has a reported fuel exp for the analysis year
# then it is added to the table 
ag.state.all <- merge(
      x=ag.state.all,y=ay.ag.fe.15.states[c("Region","ay.fe")], 
      by.x ="State",by.y="Region",all.x = TRUE
)

# This splits up the states with a reported FE from those states 
# whose value will be imputed
ag.state.impute <- subset(ag.state.all, is.na(ag.state.all$ay.fe)==TRUE)
ag.state.report <- subset(ag.state.all, is.na(ag.state.all$ay.fe)==FALSE)

## Calculating FE for states in the analysis year ##

#Performs calculations to get to the 'adjusted' column in the excel file
ag.state.impute <- ag.state.impute %>% mutate(
      ay.fe = percent.total.s*control.total,
      ay.fe = ay.fe + (ay.fe/sum(ay.fe))*(control.total - 
            (sum(ay.fe)+sum(ag.state.report$ay.fe)))
)

#Bring all of our states back together for the rest of the calc
ag.state.all <- rbind(ag.state.impute,ag.state.report)

#Equivalent to Total Gasoline & Gasohol Expenses in 
#Analysis Year ($1,000) in excel file
ag.state.all$total.gas.s <- ag.state.all$ay.fe*ag.state.all$percent.gas.r

#Bring gas prices into ag table
ag.state.all <- merge(
      x=ag.state.all ,y=retail.gas.all[c("State","Year","ppg")], 
      by = "State", all.x = TRUE
)

# We only want to perform our calculations on the current data for the analysis year
ag.state.all <- subset(ag.state.all, ag.state.all$Year==analysis.year-1)

ag.state.all$agall <- ag.state.all$total.gas.s/ag.state.all$ppg

#Bring in EPA Agricultural Equipment numbers
ag.state.all <- merge(
      x=ag.state.all , y=EPA[c("State","Agricultural.Equipment")], 
      by= "State", all.x = TRUE
)

#Perform calculations to get off highway number for agriculture
ag.state.all <- ag.state.all %>% mutate(
      truck.on = (agall - Agricultural.Equipment) * ag.vius$ONVMT.Truck, 
      Agriculture = agall - truck.on
)

off.highway.model <- ag.state.all[,c("State","Agriculture")]


#remove tables except for final ag numbers
rm(ag.state.all,ag.fe.15.states,ag.region,ag.state.impute,
      ag.state.report,census.ag.fe.15.states,census.ag.fe.region,
      ay.ag.fe.15.states,ay.ag.fe.region,control.total)

#### Aviation Values ############################

aviation.data.year <- 2016

aviation.state <- read.csv("1 Year Update/Aviation Data.csv", header = TRUE)
aviation.padd <- read.csv("1 Year Update/PADD Aviation.csv", header = TRUE)

#Get the PADD for each State
aviation.state <- merge(
      x = state.padd.fips[c("PADD","State")],y = aviation.state, 
      by = "State", all.y = TRUE
)

#Calculate the volume of aviation in thousands of gallons per year 
#(tgy) taking into account whether or not the data comes from a leap year
aviation.state <- aviation.state %>% mutate(
      aviation.tgy.s = 
      ifelse(is.leapyear(YEAR),aviation.tgd.s*366,aviation.tgd.s*365)
) 

aviation.padd <- aviation.padd %>% mutate(
      aviation.tgy.p = ifelse(
            is.leapyear(YEAR),aviation.tgd.p*366,
            aviation.tgd.p*365
      )
) 

#Sum up hours and thousand gallons year for each PADD and put it in 
#the aviation.padd table, note: using most recent year hours, 
#for 2016 it appears FAA changed it's numbers so 2015's numbers 
#have been substituted for 2016

aviation.padd.sum <- aggregate(hours ~ YEAR + PADD, aviation.state, sum)

aviation.padd.sum <- merge(
      x = aviation.padd.sum, y = aviation.padd[c("PADD","YEAR","aviation.tgy.p")],
      by = c("PADD","YEAR"), all.x = TRUE 
)

tempsum <- aggregate(aviation.tgy.s ~ YEAR + PADD, aviation.state, sum)

aviation.padd.sum <- aviation.padd.sum %>% mutate(
      tot.tgy.s = tempsum$aviation.tgy.s,
      remainder = aviation.tgy.p - tot.tgy.s
)

rm(tempsum)

#Subset the state aviation data to get the states we need to impute the tgy number

aviation.state.impute <- subset(
      aviation.state, is.na(aviation.state$aviation.tgy.s) == TRUE
)
aviation.state.report <- subset(
      aviation.state, is.na(aviation.state$aviation.tgy.s) == FALSE
)

aviation.state.impute$aviation.tgy.s <- NULL

# Below are the calculations, merges, and subsets to calculate the values for 
# states with missing yearly aviation figures

aviation.missing <- aggregate(hours ~ YEAR + PADD, aviation.state.impute, sum)

aviation.state.impute1 <- merge(
      x = aviation.state.impute, y = aviation.missing, 
      by = c("PADD","YEAR"), all.x= TRUE
)

aviation.state.impute1 <- aviation.state.impute1 %>% mutate(
      percent.missing = hours.x / hours.y
)

aviation.state.impute1 <- merge(
      x = aviation.state.impute1, y=aviation.padd.sum[c("PADD","YEAR","remainder")], 
      by = c("PADD","YEAR"), all.x = TRUE
)

aviation.state.impute1 <- aviation.state.impute1 %>% mutate(
      aviation.tgy.s = percent.missing * remainder
)

aviation.state.impute <- merge(
      x = aviation.state.impute, 
      y = aviation.state.impute1[c("State", "PADD","YEAR","aviation.tgy.s")], 
      by = c("State", "PADD", "YEAR"), all.x = TRUE
)

#put everything together and output the numbers for the off highway model 
aviation.state <- rbind(aviation.state.impute,aviation.state.report)

aviation.offhwy <- filter(
      aviation.state[c("State","aviation.tgy.s")], 
      aviation.state$YEAR == aviation.data.year
)

aviation.offhwy <- arrange(aviation.offhwy,State)

names(aviation.offhwy)[names(aviation.offhwy)=="aviation.tgy.s"] <- "Aviation"

off.highway.model <- merge(
      off.highway.model, aviation.offhwy, 
      by="State", all.y = TRUE
)

#remove all the tables we don't need going forward
rm(aviation.missing, aviation.padd, aviation.padd.sum, 
      aviation.state, aviation.state.impute, aviation.state.impute1, 
      aviation.state.report, aviation.offhwy)

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
gas.growth <- retail.gas.all$ppg[retail.gas.all$Year == 
      boat.data.year]/retail.gas.all$ppg[retail.gas.all$Year == NRBS.survey.year]

#same for DPI
dpi.growth <- DPI$dpi[DPI$Year == boat.data.year]/DPI$dpi[DPI$Year == NRBS.survey.year]

#Finally bring everything together, and determine the growth factor
boating <- cbind(boating, gas.growth, dpi.growth)

boating$growth.rate <- boating$dpi.growth/boating$gas.growth

#Get the unadjusted gasoline numbers

boat.types <- merge(
      x = boat.types, 
      y=boating[c("State","Registered.Boats","Percent.Gasoline.Boats","growth.rate")], 
      by = "State", all.x = TRUE
)

#Get the offhighway numbers

boat.types <- boat.types %>% mutate(
      number.boats = Registered.Boats * Percent.Gasoline.Boats * Percent.State.Total,
      unadjusted.gallons = (number.boats * Gallon.Boat)/1000,
      Boating = unadjusted.gallons * growth.rate
) 

boating.offhwy <- aggregate(Boating ~ State, boat.types, sum)

off.highway.model <- merge(
      off.highway.model,boating.offhwy, 
      by = "State", all.y = TRUE
)

#remove the intermediatary tables
rm(boat.types,boating,dpi.growth, gas.growth, DPI, boating.offhwy)

#### Industrial Commerical and Construction Values #############

vm2.data.year <- 2016

vm2 <- read.csv("1 Year Update/VM2.csv", header = TRUE)

#merge our vm2 with the vius

vm2 <- subset(vm2, vm2$Year == vm2.data.year)

icc <- merge(x = vm2, y = vius, by = "State")

# Perform all the calculations to get the off highway number

icc <- icc %>% mutate(
      off.vmt.mm = VM.2 * Adj..Factor,
      PERCENT.VMT.OTHER = 
            1 - rowSums(icc[c("PERCENT.VMT.CONSTRUCTION", "PERCENT.VMT.INDUSTRIAL", 
            "PERCENT.VMT.COMMERCIAL")]),
      VMT.OTHER = off.vmt.mm * PERCENT.VMT.OTHER,
      VMT.CONSTRUCTION = PERCENT.VMT.CONSTRUCTION * off.vmt.mm,
      VMT.INDUSTRIAL = PERCENT.VMT.INDUSTRIAL * off.vmt.mm,
      VMT.COMMERCIAL = PERCENT.VMT.COMMERCIAL * off.vmt.mm,
      Construction = (VMT.CONSTRUCTION / MPG.CONSTRUCTION)*1000,
      Industrial.Commercial = ((VMT.COMMERCIAL / MPG.COMMERCIAL)*1000)+
            ((VMT.INDUSTRIAL / MPG.INDUSTRIAL)*1000)
)

off.highway.model <- merge(
      x = off.highway.model, 
      y = icc[c("State","Industrial.Commercial","Construction")], 
      by = "State", all.y = TRUE
)

rm(icc)

#### Public Sector Values ################################

#The two datasets used for this section come from the 
#Federal Fuel Report releaseed by GSA and the Annual Highway
#Statistics from FHWA

ffr.data.year <- 2015
fhwa.data.year <- 2016

# I don't know why this is used
scm.adj <- 0.131771767836034

#Note will use FFR 4-3 and MV-7 to build out values

#Yearly updated tables from GSA Federal Fleet Report, report expected in mid-april
ffr.23 <- read.csv("1 Year Update/FFR Table 2-3.csv", header = TRUE)
ffr.43 <- read.csv("1 Year Update/FFR Table 4-3.csv", header = TRUE)

ffr.23 <- subset(ffr.23, ffr.23$Year == ffr.data.year)
ffr.43 <- subset(ffr.43, ffr.43$Year == ffr.data.year)


#Yearly update tables from FHWA
mf2 <- read.csv("1 Year Update/MF-2.csv", header = TRUE)
mf21 <- read.csv("1 Year Update/MF-21.csv", header = TRUE)
mv7 <- read.csv("1 Year Update/MV-7.csv", header = TRUE)
mv9 <- read.csv("1 Year Update/MV-9.csv", header = TRUE)
vm1 <- read.csv("1 Year Update/VM1.csv", header = TRUE)

mf2 <- subset(mf2, mf2$Year == fhwa.data.year)
mf21 <- subset(mf21, mf21$Year == fhwa.data.year)
mv7 <- subset(mv7, mv7$Year == fhwa.data.year)
mv9 <- subset(mv9, mv9$Year == fhwa.data.year)
vm1 <- subset(vm1, vm1$Year == fhwa.data.year)

#Off road percentage by vehicle and Highway ratios for SCM, unknown update
off.by.vehicle <- read.csv("Unknown Update/Off By Vehicle.csv", header = TRUE)
scm.ratio <- read.csv("Unknown Update/SCM Ratio.csv", header = TRUE)

#Start performing calculations to fill out FFR Table 4-3
ffr.43 <- ffr.43 %>% mutate(
      PASSENGER.VMT.Total = 
            rowSums(ffr.43[c("PASSENGER.VMT.Domestic","PASSENGER.VMT.Foreign")], 
            na.rm = TRUE),
      TRUCKS.VMT.Total = rowSums(ffr.43[c("TRUCKS.VMT.Domestic",
            "TRUCKS.VMT.Foreign")], na.rm = TRUE),
      OTHER.VMT.Total = rowSums(ffr.43[c("OTHER.VMT.Domestic",
            "OTHER.VMT.Foreign")], na.rm = TRUE)
) 

ffr.43 <- ffr.43 %>% mutate(
      PASSENGER.GALLONS.Total = 
            PASSENGER.VMT.Total / vm1$MPG[vm1$Vehicle == "ALL LIGHT DUTY VEHICLES"] / (
                  PASSENGER.VMT.Total / 
                  vm1$MPG[vm1$Vehicle == "ALL LIGHT DUTY VEHICLES"] + 
                  TRUCKS.VMT.Total / vm1$MPG[vm1$Vehicle == "TRUCKS"] + 
                  OTHER.VMT.Total / vm1$MPG[vm1$Vehicle == "BUSES"]
            ) * Worldwide.Gasoline.Use..Gallons.
)

ffr.43 <- ffr.43 %>% mutate(
      TRUCKS.GALLONS.Total = 
            TRUCKS.VMT.Total / vm1$MPG[vm1$Vehicle == "TRUCKS"] / (
                  PASSENGER.VMT.Total /
                  vm1$MPG[vm1$Vehicle == "ALL LIGHT DUTY VEHICLES"] + 
                  TRUCKS.VMT.Total / vm1$MPG[vm1$Vehicle == "TRUCKS"] + 
                  OTHER.VMT.Total / vm1$MPG[vm1$Vehicle == "BUSES"]
            ) * Worldwide.Gasoline.Use..Gallons.
)

ffr.43 <- ffr.43 %>% mutate(
      OTHER.GALLONS.Total = 
            OTHER.VMT.Total / vm1$MPG[vm1$Vehicle == "BUSES"] / (
                  PASSENGER.VMT.Total / 
                  vm1$MPG[vm1$Vehicle == "ALL LIGHT DUTY VEHICLES"] + 
                  TRUCKS.VMT.Total / vm1$MPG[vm1$Vehicle == "TRUCKS"] + 
                  OTHER.VMT.Total / vm1$MPG[vm1$Vehicle == "BUSES"]
            ) * Worldwide.Gasoline.Use..Gallons.
)

ffr.43 <- ffr.43 %>% mutate(
      PASSENGER.GALLONS.Domestic = PASSENGER.VMT.Domestic / 
            PASSENGER.VMT.Total * PASSENGER.GALLONS.Total, 
      PASSENGER.GALLLONS.Foreign = PASSENGER.VMT.Foreign / 
            PASSENGER.VMT.Total * PASSENGER.GALLONS.Total
)

ffr.43 <- ffr.43 %>% mutate(
      TRUCKS.GALLONS.Domestic = TRUCKS.VMT.Domestic / 
            TRUCKS.VMT.Total * TRUCKS.GALLONS.Total,
      TRUCKS.GALLLONS.Foreign = TRUCKS.VMT.Foreign / 
            TRUCKS.VMT.Total * TRUCKS.GALLONS.Total
)

ffr.43 <- ffr.43 %>% mutate(
      OTHER.GALLONS.Domestic = OTHER.VMT.Domestic / 
            OTHER.VMT.Total * OTHER.GALLONS.Total,
      OTHER.GALLLONS.Foreign = OTHER.VMT.Foreign / 
            OTHER.VMT.Total * OTHER.GALLONS.Total
)

civilian.usps <- as.data.table(ffr.43[,3:ncol(ffr.43)], 
      keep.rownames = FALSE)[, lapply(.SD, sum, na.rm = TRUE), ]

#Peforming these calculations with adjustment factor to revised the numbers for 
#civilian.usps, look into why they do this instead of just sum 

civilian.usps.combined <- civilian.usps %>% mutate(
      TRUCKS.VMT.Domestic = 
            sum(ffr.43$TRUCKS.VMT.Domestic, na.rm = TRUE) + 
            (sum(ffr.43$OTHER.VMT.Domestic, na.rm = TRUE) * scm.adj),
      TRUCKS.VMT.Foreign = sum(ffr.43$TRUCKS.VMT.Foreign, na.rm = TRUE) + 
            (sum(ffr.43$OTHER.VMT.Foreign, na.rm = TRUE) * scm.adj),
      OTHER.VMT.Domestic = sum(ffr.43$OTHER.VMT.Domestic, na.rm = TRUE) *
            (1 - scm.adj),
      OTHER.VMT.Foreign = 
            sum(ffr.43$OTHER.VMT.Foreign, na.rm = TRUE) * (1 - scm.adj)
)

#For the next little bit, just going to be doing the calculated columns for 
#Federal and SCM gallons

mv7[is.na(mv7)] <- 0

mv7 <- mv7 %>% mutate(
      Federal.TOTAL = Federal.CAR + Federal.BUS + Federal.TRUCK,
      SCM.TOTAL = SCM.CAR + SCM.BUS + SCM.TRUCK
)

mf2 <- mf2 %>% mutate(Percent.Gasoline = Gasoline / All.Motor.Fuel)

fed.scm <- as.data.table(mv7[,5:ncol(mv7)], keep.rownames = FALSE)[, 
      lapply(.SD, sum, na.rm = TRUE), ]

mv7 <- merge(
      x = mv7, y = mf2[c("STATE","Percent.Gasoline")], 
      by = "STATE", all.x = TRUE
)

mv7 <- mv7 %>% mutate(
      Federal.CAR.tg = ((Federal.CAR / fed.scm$Federal.CAR) * 
            civilian.usps.combined$PASSENGER.GALLONS.Domestic)/1000,
      Federal.BUS.tg = ((Federal.BUS / fed.scm$Federal.BUS) * 
            civilian.usps.combined$OTHER.GALLONS.Domestic)/1000,
      Federal.TRUCK.tg = ((Federal.TRUCK / fed.scm$Federal.TRUCK) * 
            civilian.usps.combined$TRUCKS.GALLONS.Domestic)/1000
)

mv7 <- mv7 %>% mutate(
      Federal.offhwy = Federal.CAR.tg * off.by.vehicle$Passengers + 
            Federal.TRUCK.tg * off.by.vehicle$Trucks,
      Federal.onhwy = (Federal.CAR.tg + Federal.TRUCK.tg + Federal.BUS.tg) - 
            Federal.offhwy
)

mv7 <- mv7 %>% mutate(
      SCM.CAR.vmt = civilian.usps.combined$PASSENGER.VMT.Domestic/
            (sum(ffr.23$PASSENGER.Domestic)) * SCM.CAR,
      SCM.BUS.vmt = civilian.usps.combined$OTHER.VMT.Domestic/
            (sum(ffr.23$OTHER.Domestic)) * SCM.BUS,
      SCM.TRUCK.vmt = civilian.usps.combined$TRUCKS.VMT.Domestic/
            (sum(ffr.23$TRUCKS.Domestic)) * SCM.TRUCK
)

#Bring in our ratios
mv7 <- merge(x = mv7, y = scm.ratio, by = "STATE", all.x = TRUE)

mv7 <- mv7 %>% mutate(
      SCM.Total.tg = Percent.Gasoline * (
                  SCM.CAR.vmt / vm1$MPG[vm1$Vehicle == "ALL LIGHT DUTY VEHICLES"] + 
                  SCM.BUS.vmt / vm1$MPG[vm1$Vehicle == "BUSES"] + 
                  SCM.TRUCK.vmt / vm1$MPG[vm1$Vehicle == "TRUCKS"]
            ) / 1000
)

mv7 <- mv7 %>% mutate(
      SCM.offhwy = SCM.Total.tg * SCM.Ratio,
      SCM.onhwy = SCM.Total.tg - SCM.offhwy
)

off.highway.model <- merge(
      x = off.highway.model, 
      y = mv7[c("STATE","Federal.onhwy","SCM.onhwy","SCM.offhwy")], 
      by.x = "State", by.y = "STATE", all.y = TRUE
)

#remove all the public service tables no longer being used
rm(civilian.usps,civilian.usps.combined, fed.scm, ffr.23, ffr.43, 
      mf2, mf21, mv7, mv9, off.by.vehicle, scm.ratio, vm1, vm2)

#### Writing and Displaying the Model #####################

# write.csv(off.highway.model, "off_highway_script.csv", row.names = FALSE)