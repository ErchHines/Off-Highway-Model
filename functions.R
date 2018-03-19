# source("prep.R")

## Function for getting the gasoline numbers for a particular year from the 
## Epa Model ##################################################################

get.epa.results <- function(dataYear){
  
  EpaModel <- subset(EpaNonRoad, EpaNonRoad$Year == dataYear)
  
  EpaModel
}

## Calculate Agriculture Numbers ##############################################

calculate.ag <- function(dataYear, censusYear){
  
  # Split are files based on data and census Year
  AgRegionFuelCY <- subset(AgRegionFuel, Year == censusYear)
  AgBigStateFuelCY <- subset(AgBigStateFuel, Year == censusYear)
  
  AgRegionFuelDY<- subset(AgRegionFuel, Year == dataYear)
  AgBigStateFuelDY <- subset(AgBigStateFuel, Year == dataYear)
  
  #Get Epa data for the data year and exclude District of Columbia
  AgEpa <- subset(EpaNonRoad, EpaNonRoad$Year == dataYear)
  
  AgEpa <- subset(AgEpa, !(AgEpa$State == "District of Columbia"))
  
  
  #Get a Control Total from the sum of our regional numbers calculated
  #in prep.R
  ControlTotal <- AgRegionFuelDY$Total[AgRegionFuelDY$Region=="United States"]
  
  #Bring in the state's region's percent gas 
  AgStateAll <- merge(
    x=AgCensus, y=AgRegionFuelDY[c("Region", "PercentGas")], 
    by ="Region", all.x=TRUE
  )
  
  #Calculate a State's percentage of Total Fuel and Oil expenses
  #for the census year
  AgStateAll$PercentTotalS <- AgStateAll$FuelOilExpense/
    sum(AgStateAll$FuelOilExpense)
  
  # If a state is in the top 15 and has a reported fuel exp for the analysis year
  # then it is added to the table 
  AgStateAll <- merge(
    x=AgStateAll,y=AgBigStateFuelDY[c("Region","FuelExpenditure")], 
    by.x =c("State"),by.y=c("Region"),
    all.x = TRUE
  )
  
  # This splits up the states with a reported Fuel Expenditures from those 
  # states whose value will be imputed
  AgStateImpute <- subset(AgStateAll, is.na(AgStateAll$FuelExpenditure)==TRUE)
  AgStateReport <- subset(AgStateAll, is.na(AgStateAll$FuelExpenditure)==FALSE)
  
  ## Calculating Fuel Expenditure for states in the analysis year ##
  
  # Performs calculations to get to the 'adjusted' column in the excel file
  AgStateImpute <- AgStateImpute %>% mutate(
    FuelExpenditure = PercentTotalS * ControlTotal,
    FuelExpenditure = FuelExpenditure + (FuelExpenditure/sum(FuelExpenditure))*
      (ControlTotal - (sum(FuelExpenditure)+sum(AgStateReport$FuelExpenditure)))
  )
  
  
  #Bring all of our states back together for the rest of the calc
  AgStateAll <- rbind(AgStateImpute,AgStateReport)
  
  #Equivalent to Total Gasoline & Gasohol Expenses in 
  #Analysis Year ($1,000) in excel file
  AgStateAll$TotalGasS <- AgStateAll$FuelExpenditure*AgStateAll$PercentGas
  
  # Drop the Census year from our Ag State All table and change it to 
  # data year, now that we've brought in or calculated our data year 
  # columns
  AgStateAll$Year <- dataYear
  
  #Bring gas prices into ag table by year
  AgStateAll <- merge(
   x=AgStateAll ,y=RetailGasAll[c("State","Year","Ppg")], 
   by = c("State","Year"), all.x = TRUE
  )
  
  #Determine gallons of gasoline by taking the total money spent on
  # gasoline and dividing it by the price of gas per gallon for that 
  #year
  AgStateAll$AgAll <-AgStateAll$TotalGasS/AgStateAll$Ppg

  #Bring in EPA Agricultural Equipment numbers
  AgStateAll <- merge(
    x=AgStateAll , y = AgEpa[c("State","Agricultural Equipment")], 
    by= "State", all.x = TRUE
  )
   
  AgStateAll <- arrange(AgStateAll, State)
  AgVius <- arrange(AgVius, State)

  #Perform calculations to get off highway number for agriculture
  AgStateAll <- AgStateAll %>% mutate(
    TruckOn  = (AgAll - `Agricultural Equipment`) * AgVius$OnVmtTruck,
    Agriculture = AgAll - TruckOn
  )
  
  Agriculture <- AgStateAll[,c("Year","State","Agriculture")]
  
  Agriculture

}

## Aviation FUnction ##########################################################

calculate.flight <- function(dataYear){

  #Subset the aviation figures based on year and wheter or not they have values
  #thousands of gallons per year

  AviationState   <- subset(AviationState, AviationState$Year == dataYear)
  AviationPaddSum <- subset(AviationPaddSum, AviationPaddSum$Year == dataYear)
  
  AviationStateImpute <- subset(
    AviationState, is.na(AviationState$ThousandGalYearS) == TRUE 
  )
 
  AviationStateReport <- subset(
    AviationState, is.na(AviationState$ThousandGalYearS) == FALSE
  )

  #Remove the thousand gal year column from the impute table
  AviationStateImpute$ThousandGalYearS <- NULL
  
  ## Calculate the missing Thousand gallons per year for our impute states ##
  
  AviationMissing <- aggregate(
    FlightHours ~ Year + PADD, AviationStateImpute, sum
  )
 
  AviationStateImpute <- merge(
    x = AviationStateImpute, y = AviationMissing, 
    by = c("PADD","Year"), all.x= TRUE
  )
  
  AviationStateImpute <- AviationStateImpute %>% mutate(
    PercentMissing = FlightHours.x / FlightHours.y
  )
  
  AviationStateImpute <- merge(
    x = AviationStateImpute, y=AviationPaddSum[c("PADD","Year","remainder")], 
    by = c("PADD","Year"), all.x = TRUE
  )
  
  #Put our calculated Thousand Gallons a year back into the table
  AviationStateImpute <- AviationStateImpute %>% mutate(
    ThousandGalYearS = PercentMissing * remainder
  )
  
  # Break down our impute and report tables and bind them together 
  # for our final output
  
  AviationStateImpute <- 
    AviationStateImpute[,c("Year","State","ThousandGalYearS")]
  
  AviationStateReport <-
    AviationStateReport[,c("Year","State","ThousandGalYearS")]
  
  #put everything together and output the numbers for the off highway model 
  Aviation <- rbind(AviationStateImpute, AviationStateReport)
  
  # Arrange by State
  Aviation <- arrange(Aviation, Aviation$State)
  
  colnames(Aviation)[3] <- "Aviation"
  
  Aviation
} 


## Industrial, Commercial, and Construction function ############################

calculate.icc <- function(dataYear){
  
  Vm2 <- subset(Vm2, Vm2$Year == dataYear)
  
  Icc <- merge(x = Vm2, y = Vius, by = "State")
  
  # Perform all the calculations to get the off highway number
  
  Icc <- Icc %>% mutate(
    OffVmtMillion = VmtAll * AdjFactor,
    PercentVmtOther = 
      1 - rowSums(Icc[c("PercentVmtConstruction", "PercentVmtIndustrial", 
                        "PercentVmtCommercial")]),
    VmtOher = OffVmtMillion * PercentVmtOther,
    VmtConstruction = PercentVmtConstruction * OffVmtMillion,
    VmtIndustrial   = PercentVmtIndustrial * OffVmtMillion,
    VmtCommercial   = PercentVmtCommercial * OffVmtMillion,
    Construction = (VmtConstruction / MpgConstruction)*1000,
    IndustrialCommercial = ((VmtCommercial / MpgCommercial)*1000)+
      ((VmtIndustrial / MpgIndustrial)*1000)
  )
  
  Industry <- Icc[,c("Year","State","IndustrialCommercial","Construction")]
}


#### Recreational Boating Function #############################################

calculate.boat <- function(dataYear,surveyYear){
  
  StateRecBoating <- subset(StateRecBoating, StateRecBoating$Year == dataYear)
  BoatMpg <- subset(BoatMpg, BoatMpg$Year == surveyYear)
  
  #VERY IMPORTANT make sure everything is arranged correctly for the next operations
  RetailGasAll <- arrange(RetailGasAll, State, Year)
  BeaDpi <- arrange(BeaDpi, State, Year)
  StateRecBoating <- arrange(StateRecBoating, State, Year)
  
  #get the value of dataYear gas prices divided by surveyYear gas prices
  GasGrowth <- RetailGasAll$Ppg[
    RetailGasAll$Year == dataYear]/
    RetailGasAll$Ppg[RetailGasAll$Year == surveyYear
                     ]
  
  #same for BeaDpi
  DpiGrowth <- BeaDpi$Dpi[BeaDpi$Year == dataYear]/BeaDpi$Dpi[BeaDpi$Year == surveyYear]
  
  #Finally bring everything together, and determine the growth factor
  StateRecBoating <- cbind(StateRecBoating, GasGrowth, DpiGrowth)
  
  StateRecBoating$GrowthRate <- StateRecBoating$DpiGrowth/StateRecBoating$GasGrowth
  
  #Get the unadjusted gasoline numbers
  
  BoatMpg <- merge(
    x = BoatMpg, 
    y=StateRecBoating[c("State","RegisteredBoats","PercentGasolineBoats","GrowthRate")], 
    by = "State", all.x = TRUE
  )
  
  #Get the offhighway numbers
  
  BoatMpg <- BoatMpg %>% mutate(
    NumberBoats = RegisteredBoats * PercentGasolineBoats * PercentStateTotal,
    AdjGallons = (NumberBoats * GallonBoat)/1000,
    Boating = AdjGallons * GrowthRate
  ) 
  
  #Since we built our table using survey data, we need to change the year 
  #before proceeding to retrieve our boating numbers
  
  BoatMpg$Year <- dataYear
  
  Boating <- aggregate(Boating ~ Year + State, BoatMpg, sum)
  
  Boating
}


## Public Sector Function #####################################################

calculate.gov <- function(fhDataYear, gsDataYear){

  #Subset Mv7, FedScm and total CIvilianUSPS for the year
  
  Mv7 <- subset(Mv7, Mv7$Year == fhDataYear)
  
  CivilianUSPS <- subset(CivilianUSPS, CivilianUSPS$Year == gsDataYear)
  
  FedScm <- subset(FedScm, FedScm$Year == fhDataYear)
  
  Vm1 <- subset(Vm1, Vm1$Year == fhDataYear)
  
  Mv7 <- Mv7 %>% mutate(
    FedCarGal  = ((FederalCar / FedScm$FederalCar) *
                    CivilianUSPS$PassengerGalDomestic)/1000,
  
    FedBusGal  = ((FederalBus / FedScm$FederalBus) *
                    CivilianUSPS$OtherGalDomestic)/1000,
  
    FedTruckGal= ((FederalTruck / FedScm$FederalTruck) *
                    CivilianUSPS$TrucksGalDomestic)/1000
  )
  
  Mv7 <- Mv7 %>% mutate(
    FederalOffhwy = FedCarGal* OffRdByVehicle$Passengers +
      FedTruckGal* OffRdByVehicle$Trucks,
  
    FederalOnhwy  = (FedCarGal+ FedTruckGal+ FedBusGal) -
      FederalOffhwy
  )
  
  Mv7 <- Mv7 %>% mutate(
    ScmCarVmt   = CivilianUSPS$PassengerVmtDomestic/
      (sum(Ffr23$PassengerDomestic)) * ScmCar,
  
    ScmBusVmt   = CivilianUSPS$OtherVmtDomestic/
      (sum(Ffr23$OtherDomestic)) * ScmBus,
  
    ScmTruckVmt = CivilianUSPS$TrucksVmtDomestic/
      (sum(Ffr23$TrucksDomestic)) * ScmTruck
  )
  
  #Bring in our ratios
  Mv7 <- merge(x = Mv7, y = ScmRatio, by = "State", all.x = TRUE)
  
  Mv7 <- Mv7 %>% mutate(
    ScmTotalGal = PercentGasoline * (
      ScmCarVmt   / Vm1$Mpg[Vm1$Vehicle == "ALL LIGHT DUTY VEHICLES"] +
        ScmBusVmt   / Vm1$Mpg[Vm1$Vehicle == "BUSES"] +
        ScmTruckVmt / Vm1$Mpg[Vm1$Vehicle == "TRUCKS"]
    ) / 1000
  )
  
  Mv7 <- Mv7 %>% mutate(
    ScmOffhwy = ScmTotalGal * ScmRatio,
    ScmOnhwy  = ScmTotalGal - ScmOffhwy
  )

  Government <- Mv7[c("Year","State","FederalOnhwy","ScmOnhwy","ScmOffhwy")]
  
  Government
}

#That concludes the calculation of our new model, now we get the Off-Road
# Recreational values that go inito our combined model
#use fhDataYear

calculate.rec <- function(fhDataYear){
  
  Vm1 <- subset(Vm1, Vm1$Year == fhDataYear)
  
  RecV <- RecV %>% mutate(
      OffRdGbyV = (
          Vm1$VmtVehicle[Vm1$Vehicle == "ALL LIGHT DUTY VEHICLES"] * (
              1 - LtOffrdVmtDiscount
          )
      ) / (
          Vm1$Mpg[Vm1$Vehicle == "ALL LIGHT DUTY VEHICLES"] * (
              1 - LtOffrdMpgDiscount
          )
      )
  )
  
  
  #create the calculated columns for light truck, motorcycle and ATV use
  StateOffRoadRec <- StateOffRoadRec %>% mutate(
      LtOffrdGal = (
          Pickups * PercentOffrdPickup +
          Suvs * PercentOffrdSUVs
          ) * RecV$OffRdGbyV,
      MotorcycleLow  = Motorcycles * 54,
      MotorcycleMid  = Motorcycles * 59,
      MotorcycleHigh = Motorcycles * 64,
      AtvLow  = Atvs * RecV$AtvRecreationalUsage * 46,
      AtvMid  = Atvs * RecV$AtvRecreationalUsage * 55.5,
      AtvHigh = Atvs * RecV$AtvRecreationalUsage * 65
  )
  
  #And now the long-awaited calculations for snowmobile gallonage
  StateOffRoadRec <- StateOffRoadRec %>% mutate(
      TotalSnowmobiles = RegSnowmobiles + UnRegSnowmobiles,
      RecFuel = RecV$SnowRecPercent *
          RecV$SnowRecMpg * TotalSnowmobiles * SnowFactor,
      IceFuel = RecV$SnowIcePercent *
          RecV$SnowIceMpg * TotalSnowmobiles * SnowFactor,
      AdjRecFuel = RecFuel * (
          RecV$SnowRecPercent * RecV$SnowRecMpg *
          sum(TotalSnowmobiles, na.rm = TRUE) /
          sum(RecFuel, na.rm = TRUE)
      ),
      AdjIceFuel = IceFuel * (
          RecV$SnowIcePercent * RecV$SnowIceMpg *
          sum(TotalSnowmobiles, na.rm = TRUE) /
          sum(IceFuel, na.rm = TRUE)
      ),
      AdjSnowTotal = AdjRecFuel + AdjIceFuel
  )
  
  #Get the total fuel use, 
  #Note: Not putting this into the new model table as it is not
  #there in the spreadsheet, will look into, the final adjusted
  #number is used in the combined model so will it Recreational
  
  StateOffRoadRec <- StateOffRoadRec %>% mutate(
      UnadjustedRecreational = 
          rowSums(StateOffRoadRec[c("LtOffrdGal", "MotorcycleMid", "AtvMid",
          "AdjSnowTotal")], na.rm = TRUE),
      Recreational = UnadjustedRecreational * (1 + FederalLandFactor)
  )
  
  Recreation <- StateOffRoadRec[,c("Year","State","Recreational")]
  
  Recreation
}







