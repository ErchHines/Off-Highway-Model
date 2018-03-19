# 
# #Load the tables needed
# #load("NonHwyRaw.Rdata")
# 
# source("load.R")


# Subset the EPA data to just gasoline, Wide cast it and  divide everything
# by 1000 and remove Puerto Rico and VI from the table

EpaNonRoad <- subset(EpaNonRoad, FuelType == "Gasoline")

EpaNonRoad <- dcast(
  EpaNonRoad, Year + State ~ Class, sum, value.var = "FuelCons"
)

EpaNonRoad <- aggregate( . ~ Year + State, EpaNonRoad, function(x) x/1000)

EpaNonRoad <- EpaNonRoad[!(EpaNonRoad$State == "Puerto Rico") & 
                         !(EpaNonRoad$State == "Virgin Islands"),]


#is.leayear placed here to calculate on of the columns in aviation data
#to convert from thousand gallons gasoline per day to per year

is.leapyear=function(year){
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

## Gasoline Prices Table for All States ###################

#cretate table with all states that inlcudes ppg columns
RetailGasAll <- merge(
  x=StatePaddFips,
  y=GasBigStateRetail[c("State","Year","Ppg")], 
  by="State", all = TRUE
)

#subset based on whether EIA provides report for particular states
RetailGasReport <- subset(RetailGasAll, is.na(RetailGasAll$Ppg)==FALSE)

RetailGasImpute <- subset(RetailGasAll, is.na(RetailGasAll$Ppg)==TRUE)

# #remove NA columns to avoid duplication and we are going to 
# #get these values from the PADD Data
RetailGasImpute$Ppg <- NULL

RetailGasImpute$Year <- NULL

#substitute a state's price per gallon with that of it's PADD
RetailGasImpute <- merge(
  x=RetailGasImpute,
  y=GasPaddRetail[c("PADD", "Year","Ppg")],
  by=c("PADD"), all = TRUE
)

#bring all the states back together
RetailGasAll <- rbind(RetailGasImpute,RetailGasReport)

#remove tables that are not likely to be needed later
rm(GasBigStateRetail,GasPaddRetail, RetailGasImpute, RetailGasReport)


## Agriculture Tables #####################################

#Due to the need to match most recent census with data, most of the
#calculations in this section will be carried out in functions.R

#Get a percentage for gasoline of total fuel and sum the regions for 
#each year for Alaska and Hawaii
AgRegionFuelTemp <- aggregate( . ~ Year + Census, AgRegionFuel, sum)

AgRegionFuelTemp$Region <- replace(
  AgRegionFuelTemp$Region, AgRegionFuelTemp$Region == 15, "United States"
)

AgRegionFuel <- rbind(AgRegionFuel, AgRegionFuelTemp)


AgRegionFuel$PercentGas <- AgRegionFuel$Gasoline/AgRegionFuel$Total

#Remove DC from the vius table for agriculture calculations
AgVius <- subset(Vius, !(Vius$State == "District of Columbia"))

# Create the control total and put it in wide format


rm(AgRegionFuelTemp)

## Aviation Tables ########################################

# Get the Padd for each state
AviationState <- merge(
  x = StatePaddFips[c("PADD","State")],y = AviationState, 
  by = "State", all.y = TRUE
)

#Calculate the volume of aviation in thousands of gallons per year 
#(tgy) taking into account whether or not the data comes from a leap year
AviationState <- AviationState %>% mutate(
  ThousandGalYearS = ifelse(
    is.leapyear(Year), AviationGalDay*366,
    AviationGalDay*365
  )
) 

AviationPadd <- AviationPadd %>% mutate(
  ThousandGalYearP = ifelse(
    is.leapyear(Year),AviationGalPadd*366,
    AviationGalPadd*365
  )
) 

#Sum up hours and thousand gallons year for each PADD and put it in 
#the AviationPadd table, note: using most recent year hours, 
#for 2016 it appears FAA changed it's numbers so 2015's numbers 
#have been substituted for 2016

AviationPaddSum <- aggregate(FlightHours ~ Year + PADD, AviationState, sum)

AviationPaddSum <- merge(
  x = AviationPaddSum, y = AviationPadd[c("PADD","Year","ThousandGalYearP")],
  by = c("PADD","Year"), all.x = TRUE 
)

tempsum <- aggregate(ThousandGalYearS ~ Year + PADD, AviationState, sum)

AviationPaddSum <- AviationPaddSum %>% mutate(
  ThousandGalYearTotal = tempsum$ThousandGalYearS,
  remainder = ThousandGalYearP - ThousandGalYearTotal
)

rm(tempsum, AviationPadd)

## Public Sector ##########################################

# Arrange these tables first to ensure nothing screwy happens

Mv7[is.na(Mv7)] <- 0
Ffr43[is.na(Ffr43)] <- 0
Ffr23[is.na(Ffr23)] <- 0

#Start performing calculations to fill out FFR Table 4-3
Ffr43 <- Ffr43 %>% mutate(
  PassengerVmtTotal = 
    rowSums(Ffr43[c("PassengerVmtDomestic","PassengerVmtForeign")], 
            na.rm = TRUE),
  
  TrucksVmtTotal = 
    rowSums(Ffr43[c("TrucksVmtDomestic", "TrucksVmtForeign")], 
            na.rm = TRUE),
  
  OtherVmtTotal= 
    rowSums(Ffr43[c("OtherVmtDomestic", "OtherVmtForeign")], 
            na.rm = TRUE)
)

Ffr43 <- Ffr43 %>% mutate(    
  PassengerGalTotal =
    PassengerVmtTotal / Vm1$Mpg[Vm1$Vehicle == "ALL LIGHT DUTY VEHICLES"] / (
      PassengerVmtTotal /
        Vm1$Mpg[Vm1$Vehicle == "ALL LIGHT DUTY VEHICLES"] +
        TrucksVmtTotal / Vm1$Mpg[Vm1$Vehicle == "TRUCKS"] +
        OtherVmtTotal / Vm1$Mpg[Vm1$Vehicle == "BUSES"]
    ) * WorldwideGasoline,

  TrucksGalTotal = 
    TrucksVmtTotal / Vm1$Mpg[Vm1$Vehicle == "TRUCKS"] / (
      PassengerVmtTotal /
        Vm1$Mpg[Vm1$Vehicle == "ALL LIGHT DUTY VEHICLES"] + 
        TrucksVmtTotal / Vm1$Mpg[Vm1$Vehicle == "TRUCKS"] + 
        OtherVmtTotal / Vm1$Mpg[Vm1$Vehicle == "BUSES"]
    ) * WorldwideGasoline,
  
  OtherGalTotal = 
    OtherVmtTotal/ Vm1$Mpg[Vm1$Vehicle == "BUSES"] / (
      PassengerVmtTotal / 
        Vm1$Mpg[Vm1$Vehicle == "ALL LIGHT DUTY VEHICLES"] + 
        TrucksVmtTotal / Vm1$Mpg[Vm1$Vehicle == "TRUCKS"] + 
        OtherVmtTotal / Vm1$Mpg[Vm1$Vehicle == "BUSES"]
    ) * WorldwideGasoline
)


Ffr43 <- Ffr43 %>% mutate(     
  PassengerGalDomestic = PassengerVmtDomestic / 
    PassengerVmtTotal    * PassengerGalTotal, 
  
  PassengerGalForeign = PassengerVmtForeign / 
    PassengerVmtTotal    * PassengerGalTotal,
  
  TrucksGalDomestic = TrucksVmtDomestic / 
    TrucksVmtTotal    * TrucksGalTotal,
  
  TrucksGalForeign = TrucksVmtForeign / 
    TrucksVmtTotal    * TrucksGalTotal,
  
  OtherGalDomestic = OtherVmtDomestic / 
    OtherVmtTotal   * OtherGalTotal,
  
  OtherGalForeign = OtherVmtForeign / 
    OtherVmtTotal   * OtherGalTotal
)

Ffr43[is.na(Ffr43)] <- 0

CivilianUSPS <- aggregate( . ~ Year , Ffr43, sum)

#Peforming these calculations with adjustment factor to revised the numbers for 
#CivilianUSPS, look into why they do this instead of just sum 

CivilianUSPS <- CivilianUSPS %>% mutate(
  TrucksVmtDomestic = 
    sum(Ffr43$TrucksVmtDomestic, na.rm = TRUE) + 
    (sum(Ffr43$OtherVmtDomestic, na.rm = TRUE) * ScmAdj),
  TrucksVmtForeign = sum(Ffr43$TrucksVmtForeign, na.rm = TRUE) + 
    (sum(Ffr43$OtherVmtForeign, na.rm = TRUE) * ScmAdj),
  OtherVmtDomestic = sum(Ffr43$OtherVmtDomestic, na.rm = TRUE) *
    (1 - ScmAdj),
  OtherVmtForeign  = 
    sum(Ffr43$OtherVmtForeign, na.rm = TRUE) * (1 - ScmAdj)
)

#Fill out the Mv7 Table with calculated Values

Mv7 <- Mv7 %>% mutate(
  FederalTotal = FederalCar + FederalBus + FederalTruck,
  ScmTotal = ScmCar + ScmBus + ScmTruck
)

Mf2 <- Mf2 %>% mutate(PercentGasoline = Gasoline / AllMotorFuel)

FedScm <- aggregate( . ~ Year , Mv7, sum)

Mv7 <- merge(
  x = Mv7, y = Mf2[c("State","Year","PercentGasoline")], 
  by = c("State","Year"), all.x = TRUE
)


## Off Road Recreational ##################################

#Note: Annual Data comes from FHWA so use fhwa.data.year
#state level off road data and Mv9 will be used to build out table


#merge the Mv9 and state off road StateOffRoadRecrational tables
StateOffRoadRec <- merge(
  x = Mv9, y = StateOffRoadRec,
  by = c("State","ABBREV","FIPS")
)

#Land Factor Assigned to the states
StateOffRoadRec$FederalLandFactor <- cut(
  StateOffRoadRec$RuralNetLand,
  breaks = c(-1, 20000, 40000, 60000, 80000, 100000, 120000, Inf),
  labels = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)
)

StateOffRoadRec$FederalLandFactor <- as.numeric(
  as.character(StateOffRoadRec$FederalLandFactor)
)

# Save our cleaned up data, will be useful for Shiny Application
save(list = ls(all = TRUE), file= "NonHwyClean.RData")



