### This script is to combine and display the FHWA Non-Highway and 
### the combined FHWA and EPA Non Highway Model, it builds off
### load, clean, and functions file in this folder 

## Source the appropiate files 

source("load.R")
source("prep.R")
source("functions.R")

## Input the year of all the data that has been entered

analysis.year      <- 2017

ag.data.year       <- 2016
ag.census.year     <- 2012

aviation.data.year <- 2016

boat.data.year     <- 2016
NRBS.survey.year   <- 2012

vm2.data.year      <- 2016

ffr.data.year      <- 2015
fhwa.data.year     <- 2016

epa.data.year      <- 2016

## Run the functions

Agriculture <- calculate.ag(ag.data.year, ag.census.year)

Aviation    <- calculate.flight(aviation.data.year)

Industry    <- calculate.icc(vm2.data.year)

Boating     <- calculate.boat(boat.data.year, NRBS.survey.year)

Government  <- calculate.gov(fhwa.data.year,ffr.data.year)

Recreation  <- calculate.rec(fhwa.data.year)

EpaFinal    <- get.epa.results(epa.data.year)


## Merge our list to create the table for the FHWA Off Highway Model
FhwaOffHighwayModel <- list(Agriculture, Aviation, Industry, Boating, Government) %>%
  Reduce(function(x,y) full_join(x,y,by= c("Year","State")), .)

# Replace the data year currently in our model with the analyis year

FhwaOffHighwayModel$Year <- analysis.year

FhwaOffHighwayModel <- arrange(FhwaOffHighwayModel, FhwaOffHighwayModel$State)

write.csv(FhwaOffHighwayModel, "OffHighwayModel.csv", row.names = FALSE)


## Combine the FHWA and EPA models to create the Combined Model

CombinedModel <- FhwaOffHighwayModel

CombinedModel$Aviation <- FhwaOffHighwayModel$Aviation +
  EpaFinal$`Airport Equipment`

CombinedModel$IndustrialCommercial <- FhwaOffHighwayModel$IndustrialCommercial +
  EpaFinal$`Commercial Equipment` + EpaFinal$`Industrial Equipment`

CombinedModel$Construction <- FhwaOffHighwayModel$Construction + 
  EpaFinal$`Construction and Mining Equipment`

CombinedModel$Recreational <- Recreation$Recreational/1000

CombinedModel$LawnGarden <- EpaFinal$`Lawn and Garden Equipment (Com)` +
  EpaFinal$`Lawn and Garden Equipment (Res)`

CombinedModel$Miscellaneous <- EpaFinal$`Logging Equipment` +
  EpaFinal$`Railroad Equipment`


## Rearrange columns to match the excel sheet

CombinedModel <- CombinedModel[,c(1,2,3,4,5,6,7,11,12,13,8,9,10)]

NamesForModel <- c("Analysis Year","State","Agriculture","Aviation",
  "Industrial and Commericial", "Construction", "Marine",
  "Off-Road Recreational", "Lawn and Garden", "Miscellaneous", 
  "Federal Civilian Highway", "State, County, Municipal Highway",
  "State, County, Municipal NonHighway")

colnames(CombinedModel) <- NamesForModel

write.csv(CombinedModel, "CombinedModel.csv", row.names = FALSE)

MeltCombo <- melt(CombinedModel, id.vars = c("Analysis Year","State"))

write.csv(MeltCombo, "MeltedModel.csv", row.names = FALSE)
