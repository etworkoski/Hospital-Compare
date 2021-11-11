## Title: BestHospital
## Description: Find hospital with the lowest outcome rates given a
##   user-specified state and hospital
## Author: Ellen Tworkoski
## Date: 11/11/2021

library(Hmisc)
library(data.table)
library(tidyverse)
best <- function(state,outcome){
    #Read in outcome data
    input_file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #Check validity of user-specified outcome and state
    if(outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
        stop("invalid outcome")
    }
    if(state %nin% input_file$State){
        stop("invalid state")
    }
    
    #Return name of hospital with lowest outcome rate in specified state
    #If multiple hospitals have the lowest rate select the one that comes first alphabetically
    state_input <- input_file[input_file$State == state,]  #get data for user-specified state
    
    if(outcome == "heart attack"){
        state_input_noNA <- state_input[state_input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available",]
        state_input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(state_input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        order_ha <- arrange(state_input_noNA, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)
        result <- order_ha[1,"Hospital.Name"]
        return(result)  #return name of hospital
    }
    if(outcome == "heart failure"){
        state_input_noNA <- state_input[state_input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available",]
        state_input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(state_input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        order_hf <- arrange(state_input_noNA, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)
        result <- order_hf[1,"Hospital.Name"]
        return(result)  #return name of hospital
    }
    if(outcome == "pneumonia"){
        state_input_noNA <- state_input[state_input$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available",]
        state_input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(state_input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        order_pneu <- arrange(state_input_noNA, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)
        result <- order_pneu[1,"Hospital.Name"]
        return(result)  #return name of hospital
    }
}

#Tests
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
