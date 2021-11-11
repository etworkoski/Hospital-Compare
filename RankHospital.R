## Title: RankHospital
## Description: Find hospital with a given rank in outcome occurrence
## User specifies outcome and rank of interest, as well as state of interest
## Author: Ellen Tworkoski
## Date: 11/11/2021

library(Hmisc)
library(tidyverse)
rankhospital <- function(state, outcome, num = "best"){
    #Read in outcome data
    input_file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #Check validity of user-specified outcome and state
    if(outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
        stop("invalid outcome")
    }
    if(state %nin% input_file$State){
        stop("invalid state")
    }
    
    #Return name of hospital with specified outcome rank in specified state
    #If multiple hospitals have the lowest rate select the one that comes first alphabetically
    #If specified rank is larger than number of hospitals in state return NA
    state_input <- input_file[input_file$State == state,]  #get data for user-specified state
    
    if(outcome == "heart attack"){
        state_input_noNA <- state_input[state_input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available",]  #remove NA values
        state_input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(state_input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)  #convert rate to numeric to ensure correct sorting
        order_ha <- arrange(state_input_noNA, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)
        
        last_rank <- nrow(order_ha)
        if(num == "worst"){rank = last_rank}
        else if(num == "best"){rank = 1}
        else {rank = num}
        if (rank <= last_rank){
            result <- order_ha[rank,"Hospital.Name"]
        }
        else{result = "NA"}
        return(result)  #return name of hospital
    }
    if(outcome == "heart failure"){
        state_input_noNA <- state_input[state_input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available",]
        state_input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(state_input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        order_hf <- arrange(state_input_noNA, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)
        
        last_rank <- nrow(order_hf)
        if(num == "worst"){rank = last_rank}
        else if(num == "best"){rank = 1}
        else {rank = num}
        if (rank <= last_rank){
            result <- order_hf[rank,"Hospital.Name"]
        }
        else{result = "NA"}
        return(result)  #return name of hospital
    }
    if(outcome == "pneumonia"){
        state_input_noNA <- state_input[state_input$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!= "Not Available",]
        state_input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(state_input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        order_pneu <- arrange(state_input_noNA, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)
        
        last_rank <- nrow(order_pneu)
        if(num == "worst"){rank = last_rank}
        else if(num == "best"){rank = 1}
        else {rank = num}
        if (rank <= last_rank){
            result <- order_pneu[rank,"Hospital.Name"]
        }
        else{result = "NA"}
        return(result)  #return name of hospital
    }
}

#Tests
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
