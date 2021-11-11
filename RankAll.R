## Title: RankAll
## Description: Return list of hospitals in each state (one hospital per state)
## that have a user-specified rank of outcome occurrence 
## Author: Ellen Tworkoski
## Date: 11/11/2021

library(Hmisc)
library(tidyverse)
rankall <- function(outcome, num = "best"){
    #Read in outcome data
    input_file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #Check validity of user-specified outcome and rank
    if(outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
        stop("invalid outcome")
    }
    if(num != "best" && num != "worst" && num > 10000){
        stop("invalid rank")
    }
    
    #Create vector of all of state abbrev. 
    #Create a second empty vector to store hospital names
    states <- unique(input_file$State) %>% sort()
    hospitals <- c()
    
    #Return data frame with name of hospital with specified outcome rank in each state
    #If multiple hospitals have the lowest rate select the one that comes first alphabetically
    #If specified rank is larger than number of hospitals in a given state return NA
    if(outcome == "heart attack"){
        input_noNA <- input_file[input_file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available",]  #remove NA values
        input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)  #convert rate to numeric to ensure correct sorting
        
        #Loop through vector of state abbrev and fill second vector with hospital names
        for (s in 1:length(states)){
            order_ha <- input_noNA[input_noNA$State == states[s], ] %>% arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)
            last_rank <- nrow(order_ha)
            if(num == "worst"){rank = last_rank}
            else if(num == "best"){rank = 1}
            else {rank = num}
            if (rank <= last_rank){
                hospitals[s] <- order_ha[rank,"Hospital.Name"]
            }
            else{hospitals[s] = "NA"}
        }
        #join vectors together into data frame and output
        result <- data.frame(states, hospitals)
        return(result)  #return name of hospital
    }
    
    if(outcome == "heart failure"){
        input_noNA <- input_file[input_file$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available",]  #remove NA values
        input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)  #convert rate to numeric to ensure correct sorting
        
        #Loop through vector of state abbrev and fill second vector with hospital names
        for (s in 1:length(states)){
            order_ha <- input_noNA[input_noNA$State == states[s], ] %>% arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)
            last_rank <- nrow(order_ha)
            if(num == "worst"){rank = last_rank}
            else if(num == "best"){rank = 1}
            else {rank = num}
            if (rank <= last_rank){
                hospitals[s] <- order_ha[rank,"Hospital.Name"]
            }
            else{hospitals[s] = "NA"}
        }
        #join vectors together into data frame and output
        result <- data.frame(states, hospitals)
        return(result)  #return name of hospital
    }
    
    if(outcome == "pneumonia"){
        input_noNA <- input_file[input_file$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available",]  #remove NA values
        input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(input_noNA$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)  #convert rate to numeric to ensure correct sorting
        
        #Loop through vector of state abbrev and fill second vector with hospital names
        for (s in 1:length(states)){
            order_ha <- input_noNA[input_noNA$State == states[s], ] %>% arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)
            last_rank <- nrow(order_ha)
            if(num == "worst"){rank = last_rank}
            else if(num == "best"){rank = 1}
            else {rank = num}
            if (rank <= last_rank){
                hospitals[s] <- order_ha[rank,"Hospital.Name"]
            }
            else{hospitals[s] = "NA"}
        }
        #join vectors together into data frame and output
        result <- data.frame(states, hospitals)
        return(result)  #return name of hospital
    }
}

#Tests
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
