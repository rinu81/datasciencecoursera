rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcome_data <- read.csv("~/datasciencecoursera/rprog-data-rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        col_state <- outcome_data[,"State"]
        
        ## Defining the possible outcomes
        possible_outcomes <- c("heart attack" = 11, "heart failure"=17, "pneumonia" = 23)
        
        ## Check that state and outcome are valid
        if (!(state %in%col_state)){
                stop ("invalid state")
        }
        
        if (!(outcome %in%names(possible_outcomes))) {
                stop ("invalid outcome")
        }
        
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        my_data <- na.omit(outcome_data[,c(2,7,possible_outcomes[outcome])])
        names(my_data) <- c("Hospital", "State", "Outcome")
        
        order_list <- order(my_data$State, my_data$Outcome, my_data$Hospital)
        ordered_data <- my_data[order_list,]
        final_data <- ordered_data[ordered_data$State == state,]
        
        if (num == "best") {
                num <- 1
        }
        else if (num == "worst") {
                num = nrow(final_data)
        }
        return(final_data[num,1])
}