path = "~/datasciencecoursera/rprog-data-rprog-data-ProgAssignment3-data/"

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcome_data <- read.csv(paste(path,"outcome-of-care-measures.csv",sep=""),
                                 na.strings = "Not Available", 
                                 stringsAsFactors = FALSE)
        
        ## Defining the possible outcomes
        possible_outcomes <- c("heart attack" = 11, "heart failure"=17,
                               "pneumonia" = 23)
        
        ## Check that outcome are valid
        if (!(outcome %in%names(possible_outcomes))) {
                stop ("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        my_data <- na.omit(outcome_data[,c(2,7,possible_outcomes[outcome])])
        names(my_data) <- c("Hospital", "State", "Outcome")
        
        #Sorting the data by State, outcome and hospital name
        order_list <- order(my_data$State, my_data$Outcome, my_data$Hospital)
        ordered_data <- my_data[order_list,]
        
        # Splitting the ordered data by State
        split_data <- split(ordered_data[,"Hospital"], ordered_data$State)
   
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        hospitalNameFunction <- function(x, num) {
                if (num == "best") {
                        head(x, 1)
                }
                else if (num == "worst") {
                        tail(x, 1)
                }
                else {
                        x[num]
                }
        }

        result <- lapply(split_data, hospitalNameFunction, num)
        hospital_name <- unlist(result)
        state_name <- names(result)
        
        data.frame(hospital = hospital_name,
                                 state = state_name,
                                 row.names = state_name)


}