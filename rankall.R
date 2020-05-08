rankall <- function(outcome, num){
        
        # Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character")
        
       # Get list of states
        states <- sort(unique(data$State))
        
        # Loop through states and apply rankhospital function
        output <- sapply(states, rankhospital, outcome, num)
        output <- as.data.frame(output)
        output$state <- states
        colnames(output) <- c("hospital", "state")
        output
}