rankall <- function(outcome, num){
        
        # DEBUG
        outcome = "heart attack"
        num = "best"
        
        # Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character")
        
        # Get list of states
        states <- sort(unique(data$State))
                
        # Subset by outcome, select hospital name and relevant 30-day death column
        if (outcome == "heart attack") {
                subdata <- subset(data, select = c(2, 7, 11))
                
        } else if (outcome == "heart failure") {
                subdata <- subset(data, select = c(2, 7, 17)) 
                
        } else if (outcome == "pneumonia") {
                subdata <- subset(data, select = c(2, 7, 23)) 
                
                # Throw error if neither of the three possible outcomes are input
        } else {
                stop("invalid outcome")
                
        }
        
        # Make the 30-day death data numeric (suppress NA warning)
        suppressWarnings(subdata[,3] <- as.numeric(subdata[,3]))
        
        # Remove rows with missing values
        clean <- subdata[complete.cases(subdata), ]
        
        # Sort by 30-day deaths (low to high) and then alphabetically by hospital name
        sorted <- clean[order(clean[, 2], clean[, 3], clean[, 1]), ]
        
        # Initialise a data frame to store output in.
        output <- data.frame()
        
        # Loop through each state and get the hospital of desired rank
        for (i in states) {
                sub_sorted <- subset(sorted, sorted$State == i)
                # Return list indexed by num
                if (num == "best") {
                        # Get hospital name in state with lowest 30-day death rate for the outcome
                        r <- sub_sorted[1, 1] 
                } else if (num == "worst") {
                        # Get hospital name in state with highest 30-day death rate for outcome
                        n <- nrow(sub_sorted)
                        r <- sub_sorted[n, 1]
                } else {
                        # Get hospital name in state with 30-day death rate for outcome at rank equal to num
                        r <- sub_sorted[num, 1]
                }
                # Return selected hospital name and state and bind to data frame
                row <- (cbind(r, i))
                output <- rbind(output, row)
        }
        
        # Change column names in output data frame and return
        colnames(output) <- c("hospital", "state")
        output
}