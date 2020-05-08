rankhospital <- function(state, outcome, num){
        
        # DEBUG
        state <- "FL"
        outcome <- "heart attack"
        num <- 1
        
        # Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character")
        
        # Subset by state
        subdata <- subset(data, data$State == state)
        
        # Throw error if subsetting by state returns no rows (invalid)
        if (nrow(subdata) == 0) {
                stop("invalid state") 
                
        # Subset by outcome, select hospital name and relevant 30-day death column
        } else if (outcome == "heart attack") {
                subdata <- subset(subdata, select = c(2, 11))
                
        } else if (outcome == "heart failure") {
                subdata <- subset(subdata, select = c(2, 17)) 
                
        } else if (outcome == "pneumonia") {
                subdata <- subset(subdata, select = c(2, 23)) 
        
        # Throw error if neither of the three possible outcomes are input
        } else {
                stop("invalid outcome")
                
        }
        
        # Make the 30-day death data numeric (suppress NA warning)
        suppressWarnings(subdata[,2] <- as.numeric(subdata[,2]))
        
        # Remove rows with missing values
        clean <- subdata[complete.cases(subdata), ]
        
        # Sort by 30-day deaths (low to high) and then alphabetically by hospital name
        sorted <- clean[order(clean[, 2], clean[, 1]), ]
        
        # Return list indexed by num
        if (num == "best") {
                # Get hospital name in state with lowest 30-day death rate for the outcome
                r <- sorted[1, 1] 
        } else if (num == "worst") {
                # Get hospital name in state with highest 30-day death rate for outcome
                n <- nrow(sorted)
                r <- sorted[n, 1]
        } else {
                # Get hospital name in state with 30-day death rate for outcome at rank equal to num
                r <- sorted[num, 1]
        }
        
        # Return selected hospital name
        r
}