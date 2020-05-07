best <- function(state, outcome) {
        # DEBUG
        
        #outcome = "pneumonia"
        #state = "FL"
        
        # Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
        
        # Subset by state
        subdata <- subset(data, data$State == state)
        
        # Throw error if subsetting by state returns no rows (invalid)
        if(nrow(subdata) == 0)
                stop("invalid state") else
        
        # Subset by outcome, select hospital name and relevant 30-day death column
        if(outcome == "heart attack")
                subdata <- subset(subdata, select = c(2, 11)) else
                        
        if(outcome == "heart failure")
                subdata <- subset(subdata, select = c(2, 17)) else
                        
        if(outcome == "pneumonia")
                subdata <- subset(subdata, select = c(2, 23)) else
                        
                # Throw error if neither of the three possible outcomes are input
                stop("invalid outcome")
        
        # Make the 30-day death data numeric (suppress NA warning)
        suppressWarnings(subdata[,2] <- as.numeric(subdata[,2]))
        
        # Remove rows with missing values
        clean <- subdata[complete.cases(subdata), ]
        
        # Sort by 30-day deaths (low to high) and then alphabetically by hospital name
        sorted <- clean[order(clean[,2], clean[,1]),]
   
        # Return hospital name in state with lowest 
        # 30-day death rate for the outcome
        s <- sorted[1,1]
        s
}
