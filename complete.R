complete <- function(directory, id = 1:332){
        # directory : string containing the path
        # id : integer vector containing monitor IDs
        
        # Create a blank data frame.
        n_obs <- data.frame()
        #  Get files from directory.
        filelist <- dir(directory)
        # For each file in the directory, in the range indicated by 'id',
        for(file in filelist[id]) {
                # Create the path by pasting the directory and file name together.
                path <- paste(directory, file, sep = "/")
                # Read the data.
                data <- read.csv(path, header = TRUE, sep = ",", 
                                 colClasses = c("Date", "numeric", "numeric", 
                                                "numeric"))
                # Get only complete cases from the data.
                compl <- data[complete.cases(data), ]
                # Count number of complete cases.
                count <- nrow(compl)
                # Create a row containing the id number and number of complete cases.
                row <- cbind(data$ID[1], count)
                # Add this row to the data frame.
                n_obs <- rbind(n_obs, row)
        }
        # Rename data frame columns and print.
        colnames(n_obs) <- c("id", "nobs")
        n_obs
}