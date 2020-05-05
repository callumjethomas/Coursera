pollutantmean <- function(directory, pollutant, id = 1:332){
        # directory : string containing the path
        # pollutant : string naming the pollutant
        # id : integer vector containing monitor IDs
        
        # Create blank mean list.
        meanlist <- data.frame()
        #  Get files from directory.
        filelist <- dir(directory)
        # For each file in the directory up in the range indicated by 'id',
        for(file in filelist[id]) {
                # Create the path by pasting the directory and file name together.
                path <- paste(directory, file, sep = "/")
                # Read the data.
                data <- read.csv(path, header = TRUE, sep = ",", 
                                 colClasses = c("Date", "numeric", "numeric", 
                                                "numeric"))
                # Calculate the mean of non-missing values for the pollutant.
                id_mean <- mean(data[[pollutant]], na.rm = TRUE)
                # Calculate the number of non-missing values for the pollutant.
                id_sum <- sum(!is.na(data[[pollutant]]), na.rm = TRUE)
                # Multiply mean by number of observations (to calculate weighting).
                weight <- id_mean * id_sum
                # Bind all these columns together into one row.
                row <- cbind(id_mean, id_sum, weight)
                # Add this row to the mean list.
                meanlist <- rbind(meanlist, row)
                # Strip out missing values.
                meanlist <- meanlist[complete.cases(meanlist),]
        }
        # Divide the sum of all the weights by the sum of all the observations to obtain the weighted mean.
        sum(meanlist$weight)/sum(meanlist$id_sum)
}
