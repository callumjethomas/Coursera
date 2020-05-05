corr <- function(directory, threshold = 0){ 
        # directory : string containing the path
        # threshold : numeric, number of complete observations required to calculate correlation
        
        # Create empty vector
        cor_vec <- c()
        
        # Get number of observations per monitor using "complete" function.
        n_obs <- complete(directory)
        
        # Get list of monitors with observations over the threshold.
        obs_thr <- n_obs[which(n_obs$nobs > threshold),]
        mon_list <- obs_thr$id
        
        filelist <- dir(directory)
        # For each file in the directory included in the monitor list,
        for(file in filelist[mon_list]) {
                # Create the path by pasting the directory and file name together.
                path <- paste(directory, file, sep = "/")
                # Read the data.
                data <- read.csv(path, header = TRUE, sep = ",", 
                                 colClasses = c("Date", "numeric", "numeric", 
                                                "numeric"))
                # Strip missing values.
                data <- data[complete.cases(data), ]
                # Get correlation between pollutants.
                correlat <- cor(data$sulfate, data$nitrate)
                # Add correlation to vector.
                cor_vec <- c(cor_vec, correlat)
        }
        # Output vector.
        cor_vec

}
