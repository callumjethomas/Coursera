corr <- function(directory, threshold = 0){ 
        # directory : string containing the path
        # threshold : numeric, number of complete observations required to calculate correlation
        
        #  Get files from directory.
        filelist <- dir(directory)
        for(file in filelist) {
                # Create the path by pasting the directory and file name together.
                path <- paste(directory, file, sep = "/")
                # Read the data.
                data <- read.csv(path, header = TRUE, sep = ",", 
                                 colClasses = c("Date", "numeric", "numeric", 
                                                "numeric"))
        }
}