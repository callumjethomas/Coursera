complete <- function(directory, id = 1:332){
        # directory : string containing the path
        # id : integer vector containing monitor IDs
        
        # DEBUG:
        directory <- "C:/Users/callu/Desktop/specdata"
        id <- 1:332
        
        #  Get files from directory.
        filelist <- dir(directory)
        # Get range indicated by 'id'
        range <- id[1]:id[length(id)]
        # For each file in the directory up in the range indicated by 'id',
        for(file in filelist[range]) {
                # Create the path by pasting the directory and file name together.
                path <- paste(directory, file, sep = "/")
                # Read the data.
                data <- read.csv(path, header = TRUE, sep = ",", 
                                 colClasses = c("Date", "numeric", "numeric", 
                                                "numeric"))