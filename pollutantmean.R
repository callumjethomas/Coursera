pollutantmean <- function(directory, pollutant, id = 1:332){
        directory <- "C:/Users/callu/Desktop/specdata"
        pollutant <- "sulfate"
        id <- 1:10
        i = id[1] - 1
        meanlist <- data.frame()
        filelist <- dir(directory)
        for(file in filelist[1:length(id)]) {
                i = i+1
                path <- paste(directory, file, sep = "/")
                data <- read.csv(path, header = TRUE, sep = ",", 
                                 colClasses = c("Date", "numeric", "numeric", 
                                                "numeric"))
                id_mean <- mean(data[[pollutant]], na.rm = TRUE)
                row <- cbind(i, id_mean)
                meanlist <- rbind(meanlist, row)
        }
        mean(meanlist$id_mean)
}
        # directory : string containing the path
        # pollutant : string naming the pollutant
        # id : integer vector containing monitor IDs
        