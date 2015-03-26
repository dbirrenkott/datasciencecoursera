pollutantmean <- function(directory, pollutant, id = 1:332) {
        files_list <- list.files(directory, pattern="*.csv", full.names = TRUE)        
        inputValueTotal = c()
        for (i in 1:NROW(id)){
                currentValue <- id[i]
                RawCSV <- read.csv(files_list[currentValue])
                if (pollutant == "sulfate") {
                        inputValue <- RawCSV$sulfate
                } else if (pollutant == "nitrate") {
                        inputValue <- RawCSV$nitrate
                }
                inputValueTotal <- c(inputValueTotal, inputValue)
        }
        print(inputValueTotal)       
        return(mean(inputValueTotal, na.rm=TRUE))
}