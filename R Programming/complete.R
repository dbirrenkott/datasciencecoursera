complete <- function(directory, id = 1:332) {
        files_list <- list.files(directory, pattern="*.csv", full.names = TRUE)
        completeCasesValueList = c()
        for (i in 1:NROW(id)){
                currentValue <- id[i]
                RawCSV <- read.csv(files_list[currentValue])
                completeCasesLogical <- complete.cases(RawCSV)
                completeCasesValue <- sum(completeCasesLogical)
                nobs <- c(nobs, completeCasesValue)
        }
        complete.data <- data.frame(id,nobs)
        return(complete.data)
}