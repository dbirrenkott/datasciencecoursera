corr <- function(directory, threshold = 0) {
        files_list <- list.files(directory, pattern="*.csv", full.names = TRUE)
        correlationAllValue = c()
        for (i in 1:NROW(files_list)){
                RawCSV <- read.csv(files_list[i])
                completeCasesLogical <- complete.cases(RawCSV)
                completeCasesValue <- sum(completeCasesLogical)
                if (completeCasesValue > threshold) {
                        completeCSV <- RawCSV[complete.cases(RawCSV),]
                        correlation <- cor(completeCSV[2],completeCSV[3])
                        correlationSingleValue <- correlation[1]
                        correlationAllValue <- c(correlationAllValue, correlationSingleValue)
                        
                }
        }
        #correlationAllValue <- round(correlationAllValue, 5)
        return(correlationAllValue)
}