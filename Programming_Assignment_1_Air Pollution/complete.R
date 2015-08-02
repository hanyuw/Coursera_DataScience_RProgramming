## Hanyu Wang
## July.21
## Coursera R programming Assignment 1 Air Pollution
## Part2: complete.R

complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        completeRows = data.frame(id = integer(), nobs = integer())
        ## Search for each files in the given range id, then count for the nobs, and 
        ## finally adds it the the table one by one.
        for (i in id){
                fileProcessed = readFile(directory, i)
                nobsCount = countNobs(fileProcessed)
                completeRows = rbind(completeRows, data.frame(id = i, nobs = nobsCount))
                ## notice adding a row to the empty dataframe has to be in this form, otherwise 
                ## the table name will be messed up.
                
        } 
        return(completeRows);
}

## Return each file in the range, respectively
readFile <- function(directory, j) {
        fileName = paste(directory, "/", formatC(j, width = 3, flag = 0), ".csv", sep = "")
        return (read.csv(fileName));
}

## Count the complete rows in the selected file.
countNobs <- function(fileProcessed) {
        nobsRows = complete.cases(fileProcessed)
        nobs = length(subset(nobsRows, nobsRows == TRUE))
        return (nobs);
}