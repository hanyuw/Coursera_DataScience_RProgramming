## Hanyu Wang
## July.20
## Coursera R programming Assignment 1 Air Pollution
## Part1: pollutantmean


pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        ## Read tables and calculates its mean
        table = readFiles(directory, id)
        mean(table[ ,pollutant], na.rm = TRUE)
}

## Creating a dataframe that contains all the table that has been selected
readFiles <- function(directory = "specdata", id){
        files = data.frame(Date = factor(), sulfate = double(), nitrate = double(), ID = integer())
        for(j in id){
                newFile = readFile(directory, j)
                files = rbind(files, newFile)
        }
        return (files);
}

## Read the table by the given directory and each id in the id range
readFile <- function(directory, i){
        filePath = paste(directory, "/", formatC(i, width = 3, flag = 0), ".csv", sep = "")
        file = read.csv(filePath)
        cleanFile = file[complete.cases(file), ]
        return (cleanFile);
}