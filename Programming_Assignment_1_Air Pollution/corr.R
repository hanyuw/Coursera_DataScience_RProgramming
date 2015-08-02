## Hanyu Wang
## July.21
## Coursera R programming Assignment 1 Air Pollution
## Part3: corr.R


corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        corSets = c()
        
        ## Stereotypical ways to read all files in a given directory.
        filesNameList = list.files(path = directory)    ## Get all names under in the given directory
        for(i in 1:length(filesNameList)){
                currentFileName = filesNameList[i];
                currentFilePath = paste(directory, "/", currentFileName, sep = "")
                currentFile = read.csv(currentFilePath)
                cleanFile = currentFile[complete.cases(currentFile), ]
                if(nrow(cleanFile) > threshold){         ## Find qualified file
                        cor = getCor(cleanFile)          ## Calculate for its correlation
                        corSets = c(corSets, cor)        ## Add this correlation to the list
                }
        }
        
        return(corSets)
        
}


## Counting for the correlation for qualified files, then returns it to the vector list.
getCor <- function(currentCleanFile){
        cleanFileCor = cor(currentCleanFile[,2], currentCleanFile[ ,3])
        return (cleanFileCor);
}