## Hanyu Wang
## Aug.1st
## Coursera R programming Assignment 3 Hospital Quality
## Part4: Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        hlist = read.csv("outcome-of-care-measures.csv")
        
        ## Check wether the state and outcome are valid
        if(outcome == "heart attack"){
                ocList = hlist[,c(2,7,11)]
        } else if(outcome == "heart failure"){
                ocList = hlist[,c(2,7,17)]
        } else if(outcome == "pneumonia"){
                ocList = hlist[,c(2,7,23)]
        } else {
                stop("invalid outcome")   
        }
        
        ## Furbish the table by removing the missing values and formating each columns. 
        CPLocList = subset(ocList, ocList[,3] != "Not Available")
        CPLocList[,1] = as.character(CPLocList[,1])
        CPLocList[,2] = as.character(CPLocList[,2])
        CPLocList[,3] = as.numeric(as.character(CPLocList[,3]))
        
        ## Sorting the List by alphabatical name
        CPLocList = CPLocList[order(CPLocList$State), ]
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        sortList = CPLocList[order(CPLocList[,3], CPLocList[,1]), ]
        stocList = split(sortList, sortList$State)
        
        
        ## Translating num and apply it to the ranking
        if(num == "best"){
                rankState = sapply(stocList, function(x) return(x[1,]))
        } else if (num == "worst"){
                rankState = sapply(stocList, function(x) return(x[nrow(x),]))
        } else {
                rankState = sapply(stocList, function(x) return(x[num,]))
        }
        
        ## I don't like this way to avoid the NA for state name.
        ## try to figure out something that is better
        StateList = CPLocList[,2][!duplicated(CPLocList[,2])]
        
        rankHospital =  data.frame(hospital = unlist(rankState[1,]), state = StateList)
        # rankHospital =  data.frame(hospital = unlist(rankState[1,]), state = unlist(rankState[2,]))
        
        return(rankHospital)
}