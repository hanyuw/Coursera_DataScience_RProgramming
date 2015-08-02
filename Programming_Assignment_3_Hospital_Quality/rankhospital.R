## Hanyu Wang
## Aug.1st
## Coursera R programming Assignment 3 Hospital Quality
## Part3: Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        hlist = read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
                # Validifying state
        stateList = unique(hlist$State)
        if(!is.element(state, stateList)){
                stop("invalid state")
        } else {
                # Unsolved Problem 
                # hStateList = split(hlist, hlist$State)$state
                hStList = subset(hlist, hlist$State == state)
                
        }

                # Validifying outcome
        if(outcome == "heart attack"){
                ocList = hStList[,c(2,7,11)]
        } else if(outcome == "heart failure"){
                ocList = hStList[,c(2,7,17)]
        } else if(outcome == "pneumonia"){
                ocList = hStList[,c(2,7,23)]
        } else {
                stop("invalid outcome")   
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        CPLocList = subset(ocList, ocList[,3] != "Not Available")
        CPLocList[,3] = as.numeric(as.character(CPLocList[,3]))
        sortList = CPLocList[order(CPLocList[,3], CPLocList[,1]), ]
        
        if(num == "best"){
                return(as.character(sortList[1,1]))
        } else if (num == "worst"){
                return(as.character(sortList[nrow(sortList),1]))
        } else if (num < nrow(sortList)){
                return(as.character(sortList[num,1]))
        } else {
                return (NA)
        }
}
