## Hanyu Wang
## Aug.1st
## Coursera R programming Assignment 3 Hospital Quality
## Part2: Finding the best hospital in a state

best <- function(state, outcome) {
        ## Read outcome data
        hlist = read.csv("outcome-of-care-measures.csv")
        # head(hlist[1:5,1:5])
        
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
        # print(head(hStList[c(2,7),]))

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
        ## outcomeList = c("heart attack", "heart failure", "pneumonia")
        ## if(!is.element(outcome, outcomeList)){
        ##        stop("invalid outcome")
        ##}
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        CPLocList = subset(ocList, ocList[,3] != "Not Available")
        CPLocList[,3] = as.numeric(as.character(CPLocList[,3]))
        sortList = CPLocList[order(CPLocList[,3], CPLocList[,1]), ]
        return (as.character(sortList[1,1]))
}