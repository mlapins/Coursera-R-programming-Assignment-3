best <- function(state, outcome) {
    
    ## Read outcome data
    
    data <- read.csv("outcome-of-care-measures.csv",  colClasses = "character")
    
    
    ## Check that state and outcome are valid
    ## Alternatively, get unique values from the states column using unique
    ## and check against a smaller set of items. This functions uses the subset
    ## instead.
    
    #check.states <- unique(rd_file$State)
    
    if(!(state %in% data$State)){
        
        stop ("Invalid state")
    }
    
    validOutcome = c("heart attack","heart failure","pneumonia")
    
    if (!outcome %in% validOutcome) { 
        
        stop("invalid outcome")
        
    }
    
    ## Get outcomes "heart attack", "heart failure", or "pneumonia"
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    
    
    sort_by_column_NA <- function(data,col1, col2){
        for (i in length(data)){
            data[,i] <- suppressWarnings(as.numeric(levels(data[,i])[data[,i]]))
        }
        
        orderdata <- sub.data[order(sub.data[,col1],sub.data[,col2]),]
        orderdata <- orderdata[complete.cases(orderdata),] 
        orderdata <- orderdata[[1]]
        return(orderdata)
    }
    
    
    if (outcome == "heart attack"){
        
        #subset data from with only columns we need - Hospital (2), state(7), mortality (11)
        
        
        
        #get col index of mortality rate col
        
        #get col index of hospital name col
        
        
        #get min vlue of mortality rate col
        #sub.datamin.value <- min(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm=TRUE)
        sub.data <- data[with(data, State == state),  c(2,7,11)]
        
        min.value <- min(data[ ,11], na.rm=TRUE)
        
        sub.data <- sub.data[with(sub.data, sub.data[ ,3] == min.value),]
        
        #once got indexed and min number sort and return
        
        result <- sort_by_column_NA(data,1,3)
        
    }else if (outcome == "heart failure"){
        
        sub.data <- data[with(data, State == state), c(2,7,17)]
        
        min.value <- min(data[ ,11], na.rm=TRUE)
        
        sub.data <- sub.data[with(sub.data, sub.data[ ,3] == min.value),]
        
        #once got indexed and min number sort and return
        
        result <- sort_by_column_NA(data,1,3)
        
    }else if (outcome == "pneumonia"){
        sub.data <- data[with(data, State == state), c(2,7,23)]
        
        min.value <- min(data[ ,11], na.rm=TRUE)
        
        sub.data <- sub.data[with(sub.data, sub.data[ ,3] == min.value),]
        
        #once got indexed and min number sort and return
        
        result <- sort_by_column_NA(data,1,3)
        
    }
    
    return(result)
    
}    