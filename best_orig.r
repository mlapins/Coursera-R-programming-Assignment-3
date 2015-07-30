

best <- function(state, outcome) {
    
    ## Read outcome data
    
    rd_file <- read.csv("outcome-of-care-measures.csv",  colClasses = "character")
    
    
    ## Check that state and outcome are valid
    ## Alternatively, get unique values from the states column using unique
    ## and check against a smaller set of items. This functions uses the subset
    ## instead.
    
    #check.states <- unique(rd_file$State)
    
       if(!(state %in% rd_file$State)){
        
        stop ("Invalid state")
    }

    validOutcome = c("heart attack","heart failure","pneumonia")
    
    if (!outcome %in% validOutcome) { 
        
        stop("invalid outcome")
        
        }
    
   ## Get outcomes "heart attack", "heart failure", or "pneumonia"

    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    if (outcome == "heart attack"){
        
        #subset by state
        state.list <- subset(rd_file, rd_file$State == state, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        
        colm <- min(state.list$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm=TRUE)
        
        get.hospital.name <- subset(state.list, state.list$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == colm, select = Hospital.Name)
                             
        hospital.name <- get.hospital.name[[1]]
          
    }else if (outcome == "heart failure"){
        
        state.list <- subset(rd_file, rd_file$State == state, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        
        colm <- min(state.list$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm=TRUE)
        
        get.hospital.name <- subset(state.list, state.list$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == colm, select = Hospital.Name)
        
        hospital.name <- get.hospital.name[[1]]
        
    }else if (outcome == "pneumonia"){
        
        state.list <- subset(rd_file, rd_file$State == state, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        
        order(state.list$Hospital.Name)
        
        colm <- min(as.numeric(state.list$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), na.rm=TRUE)
        
        get.hospital.name <- subset(state.list, state.list$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == colm)

        hospital.name <- get.hospital.name[[1,]]

    }
    

    return(hospital.name)
    
    
}

