

rankhospital <- function(state, outcome, num = "best") {
    
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
    
    ## Set index of mortality fields
    index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
    
    
    ## Prepare dataframe
    
    data <- na.omit(data)

    sub.data <- data[with(data, State == state),  c(2,7,as.numeric(index))]
    
    sub.data <- sub.data[ which( ! sub.data[,3] %in% "Not Available") , ]
    
    ## Order datafrme by decreasing mortality rate  
    sub.data <- sub.data[order(suppressWarnings(as.numeric(sub.data[,3])),suppressWarnings(as.numeric(sub.data[,1]))),]
 
    sub.data$Rank <- c(suppressWarnings(as.numeric(1:nrow(sub.data))))

     ## Add new column to dataframe
    sub.data$Rank <- sub.data

    get.last <- nrow(sub.data) #row length of dataframe
    
    ## Return hospital name in that state with lowest 30-day deat
    ## rate        
    
    if (num == "best" || num == 1){
        
        result <- sub.data[1,1]
        
    }else if (num == "worst"){
        
        result <- sub.data[get.last,1]
        
    }else { # Number rank
  
        ## Catch error
        
             for (i in 1:nrow(sub.data)){
                 

                if (sub.data$Rank[i,4] == as.numeric(num)){
    
                    result <- sub.data[i,1]
                    
                    break

                }else if (num > as.numeric(get.last)) {
                
                    result <- "NA"

                }
                

            }
        
    }    

    return(result)
 
}    