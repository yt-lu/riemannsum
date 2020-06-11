# Yuanting Lu
# Draft: 11/16/2018
# Update: 11/17/2018

curlparentheses <- function(x, prefix){
  
  if (x == strsplit(x, prefix, fixed = TRUE)){
    # If x does not contain the prefix
    return(x)
  }else{
    # Separate all the individual char. Type is list.
    xlist <- strsplit(x, "")
    
    m <- length(strsplit(prefix,"")[[1]]) # Length of prefix
    n <- length(xlist[[1]]) # Length of the input string
    
    # Status of forward parenthesis in sequence
    # y for curl, n for normal
    status <- list() 
    
    for (i in 1:n){
      
      # List to string. ahead: 
      # i < m+1: all chars up to i 
      # i > m: previous m chars before i
      if (i <= m) ahead <- paste(unlist(xlist)[1:i],collapse = "")
      else ahead <- paste(unlist(xlist)[(i-m):(i-1)],collapse = "")
      
      if(xlist[[1]][i] == "(" & ahead == prefix){
        xlist[[1]][i] <- "{"
        status <- c(status, 'y') # parenthesis changed 
      }else if(xlist[[1]][i] == "(" & ahead != prefix){
        status <- c(status, 'n') # no change to parenthesis
      }else if(xlist[[1]][i] == ")"){
            if (tail(status,1) == 'y') xlist[[1]][i] <- "}"
            else NULL
            status <- head(status,-1) # Knock out the last status 
      }else NULL     
    }#END OF FOR-LOOP:I
    return(paste(unlist(xlist), collapse = ""))
  }#END OF ELSE
}#END OF FUNCTION