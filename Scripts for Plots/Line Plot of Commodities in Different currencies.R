lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

c.currency.converter.plt <- function(x, y, s = NULL, e = NULL){
  
  x <- c(x, y)
  
  p <- NULL # Create empty variable to contain data
  
  for (A in x){ if (is.null(s) && is.null(e)) { 
    
      q <- getSymbols(A, src = "yahoo", auto.assign = F)
    
    } else if (is.null(e)){ q <- getSymbols(A,from=s,src="yahoo",auto.assign=F)
    
    } else if (is.null(s)){ q <- getSymbols(A, to=e, src="yahoo",auto.assign=F)
    
    } else { q <- getSymbols(A, from = s, to = e, src="yahoo", auto.assign=F) }
      
    p <- cbind(p, q[,4]) } # Join all columns into one data frame
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Put the tickers in data set
  
  p <- as.timeSeries(p) # Make it time series
  
  C <- p[,1] * p[,2] # New column
  
  colnames(C) <- sprintf("%s in %s", x[1], x[2]) # Column name
  
  plot(C, las = 1, xlab = "Trading Days", ylab = y,
       main = sprintf("%s in %s", x[1], x[2])) # Plot
}
c.currency.converter.plt(x = "BZ=F", y = "RUB=X") # Test
