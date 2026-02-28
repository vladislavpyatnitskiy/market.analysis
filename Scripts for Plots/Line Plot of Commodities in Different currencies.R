lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

c.currency.converter.plt <- function(x, y, s = NULL, e = NULL, main = NULL){
  
  x <- c(x, y) # Join values
  
  p <- NULL # Create empty variable to contain data
  src <- "yahoo"
  
  getData <- function(A, s, e) {
    if (is.null(s) && is.null(e)) return(getSymbols(A, src=src, auto.assign=F)) 
    if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
    if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
    return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
  }
  for (A in x){ p <- cbind(p, getData(A, s, e)[,4]) 

    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        A, which(x == A), length(x)
      )
    ) # Download message
               
  } # Join data
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Put the tickers in data set
  
  p <- as.timeSeries(p) # Make it time series
  
  C <- p[,1] * p[,2] # New column
  
  colnames(C) <- sprintf("%s in %s", x[1], x[2]) # Column name
  
  if (is.null(main)){ main = sprintf("%s in %s", x[1], x[2]) }

  par(mar = rep(5, 4)) # Define borders of the plot
               
  plot(C, las = 1, xlab = "Trading Days", ylab = y, main = main) # Plot
  
  axis(side = 4, las = 2) # Right side y-axis values
  
  grid(nx = 1, ny = NULL, lty = 3, col = "grey") # Horizontal lines
  abline(h = 0)                              
}
c.currency.converter.plt(x = "BZ=F", y = "RUB=X") # Test
