lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

ratio.plt <- function(x, y, s=NULL, e=NULL, main=NULL, ylab=NULL){
  
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
  
  C <- p[,2] / p[,1] # New column
  
  if (!is.null(rownames(C[C < 0,]))){ m <- rownames(C[C < 0,])
  
    C <- as.data.frame(C[apply(C, 1, function(row) all(row > 0))])
    
    R <- rownames(p)
    
    rownames(C) <- R[-grep(m, R)] }
  
  C <- as.timeSeries(C)
  
  colnames(C) <- sprintf("%s to %s", x[2], x[1]) 
  
  if (is.null(main)) main = sprintf("Ratio of %s to %s", x[2], x[1]) 
  
  if (is.null(ylab)) ylab = sprintf("%s to %s", x[2], x[1])
  
  plot(
    C,
    las = 1,
    xlab="Trading Days",
    ylab=ylab,
    main=main
  ) # Plot
  
  axis(side = 4, las = 2) # Right side y-axis values
  
  par(mar = rep(5, 4)) # Define borders of the plot
  
  grid(nx = 1, ny = NULL, lty = 3, col = "grey") # Horizontal lines
  abline(h = 1)  
}
ratio.plt("BZ=F", "CL=F")
