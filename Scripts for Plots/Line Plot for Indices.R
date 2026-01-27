lapply(c("moexer", "quantmod", "timeSeries"),require,character.only = T) # Libs

indices.plt <- function(us, rus, s=NULL, e=NULL){
  
  U <- NULL 
  R <- NULL 
  
  src <- "yahoo" # Source for all indices but Russian
  
  getData1 <- function(A, s, e) {
    if (is.null(s) && is.null(e)) return(getSymbols(A, src=src, auto.assign=F)) 
    if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
    if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
    return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
  }
  for (A in us){ U <- cbind(U, getData1(A, s, e)[,4]) 
  
    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        A, which(us == A), length(us) + length(rus)
      )
    )
  } # Join data
  
  getData2 <- function(A, s, e) { 
    if (is.null(s) && is.null(e))
      return(get_candles(A, from = "2007-07-20", interval = 'daily')) 
    if (is.null(e)) return(get_candles(A, from = s, interval = 'daily')) 
    if (is.null(s)) return(get_candles(A, till = e, interval = 'daily')) 
    return(get_candles(A, from = s, till = e, interval = 'daily')) 
  }
  for (A in rus){ D <- as.data.frame(getData2(A, s, e)[,c(3,8)])
      
    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        A, length(us) + which(rus == A), length(us) + length(rus)
      )
    )
  
    D <- D[!duplicated(D),] # Remove duplicates
    
    R <- cbind(R, xts(D[, 1], order.by = as.Date(D[, 2]))) }
    
  DF <- cbind(U, R) # Join
  
  DF <- DF[apply(DF, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(DF) <- c(us, rus) # Assign column names
  
  DF <- apply(diff(log(as.timeSeries(DF)))[-1,], 2,
              function(col) exp(cumsum(col)) - 1) * 100
  
  par(mar = c(8, 2.5, 4, 2.5)) # Define borders of the plot
  
  plot(
    DF[,1], 
    ylim = c(min(DF), max(DF)),
    lty = 1, 
    type = "l", 
    lwd = 2,
    las = 1, 
    xlab = "Trading Days", 
    ylab = "Returns (%)",
    main = "Performance of Major Benchmarks"
    )
  
  axis(side = 4, las = 2) # Right Y-Axis Values
  
  grid(nx = 1, ny = NULL, lty = 3, col = "grey") # Horizontal lines
  
  abline(h = 0) # Add black horizontal line at break even point
  
  for (n in 2:(ncol(DF))){ lines(DF[,n], col = n, lwd = 2) } # Plot indices
  
  legend(
    x = "bottom", 
    inset = c(0, -0.2), 
    legend = colnames(DF), 
    xpd = T,
    col = seq(ncol(DF)), 
    lwd = 3, 
    cex = .75, 
    bty = "n", 
    horiz = T
    )
  
  on.exit(par(par(no.readonly = T))) # Show legend with names
}
indices.plt(c("^GSPC", "^DJI", "^IXIC", "^FTSE", "^FCHI", "^GDAXI"),
            rus = "RTSI", s = "2022-01-01")
