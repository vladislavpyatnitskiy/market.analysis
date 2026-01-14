lapply(c("quantmod", "timeSeries"), require, character.only = T)

line.plt.sector <- function(data=T, s=NULL, e=NULL){
  
  x <- c("XLC","XLY","XLP","XLE","XLF","XLV","XLI","XLK","XLB","XLRE","XLU")
  
  y <- c("Communications", "Consumer Disc.", "Consumer Staples",
         "Energy", "Financials", "Health Care", "Industrials", "IT",
         "Materials", "Real Estate", "Utilities")
  
  if (data){ p <- NULL # data off
  
    src <- "yahoo" # Source for all indices but Russian
    
    getData1 <- function(A, s, e) {
      if (is.null(s) && is.null(e)) return(getSymbols(A,src=src,auto.assign=F)) 
      if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
      if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
      return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
    }
    for (A in x){ p <- cbind(p, getData1(A, s, e)[,4]) 
    
      message(
        sprintf(
          "%s is downloaded (%s / %s)", 
          A, which(x == A), length(x)
        )
      ) # Download message
      
    } # Join data
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Eliminate NAs
    
    colnames(p) <- y
    
    DF <- p } # Give column names 
  
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
    main = "Performance of US Sectors"
  )
  
  axis(side = 4, las = 2) # Right Y-Axis Values
  
  grid(nx = 1, ny = NULL, lty = 3, col = "grey") # Horizontal lines
  
  abline(h = 0) # Add black horizontal line at break even point
  
  for (n in 2:(ncol(DF))){ lines(DF[,n], col = n, lwd = 2) } # Plot indices
  
  legend(
    x = "bottom",
    inset = c(0, -.2),
    legend = colnames(DF),
    xpd = T,
    col = seq(ncol(DF)),
    lwd = 2,
    cex = .65,
    bty = "n",
    horiz = F,
    ncol = 6
  )
  
  on.exit(par(par(no.readonly = T))) # Show legend with names
}
line.plt.sector(T)
