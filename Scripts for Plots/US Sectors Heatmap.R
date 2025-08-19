lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libs 

sector.correlation <- function(data=T, s=NULL, e=NULL, lg=T, size=2){
  
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
    for (A in x){ p <- cbind(p, getData1(A, s, e)[,4]) } # Join data
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Eliminate NAs
    
    colnames(p) <- y
    
    x <- p } # Give column names 
    
  if (lg | data) { x <- diff(log(as.timeSeries(x)))[-1,] }
  
  m.correlation = as.matrix(x) # Convert data into matrix
  
  c.correlation = ncol(m.correlation) # Get number of columns
  
  new_cor <- cor(m.correlation) # Calculate correlation coefficients
  
  # Create appropriate colour for each pair of correlation for heatmap
  k.c <- round((10 * length(unique(as.vector(new_cor))))/2)
  corrColorMatrix <- rgb(c(rep(0, k.c), seq(0, 1, length = k.c)),
                         c(rev(seq(0,1,length=k.c)), rep(0,k.c)), rep(0,2*k.c))
  # Display heatmap
  image(x = 1:c.correlation,y = 1:c.correlation,z = new_cor[, c.correlation:1],
        col = corrColorMatrix, axes = FALSE, main = "", xlab = "", ylab = "")
  
  # Add labels for both axis
  axis(2, at = c.correlation:1, labels = colnames(m.correlation), las = 2)
  axis(1, at = 1:c.correlation, labels = colnames(m.correlation), las = 2)
  
  title(main = "Heatmap of US Sector Correlations") # title
  
  box() # Box heatmap
  
  # Add correlation values as text strings to each heatmap cell
  x = y = 1:c.correlation
  n_x = n_y = length(y)
  xoy = cbind(rep(x, n_y), as.vector(matrix(y, n_x, n_y, byrow = TRUE)))
  corr.coord = matrix(xoy, n_x * n_y, 2, byrow = FALSE)
  X.corr = t(new_cor)
  for (i in 1:c.correlation ^ 2) {
    text(corr.coord[i, 1], corr.coord[c.correlation ^ 2 + 1 - i, 2],
         round(X.corr[corr.coord[i,1],corr.coord[i,2]],digits=2),col = "white",
         cex=size) }
  
  par(mar = rep(8, 4)) # Define borders of the plot
}
sector.correlation(T)
