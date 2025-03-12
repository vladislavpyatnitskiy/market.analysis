library("rvest") # Library

rus.hist.plt.mrk.cap <- function(x){ # Histogram with Market Cap values
  
  l <- NULL # Store data here
  
  for (m in 1:length(x)){ v <- x[m] # For each ratio get Smartlab HTML
  
    y<-read_html(sprintf("https://smart-lab.ru/q/shares_fundamental/?field=%s",
                         v)) %>% html_nodes('table') %>% .[[1]] %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    D <- NULL # Variable for Table with Name, Ticker and values
    
    for (n in 0:(length(y)/6)){ D <- rbind(D, cbind(y[(3+n*6)],y[(6+n*6)])) }
    
    D <- D[-nrow(D),] # Reduce last row
    D[,2] <- gsub('["\n"]', '', gsub('["\t"]', '', D[,2]))
    
    for (n in 1:length(D)){ if (isTRUE(grepl(" ", D[n]))){
      
        D[n] <- gsub(" ", "", D[n]) } } # Reduce gap in market cap
    
    colnames(D) <- c("Ticker", gsub("_", "/", toupper(x[m]))) # Column names
    
    if (is.null(l)){ l<-D } else { l<-merge(x=l,y=D,by="Ticker",all=T)} } #Join
    
  if (isTRUE(l[1,1] == "")){ l <- l[-1,] } # Reduce empty row
  
  rownames(l) <- l[,1] # Move tickers to row names
  
  l <- as.data.frame(l[,-1]) # Reduce excessive column with tickers
  
  for (n in 1:ncol(l)){ l[,n] <- as.numeric(l[,n]) } # Make data numeric
  
  l <- l[row.names(l) != "OMZZ", , drop = F][,1] # Drop False value
  
  hist(l, freq = T, xlab = "Market Cap in Billions of Roubles", border="white",
       las = 1, col = "steelblue", breaks=100, sub="Data Source: smart-lab.ru",
       main = "Distribution of Russian Companies by Market Cap") # Plot
  
  grid(nx = NULL, ny = NULL, col = "grey", lwd = 1) # Vertical lines
  
  abline(h = 0, col = "black") # Add vertical line at y = 0
  
  axis(side = 4, las = 2) # Set y-axis values
  
  par(mar = c(5, 5, 5, 5)) # Define borders of the plot
  
  box() # Define borders
}
rus.hist.plt.mrk.cap(x = "market_cap") # Test
