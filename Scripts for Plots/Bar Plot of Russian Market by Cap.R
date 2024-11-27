library("rvest") # Library

smartlab.bar.plt.marketcap <- function(y){ # Portfolio Securities by Market Cap
  
  l <- NULL # Store data here
  
  for (m in 1:length(y)){ v <- y[m] # For each ratio get Smartlab HTML
  
  d<-read_html(sprintf("https://smart-lab.ru/q/shares_fundamental/?field=%s",
                       v)) %>% html_nodes('table') %>% .[[1]] %>%
    html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  D <- NULL # Variable for Table with Name, Ticker and values
  
  for (n in 0:(length(d)/6)){ D <- rbind(D, cbind(d[(3+n*6)], d[(6+n*6)])) }
  
  D <- D[-nrow(D),] # Reduce last row
  D[,2] <- gsub('["\n"]', '', gsub('["\t"]', '', D[,2]))
  
  for (n in 1:length(D)){ if (isTRUE(grepl(" ", D[n]))){
    
      D[n] <- gsub(" ", "", D[n]) } } # Reduce gap in market cap
  
  colnames(D) <- c("Ticker", gsub("_", "/", toupper(y[m]))) # Column names
  
  if (is.null(l)){ l<-D } else { l <- merge(x=l,y=D,by="Ticker",all=T)} }# Join
  
  if (isTRUE(l[1,1] == "")){ l <- l[-1,] } # Reduce empty row
  
  rownames(l) <- l[,1] # Move tickers to row names
  
  l <- as.data.frame(l[,-1]) # Reduce excessive column with tickers
  
  for (n in 1:ncol(l)){ l[,n] <- as.numeric(l[,n]) } # Make data numeric
  
  tickers <- rownames(l)[2:length(rownames(l))]
  
  l <- l[row.names(l) != "OMZZ", , drop = F][,1] # Drop False value
  
  l <- as.data.frame(l)
  
  rownames(l) <- tickers
  
  M <- NULL # Assign values for Market Cap Values
  
  for (n in 1:nrow(l)){ m <- l[n,1] # Micro, Small, Mid, Large and Mega Caps
  
    if (m < 1){ M <- rbind.data.frame(M, "Micro-Cap") } # 1
    
    else if (m > 1 && m < 10) { M <- rbind.data.frame(M, "Small-Cap") } # 2
    
    else if (m > 10 && m < 100) { M <- rbind.data.frame(M, "Mid-Cap") } # 3
    
    else if (m > 100 && m < 1000) { M <- rbind.data.frame(M, "Large-Cap") } # 4
    
    else { M <- rbind.data.frame(M, "Mega-Cap") } } # 5
      
  rownames(M) <- rownames(l)
  colnames(M) <- "Level" # Column Name
  
  df <- cbind.data.frame(M, l) # Join
  
  colnames(df)[2] <- "MC" # Assign column name for numeric values
  
  df <- aggregate(MC ~ Level, data=df, length) # Conditional sum & Colours
  
  k <- c("Micro-Cap", "Small-Cap", "Mid-Cap", "Large-Cap", "Mega-Cap")
  
  df <- df[match(k, df$Level),] # Change Order of Market Caps
  
  C = c("#466791","#df462a","#4fbe6c","#ce49d3","#a7b43d","#5a51dc") # Colours
  
  a <- round(min(df[,2]) * -1 + max(df[,2]), 0) # Difference between max & min
  
  a <- a / 10 ^ (nchar(a)) # Grey division lines 
  
  i <- c(0, 1, 2, 5) # Calculate intervals for lines and axes
  
  for (n in 1:length(i) - 1){ if (a > i[n] && a < i[n + 1]){
    
      mn <- i[n + 1] * 10 ^ (nchar(a) - 3) } else { next } }
  
  B <- barplot(df[,2], names.arg=df[,1], horiz=F, las=1, xpd=F, col=C,
               main = "Distribution of Public Companies by Market Cap Levels",
               sub = "Data Source: smart-lab.ru", ylim = c(0, max(df[,2]) + 1))
  
  axis(side = 4, las = 1, at = seq(0, 1000, mn)) # Values for Right Axis
  axis(side = 2, las = 1, at = seq(10, 1000, 2*mn)) # Values for Left Axis
  
  abline(v = B, col ="grey",lty = 3) # Put vertical lines
  abline(h = 0) # Horizontal line
  
  for (n in seq(10, 100, mn)){ abline(h = n, col = "grey", lty = 3) }
  
  par(mar = c(5, 5, 5, 5)) # Define borders of the plot
  
  box() # Borders
}
smartlab.bar.plt.marketcap(y=c("market_cap")) # Test
