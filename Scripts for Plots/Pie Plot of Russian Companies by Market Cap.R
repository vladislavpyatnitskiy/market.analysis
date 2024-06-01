library("rvest") # Library

smartlab.pie.plt.marketcap <- function(y){ # Portfolio Securities by Market Cap
  
  l <- NULL # Store data here
  
  for (m in 1:length(y)){ v <- y[m] # For each ratio get Smartlab HTML
  
    s<-read_html(sprintf("https://smart-lab.ru/q/shares_fundamental/?field=%s",
                         v))
    
    tab <- s %>% html_nodes('table') %>% .[[1]]
    
    d <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    D <- NULL # Variable for Table with Name, Ticker and values
    
    for (n in 0:(length(d)/6)){ D <- rbind(D, cbind(d[(3 + n * 6)],
                                                    d[(6 + n * 6)])) }
    D <- D[-nrow(D),] # Reduce last row
    D[,2] <- gsub('["\n"]', '', gsub('["\t"]', '', D[,2])) # Reduce \,n,t
    
    for (n in 1:length(D)){ if (isTRUE(grepl(" ", D[n]))){
      
        D[n] <- gsub(" ", "", D[n]) } } # Reduce gap in market cap
    
    colnames(D) <- c("Ticker", gsub("_", "/", toupper(y[m]))) # Column names
    
    if (is.null(l)){ l<-D } else { l<-merge(x=l,y=D,by="Ticker",all=T)} }#Join
    
  if (isTRUE(l[1,1] == "")){ l <- l[-1,] } # Reduce empty row
  
  rownames(l) <- l[,1] # Move tickers to row names
  
  l <- as.data.frame(l[,-1]) # Reduce excessive column with tickers
  
  for (n in 1:ncol(l)){ l[,n] <- as.numeric(l[,n]) } # Make data numeric
  
  tickers <- rownames(l)[2:length(rownames(l))] # Assign tickers as variable
  
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
  
  df <- aggregate(MC ~ Level, data=df, sum) # Conditional sum & Colours
  
  S <- sum(df[,2]) # Sum 
  
  for (n in 1:nrow(df)){ df[n,2] <- round(df[n,2] / S * 100, 2) }
  
  C = c("#466791","#df462a","#4fbe6c","#ce49d3","#a7b43d","#5a51dc")

  pie(df[,2], labels=c(sprintf("%s %s%%", df[,1], df[,2])), col=C, radius=2.5,
      main = "Securities by Market Capitalisation Level in Russia") # Plot
}
smartlab.pie.plt.marketcap(y=c("market_cap")) # Test
