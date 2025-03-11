library("rvest") # Library

finviz.sectors.marketcap <- function(x){ # Data Frame with info about sectors
  
  y <- read_html(sprintf("https://finviz.com/%s", x)) %>%
    html_nodes('table') %>% .[[8]] %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text()
  
  d <- data.frame(y[seq(from = 2, to = length(y), by = 15)],
                  y[seq(from = 3, to = length(y), by = 15)])
  
  rownames(d) <- d[,1] # Assign row names
  
  d <- subset(d, select = -c(1)) # Reduce excessive column
  
  colnames(d) <- c("Market Cap ($blns)") # Reduce "B" from Market Cap values
  
  for (n in 1:nrow(d)){ d[n,1] <- read.fwf(textConnection(d[n,1]),
                                           widths = c(nchar(d[n,1]) - 1, 1),
                                           colClasses = "character")[,1] }
  d <- as.data.frame(d) # Data frame
  
  for (n in 1:ncol(d)){ d[,n] <- as.numeric(d[,n]) } # Make data numeric
  
  tickers <- rownames(d) # Assign tickers
  
  d <- d[,1] / 1000 # Divide by 1,000 and change to numeric type
  
  names(d) <- tickers # Assign tickers to values
  
  d <- sort(d, decreasing = F) # Sort 
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a")
  
  B <- barplot(d,names.arg=names(d),horiz=T,las=1,xlim=c(0, max(d) + 1), xpd=F,
               col=C, main="US Sectors by Market Capitalisation in $Trillions")
  
  grid(nx = NULL, ny = 1, col = "grey", lwd = 1) # Vertical lines
  abline(h = B, col = "grey", lty = 3) # Horizontal lines
  
  vals = list(list(mean(d), median(d)), c("red", "green")) # Mean & Median 
  for (n in 1:2){ abline(v = vals[[1]][[n]], col = vals[[2]][n], lwd = 3) }
  
  par(mar = c(4, 11, 4, 4)) # Define borders of the plot
  
  legend(x="bottom", inset=c(0,-.22), cex=.9, bty="n", horiz=T, col=vals[[2]],
         legend=c((sprintf("Mean: %s",round(mean(d), 2))),
                  sprintf("Median: %s",round(median(d), 2))), xpd=T, pch=15)
  
  box() # Make borders for plot
}
finviz.sectors.marketcap("groups.ashx?g=sector&v=120&o=name") # Test
