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
  
  mx <- ceiling(round(max(d)) / 10 ^ (nchar(round(max(d))) - 1)) *
    10 ^ (nchar(round(max(d))) - 1) # Round maximum value up
  
  mn <- trunc(round(min(d)) / 10 ^ (nchar(round(min(d))) - 1)) *
    10 ^ (nchar(round(min(d))) - 1) # Round maximum value up
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a")
  
  B <- barplot(d,names.arg=names(d),horiz=T,las=1,xlim=c(mn, max(d)+1), xpd=F,
               col=C, main = "Sectors by Market Capitalisation in $Trillions")
  
  p.seq <- seq(round(min(d) - 1), mx, by = 1) # Values for axes
  
  for (n in p.seq){ abline(v = n, col ="grey", lty = 3) } # Put horiz lines
  abline(h = B, col = "grey", lty = 3) # Put vertical lines
  abline(v = mean(d), col = "red", lwd = 3) # Mean percentage line
  abline(v = median(d), col = "green", lwd = 3) # Median percentage line
  
  axis(side = 1, at = p.seq) # Assign values for horizontal axis
  
  par(mar = c(4, 11, 4, 4)) # Define borders of the plot
  
  legend(x="bottom",inset=c(0,-.17),cex=.9,bty="n",horiz=T,col=c("red","green"),
         legend=c((sprintf("Mean: %s",round(mean(d),2))),
                  sprintf("Median: %s",round(median(d),2))),xpd=T,pch=15)
  
  box() # Make borders for plot
}
finviz.sectors.marketcap("groups.ashx?g=sector&v=120&o=name") # Test
