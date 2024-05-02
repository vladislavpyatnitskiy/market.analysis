library("rvest") # Library

finviz.sectors.dividends.bar <- function(x){ # Bar Plot with Dividend Yields
  
  s <- read_html(sprintf("https://finviz.com/%s", x))
  
  s.yahoo <- s %>% html_nodes('table') %>% .[[8]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  d <- NULL # Data Frame with values
  
  for (n in 0:(length(y) / 11)){ d<-rbind(d,cbind(y[(2+n*11)],y[(5+n*11)])) }
  
  d <- d[-nrow(d),] # Reduce last column
  
  rownames(d) <- d[,1] # Assign row names
  
  d <- subset(d, select = -c(1)) # Reduce excessive column
  
  colnames(d) <- c("Dividend Yield (%)") 
  
  for (n in 1:nrow(d)){ # Reduce "%" from Market Cap column values
    
    d[n,1] <- read.fwf(textConnection(d[n,1]), widths = c(nchar(d[n,1]) - 1, 1),
                       colClasses = "character")[,1] }
  
  d <- as.data.frame(d) # Transform to data frame
  
  for (n in 1:ncol(d)){ d[,n] <- as.numeric(d[,n]) } # Make data numeric
  
  tickers <- rownames(d)
  
  d <- d[,1] 
  
  names(d) <- tickers
  
  d <- sort(d, decreasing = F)
  
  mx <- ceiling(round(max(d)) / 10 ^ (nchar(round(max(d))) - 1)) *
    10 ^ (nchar(round(max(d))) - 1) # Round maximum value up
  
  mn <- trunc(round(min(d)) / 10 ^ (nchar(round(min(d))) - 1)) *
    10 ^ (nchar(round(min(d))) - 1) # Round maximum value up
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
        "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
        "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
        "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
        "#895c8b","#bd5975")
  
  B <- barplot(d, names.arg = names(d), horiz = T, las = 1, xlim = c(0, mx + 1),
               xpd = F, col = C, main = "Sectors by Dividend Yield (%)")
  
  p.seq <- seq(0, mx + 1, by = 0.5) # Values for axes
  
  for (n in p.seq){ abline(v = n, col ="grey",lty = 3) } # Put horiz lines
  abline(h = B, col ="grey",lty = 3) # Put vertical lines
  abline(v = mean(d), col = "red", lwd = 3) # Mean percentage line
  abline(v = median(d), col = "green", lwd = 3) # Median percentage line
  
  axis(side = 1, at = seq(from = .5, to = mx + 1, by = 1))
  
  par(mar = c(4, 11, 4, 4)) # Define borders of the plot
  
  legend(x = "bottom", inset = c(0, -.22), cex = .85, bty = "n", horiz = T,
         legend = c((sprintf("Mean: %s", round(mean(d), 2))),
                    sprintf("Median: %s", round(median(d), 2))),
         col = c("red", "green"), xpd = T, pch = 15)
  
  box() # Make borders for plot
}
finviz.sectors.dividends.bar("groups.ashx?g=sector&v=110&o=name") # Test
