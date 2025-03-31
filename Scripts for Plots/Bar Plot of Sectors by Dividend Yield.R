library("rvest") # Library

finviz.sectors.dividends.bar <- function(x){ # Bar Plot with Dividend Yields
  
  y <- read_html(sprintf("https://finviz.com/%s", x)) %>%
    html_nodes('table') %>% .[[8]] %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text()
  
  d <- data.frame(y[seq(from = 2, to = length(y), by = 11)],
                  y[seq(from = 5, to = length(y), by = 11)])
  
  rownames(d) <- d[,1] # Assign row names
  
  d <- subset(d, select = -c(1)) # Reduce excessive column
  
  colnames(d) <- c("Dividend Yield (%)") # Column name
  
  for (n in 1:nrow(d)){ d[n,1] <- read.fwf(textConnection(d[n,1]),
                                           widths = c(nchar(d[n,1]) - 1, 1),
                                           colClasses = "character")[,1]
  }
  d <- as.data.frame(d) # Transform to data frame
  
  for (n in 1:ncol(d)){ d[,n] <- as.numeric(d[,n]) } # Make data numeric
  
  tickers <- rownames(d)
  
  d <- d[,1] 
  
  names(d) <- tickers
  
  d <- sort(d, decreasing = F) # Sort in a descending way
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a")
  
  B <- barplot(d, names.arg = names(d), horiz=T, las=1, xlim = c(0, max(d)+1),
               xpd = F, col = C, main = "Sectors by Dividend Yield (%)")
  
  grid(nx = NULL, ny = 1, lty = 3, col = "grey") # Horizontal lines
  abline(h = B, col ="grey",lty = 3) # Vertical lines
  abline(v = mean(d), col = "red", lwd = 3) # Mean percentage line
  abline(v = median(d), col = "green", lwd = 3) # Median percentage line
  
  par(mar = c(4, 11, 6, 4)) # Define borders of the plot
  
  legend(x = "bottom", inset = c(0, -.19), cex = .85, bty = "n", horiz = T,
         legend = c((sprintf("Mean: %s", round(mean(d), 2))),
                    sprintf("Median: %s", round(median(d), 2))),
         col = c("red", "green"), xpd = T, pch = 15)
  
  box() # Make borders for plot
}
finviz.sectors.dividends.bar("groups.ashx?g=sector&v=110&o=name") # Test
