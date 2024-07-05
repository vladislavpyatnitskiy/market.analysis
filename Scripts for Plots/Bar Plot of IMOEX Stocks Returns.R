library(rvest) # Library

bar.plt.imoex <- function(x){ # Bar Plot with IMOEX Stocks Returns
  
  f <- read_html(x) %>% html_nodes('body') %>% html_nodes('table') %>%
    html_nodes('tr') %>% html_nodes('td') %>% html_text() # Read HTML
  
  l <- NULL # Get data of positive and negative returns
  
  for (n in 0:length(f)){ v <- gsub('["\n"]', '', gsub('["\t"]', '',f[7+17*n]))
    
    v <- as.character(read.fwf(textConnection(v), widths = c(nchar(v) - 1, 1),
                              colClasses = "character")[1])
    
    if (isTRUE(grepl("\\+", v))){ v <- as.numeric(gsub("\\+", "", v)) }
    
    l <- rbind.data.frame(l, cbind(f[2 + 17 * n], as.numeric(v))) }
  
  l <- l[apply(l, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(l) <- c("Компания", "%") # Column names
  
  l <- l[order(-as.numeric(l[,2])), ] # Order in a descending way
  
  tickers <- l[,1] # Tickers
  
  l <- as.numeric(l[,2]) # Returns values
  
  names(l) <- tickers # Assign tickers to returns
  
  p.seq <- seq(from = ceiling(min(l) * -1) * -1, to = ceiling(max(l)), by = 1)
  
  plt <- barplot(l,names.arg=names(l),main="IMOEX Stocks Returns for Today (%)", 
                 col = c(rep("green4", length(l) - sum(l < 0)),
                         rep("red3", sum(l < 0))), horiz = F, las = 2,
                 ylim = c(p.seq[1], p.seq[length(p.seq)]))
  
  abline(v = plt, col = "grey", lty = 3) # Vertical grey lines
  abline(h = p.seq, col = "grey", lty = 3) # Horizontal grey lines
  abline(h = mean(l), col = "blue", lwd = 3) # Mean line
  abline(h = median(l), col = "green", lwd = 3) # Median line
  
  legend(x = "bottom", inset = c(0, -.4), cex = .85, bty = "n", horiz = T,
         legend = c((sprintf("Mean: %s %%", round(mean(l), 2))),
                    sprintf("Median: %s %%", round(median(l), 2))),
         col = c("blue", "green"), xpd = T, pch = 15)
  
  axis(side = 4, at = p.seq, las = 1) # Right Y-axis
  
  par(mar = c(8, 4, 3, 4)) # Define borders of the plot
  
  box() # Borders
}
bar.plt.imoex("https://smart-lab.ru/q/index_stocks/IMOEX/") # Test