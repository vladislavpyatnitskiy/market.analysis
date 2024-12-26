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
  
  f <- read_html("https://smart-lab.ru/q/shares/") %>% html_nodes('table') %>%
    .[[1]] %>% html_nodes('tr') #
  
  L <- NULL # Reorganise data into data frame
  
  for (n in 2:length(f)){ d <- f[n] %>% html_nodes('td') %>% html_text()
  
    P <- gsub('["\n"]', '', gsub('["\t"]', '', d[8]))
    
    P <- as.character(read.fwf(textConnection(P), widths = c(nchar(P) - 1, 1),
                               colClasses = "character")[1])
    
    if (isTRUE(grepl("\\+", P))){ P <- as.numeric(gsub("\\+", "", P)) }
    
    L <- rbind.data.frame(L, cbind(d[3], d[7], P)) } # ticker, price & %
    
  plt <- barplot(l,names.arg=names(l),main="IMOEX Stocks Returns for Today (%)", 
                 col = c(rep("green4", length(l) - sum(l < 0)),
                         rep("red3", sum(l < 0))), horiz = F, las = 2)
  
  # Break even line, horizontal lines, Mean, Median and IMOEX values
  nums = list(0, mean(l), median(l), L[1,3])
  cols <- c("black", "blue", "green", "purple") # Colours
  lwds <- c(1, rep(3, 3)) # Width 
  ltys <- c(1, rep(1, 3)) # Type
  
  for (n in 1:4){ abline(h=nums[[n]], col=cols[n], lwd=lwds[n], lty=ltys[n]) }
  
  legend(x="bottom", inset=c(0, -.3), cex=.85, bty="n", horiz=T, xpd=T, pch=15,
         legend = c((sprintf("Mean: %s %%", round(mean(l), 2))),
                    sprintf("Median: %s %%", round(median(l), 2)),
                    sprintf("%s: %s %%", L[1,1], L[1,3])), col = cols[2:4])
  
  axis(side = 4, las = 2) # Right y-axis
  
  grid(nx=NULL, ny=NULL, col = "grey", lty = "dotted", lwd = 1) # grid lines
  
  par(mar = c(8, 4, 3, 4)) # Define borders of the plot
  
  box() # Borders
}
bar.plt.imoex("https://smart-lab.ru/q/index_stocks/IMOEX/") # Test
