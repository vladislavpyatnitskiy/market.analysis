library("rvest") # Library

finviz.sectors.marketcap.pie <- function(x){ # Pie Plot with info about sectors
  
  s <- read_html(sprintf("https://finviz.com/%s", x))
  
  s.yahoo <- s %>% html_nodes('table') %>% .[[8]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  d <- NULL # Data Frame with values
  
  # Market Cap 
  for (n in 0:(length(y) / 15)){ d <- rbind(d,cbind(y[(2+n*15)],y[(3+n*15)])) }
  
  d <- d[-nrow(d),] # Reduce last column
  
  rownames(d) <- d[,1] # Assign row names
  
  d <- subset(d, select = -c(1)) # Reduce excessive column
  
  colnames(d) <- c("Market Cap ($blns)") #
  
  for (n in 1:nrow(d)){ # Reduce "B" from Market Cap column values
    
    d[n,1] <- read.fwf(textConnection(d[n,1]), widths = c(nchar(d[n,1]) - 1, 1),
                       colClasses = "character")[,1] }
  
  d <- as.data.frame(d) # Transform to data frame
  
  for (n in 1:ncol(d)){ d[,n] <- as.numeric(d[,n]) } # Make data numeric
  
  tickers <- rownames(d) # Assign names for sectos
  
  d <- d[,1] # make data numeric
  
  d <- round(d / sum(d), 2) * 100 # calculates percents
  
  names(d) <- tickers # Assign names for sectors
  
  d <- sort(d, decreasing = T)
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
        "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
        "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
        "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
        "#895c8b","#bd5975")
  
  pie(d, labels = c(sprintf("%s %s%%", names(d), d)), col = C, radius = 1.75,
      main = "Portions of Sectors in Market Cap", sub = "FINVIZ") # Plot
}
finviz.sectors.marketcap.pie("groups.ashx?g=sector&v=120&o=name") # Test
