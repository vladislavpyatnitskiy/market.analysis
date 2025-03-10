library("rvest") # Library

finviz.sectors.marketcap.pie <- function(x){ # Pie Plot with info about sectors
  
  y <- read_html(sprintf("https://finviz.com/%s", x)) %>%
    html_nodes('table') %>% .[[8]] %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text() # Get Data and from Data Frame with values
  
  d <- data.frame(y[seq(from = 2, to = length(y), by = 15)],
                  y[seq(from = 3, to = length(y), by = 15)])
  
  rownames(d) <- d[,1] # Assign row names
  
  d <- subset(d, select = -c(1)) # Reduce excessive column
  
  colnames(d) <- c("Market Cap ($blns)") # Column name
  
  for (n in 1:nrow(d)){ # Reduce "B" from Market Cap column values
    
    d[n,1] <- read.fwf(textConnection(d[n,1]), widths = c(nchar(d[n,1]) - 1, 1),
                       colClasses = "character")[,1] }
  
  d <- as.data.frame(d) # Transform to data frame
  
  for (n in 1:ncol(d)){ d[,n] <- as.numeric(d[,n]) } # Make data numeric
  
  D <- round(d[,1] / sum(d[,1]), 2) * 100 # calculates percent
  
  names(D) <- rownames(d) # Assign names for sectors
  
  D <- sort(D, decreasing = T) # Sort sectors by portion sizes
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a")
  
  pie(D, labels = c(sprintf("%s %s%%", names(D), D)), col = C, radius = 1.5,
      main = "Portions of US Sectors in Market Cap", sub = "FINVIZ") # Plot
}
finviz.sectors.marketcap.pie("groups.ashx?g=sector&v=120&o=name") # Test
