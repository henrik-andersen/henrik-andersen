
# install.packages("scholar")
library(scholar)

# Get overview of publications
# get_publications("bWQX7N8AAAAJ")
# get_article_cite_history("bWQX7N8AAAAJ", "d1gkVwhDpl0C")

# Get overview of citations per year
citations <- get_citation_history("bWQX7N8AAAAJ")
# citations

# Function to round up 
roundUpNice <- function(x, nice = c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

# ---- cite-plot ---- 

# Plot citations 
library(ggplot2)

cite_plot <- ggplot(citations, aes(x = year, y = cites)) + 
  geom_col(color = "black", fill = "deepskyblue4") + 
  geom_text(aes(label = cites), vjust = -0.5) + 
  scale_y_continuous(name = "Citations", 
                     limits = c(0, roundUpNice(max(citations$cites))), 
                     breaks = seq(0, roundUpNice(max(citations$cites)), 5)) + 
  scale_x_continuous(name = "Year") + 
  ggtitle("Google Scholar citations per year for Henrik K. Andersen")

# saveRDS(cite_plot, file = "r-files/cite_plot.Rda")
ggsave("r-files/cite_plot.png", plot = cite_plot, device = "png")

