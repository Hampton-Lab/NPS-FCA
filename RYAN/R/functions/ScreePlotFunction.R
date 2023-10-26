# Adapted from https://ourcodingclub.github.io/2018/05/04/ordination.html

scree_plot <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 5),
       replicate(5, metaMDS(comm = x, distance = "bray", k = 1)$stress),
       xlim = c(1, 5),ylim = c(0, 0.30), xlab = "# of Dimensions",
       ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:5) {
    points(rep(i + 1,5),replicate(5, metaMDS(x,  distance = "bray", k = i + 1)$stress))
  }
}
