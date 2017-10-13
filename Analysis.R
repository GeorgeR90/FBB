##Packages
require(mclust)

##Tiers from Fantasy Pros
clust.k <- function(x, k = NA){
    if(is.na(k)){
      k = floor(nrow(x)/8)
    }
  clust <- Mclust(sqrt(x$tiers.fp.avg), G = k)
  x$tiers.c <- clust$classification
  
  clusters.found <- levels(factor(x$tiers.c))
  clusters.found = as.numeric(clusters.found)
  for (i in 1:k) {
    if ( sum(x$tiers.c ==i)==0 ) { # if you don't find any of this cluster
      # decrease everything above it by one
      x$tiers.c[x$tiers.c>i] <- x$tiers.c[x$tiers.c>i]-1
    }
  }
  return(x)
}

fp.tier.list <- lapply(fp.tier.list, clust.k)

