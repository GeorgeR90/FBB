##Packages
require(mclust)
require(ggplot2)

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
  
  x$rank <- as.numeric(1:nrow(x))  #Rank for sorting
  x$tiers.c <- as.factor(x$tiers.c)
  x$name.length <- nchar(x$tiers.fp.player)
  return(x)
}

fp.tier.list <- lapply(fp.tier.list, clust.k)


for(i in 1:length(names(fp.tier.list))){

  if(names(fp.tier.list)[i] == 'Overall') {

    ##Overall Plot
    font <- 2.4; barsize <- 1; dotsize <- 0.8; highcolor <- 650;
    
    p <- ggplot(fp.tier.list$Overall[1:150,], aes(x=-rank, y = tiers.fp.avg))
    p <- p + ggtitle('Overall Rankings')
    p <- p + geom_errorbar(aes(ymin = tiers.fp.avg - tiers.fp.sd/2, ymax = tiers.fp.avg + tiers.fp.sd/2, width=0.2, colour=tiers.c), size=barsize*0.8, alpha=0.4)
    p <- p + geom_point(colour="grey20", size=dotsize) 
    p <- p + coord_flip()
    p <- p + annotate("text", x = Inf, y = -Inf, label = "www.GeorgeRooney.com", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
    p <- p + geom_text(aes(label=tiers.fp.player, colour=tiers.c, y = tiers.fp.avg - name.length/2 
                           - tiers.fp.sd/1.8), size=font) 
    p <- p + scale_x_continuous("Expert Consensus Rank")
    p <- p + ylab("Average Expert Rank")
    p <- p + theme(legend.justification=c(1,1), legend.position=c(1,1))
    p <- p + scale_colour_discrete(name="Tier")
    p <- p + scale_colour_hue(l=55, h=c(0, highcolor))
    maxy <- max( abs(fp.tier.list$Overall$tiers.fp.avg[1:150])+fp.tier.list$Overall$tiers.fp.sd[1:150]/2) 
    p = p + ylim(-10, maxy)
    
    DPI = 250
    ggsave(file="predraft-overall.png", width = 14, height = 14, dpi = DPI)
    
  }
  
  else{
    font <- 3; barsize <- 1.25; dotsize <- 1; highcolor <- 450;
  }
  
  
}


##Create custom $$ value

fbb.data <- fbb.data[order(-fbb.data$ovr.r),] ## Reorder
num.teams <- 16; rost.size <- 12; doll.per <- 200; doll.min <- 1;
fbb.data$cost.g.o <- (fbb.data$ovr.r/sum(fbb.data$ovr.r[1:(num.teams*rost.size)]))*((doll.per - doll.min)*num.teams)
fbb.data$cost.g.o[(fbb.data$cost.g.o < 0) | is.na(fbb.data$cost.g.o)] <- 0

##calculate custom ratings based on emphasis
ga <- c('3pm.r', 'ast.r', 'ft.r') #guard archetype fields
fa <- c('fg.r', 'reb.r', 'blk.r') #fields archetype fields
fbb.data$ovr.r.ga.s <- fbb.data$ovr.r + rowSums(.2*fbb.data[,ga])  ##Standard guard focus
fbb.data$ovr.r.ga.a <- fbb.data$ovr.r + rowSums(.5*fbb.data[,ga])  ##Aggressive guard focus
fbb.data$ovr.r.ga.va <- fbb.data$ovr.r + rowSums(1*fbb.data[,ga]) ##Very aggressive guard focus
fbb.data$ovr.r.fa.s <- fbb.data$ovr.r + rowSums(.2*fbb.data[,fa])  ##Standard forward focus
fbb.data$ovr.r.fa.a <- fbb.data$ovr.r + rowSums(.5*fbb.data[,fa])  ##Aggressive forward focus
fbb.data$ovr.r.fa.va <- fbb.data$ovr.r + rowSums(1*fbb.data[,fa]) ##Very aggressive forward focus

custom.ratings <- c('ovr.r.ga.s','ovr.r.ga.a','ovr.r.ga.va','ovr.r.fa.s','ovr.r.fa.a','ovr.r.fa.va')
custom.cost <- paste0('cost',substr(custom.ratings,4,10000))
fbb.data[,custom.cost] <- sapply(fbb.data[,custom.ratings], function(x) {
  x/sum(x[1:(num.teams*rost.size)])*((doll.per - doll.min)*num.teams)
})


