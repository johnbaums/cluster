# ISODATA clustering technique, J Baumgartner 4 Mar 2010
nclust <- 15
npts <- 1000
iters <- 1000
threshold <- 0.02
cluster.means <- array(dim=c(nclust, 3, iters+1), 
                       dimnames=list(1:nclust, c('cluster', 'x', 'y'), NULL))
cluster.means[,,1] <- c(1:nclust, runif(2*nclust, 0, 10)) # initial mean vector
pts <- matrix(c(runif(2*npts, 0, 10)), ncol=2)
cluster <- matrix(nrow=npts, ncol=iters)
inclust <- matrix(nrow=nclust, ncol=iters)

# Quick peek at what's going on...
checkpt <- 1
x <- pts[checkpt, ] # interrogate this point
closest <- which.min(dist(rbind(x, cluster.means[, -1, 1]))[1:nclust])
plot(pts, pch=20, cex=0.8, col='gray40', xlim=c(0, 10), ylim=c(0, 10), xlab='x', 
     ylab='y', main=sprintf('Closest to point %s is cluster %s', checkpt, closest))
points(cluster.means[, -1, 1], cex=3, pch=20)
segments(rep(pts[checkpt,1], nclust), rep(pts[checkpt,2], nclust), 
         cluster.means[, 2, 1], cluster.means[, 3, 1], lwd=2)
text(cluster.means[, 2, 1], cluster.means[, 3, 1], cex=0.7, col=0)

library(RColorBrewer)
library(plyr)
cols <- c(brewer.pal(8, 'Dark2'), brewer.pal(12, 'Paired'), brewer.pal(9, 'Set1'))

png(file="ISODATA_step%02d.png", width=700, height=550)
for (i in 1:iters) {
  cluster[,i] <- apply(pts, 1, function(x) {
    which.min(dist(rbind(x, cluster.means[, 2:3, i]))[1:nclust])
  })
  inclust[, i] <- sapply(1:nclust, function(x) length(which(cluster[, i]==x)))
  if (i > 1) {
    if(all(na.omit(abs(1 - (inclust[, i] / inclust[, i-1])) < threshold))) break
  }
  new.means <- aggregate(pts, list(cluster[, i]), mean)
  colnames(new.means) <- c('cluster', 'x', 'y')
  tmp <- as.matrix(merge(cluster.means[,, i], new.means, by='cluster', all.x=TRUE))
  tmp[is.na(tmp[,4]), 4:5] <- tmp[is.na(tmp[,4]), 2:3]
  cluster.means[,, i+1] <- tmp[, -(2:3)]
  plot(pts, xlim=c(0, 10), ylim=c(0, 10), type='n', ylab='y', xlab='x')
  sapply(unique(cluster[,i]), function(x) {
    p <- pts[cluster[,i]==x,]
    hull <- chull(p)
    polygon(p[hull,1], p[hull,2], border=1, lwd=2)
  })
  points(pts, pch=21, bg=cols[cluster[, i]], xlim=c(0, 10), ylim=c(0, 10))
  points(cluster.means[,-1,i], pch=21, cex=2.5, bg=cols, col=1, lwd=3)
  text(cluster.means[, 2, i], cluster.means[, 3, i], cex=0.8, col=1, font=2)
}
dev.off()

last <- tail(list.files(patt='\\.png$'), 1)
steps <- match(NA, cluster.means[1,2,]) - 1
# imagemagick convert to animated gif
system(sprintf("convert -delay 0 -loop 1 %s -delay 40 *.png ISODATA_clusters_%ssteps.gif", last, steps))
file.remove(list.files(pattern="ISODATA_step[0-9]+.png")) # ditch the individual png frames