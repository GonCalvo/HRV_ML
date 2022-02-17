window = 5

for ( i in 1:window ) {
  path = paste("./Anns/formatted/b", i, "_f", i, "/", sep="")
  print(paste("Working on directory:", path))
  files = list.files(path)
  destination = paste("plots_b", i, "_f", i, ".pdf", sep="")
  pdf(file=paste(path,destination, sep=""), title=destination)
  par(mfrow = c(2,2))
  print(paste("Saving to file: ", destination))
  
  for ( file in files ) {
    if (endsWith(file, ".txt")) {
      data = read.table(paste(path,file, sep=""), header=TRUE)
      deltas = data[,1:length(data)-1]
      plot(deltas, col=data$Filter+1, pch=".", cex=3, main=file)
    }
  }
  dev.off()
}

# 5 Centers seemed to give me a good result.
# 6 centers sometimes gave a nice result.
#num_centers = 6
#cl = kmeans(deltas, centers=num_centers)
#library( rgl )
#plot3d( data, col = cl$cluster )
#plot3d( cl$centers, add=TRUE, type='s', radius=5)
#legend3d("topright", legend=paste("Cluster", c(1:num_centers)))

# We add the cluster information to our data
#data$cluster <- cl$cluster

#We get the cluster number of the measurements we know we should filter
#If we did this, we could filter entire cluster of beats just because they had 1 erroneus beat on themlist.files
#clusters_to_filter = cl$cluster[t(data[3])] 
#filtered_data = subset(data, !cluster%in%clusters_to_filter)





