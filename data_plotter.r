window = 1
num_centers = 6

get_deltas_from_data <- function(data){
  return(data[,1:(length(data)-2)])
}

plot_colour_timestamp <- function(file, data) {
  timepos = data$Time
  gray_scale = grey((timepos-min(timepos))/(max(timepos)-min(timepos)))
  par(bg="red3")
  
  deltas = get_deltas_from_data(data)
  
  title = paste(file, "Coloured by timestamp",sep=" ")
  plot(deltas, col=gray_scale, pch=".", cex=3, main=title)
}


plot_colour_filter <- function(file, data) {
  title = paste(file, "Coloured by filter",sep=" ")
  deltas = get_deltas_from_data(data)
  plot(deltas, col=data$Filter+1, pch=".", cex=3, main=title)
}


plot_colour_cluster <- function(file, data, num_centers) {
  deltas = get_deltas_from_data(data)
  cl = kmeans(deltas, centers=num_centers)
  title = paste(file, "_", num_centers, "cluster")
  plot(deltas, col=cl$cluster, pch=".", cex=3, main=title)
}

plot_colour_mcluster <- function(file, data) {
  library(mclust)
  deltas = get_deltas_from_data(data)
  cl = Mclust(deltas)
  plot(cl, what=c("classification"))
}

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
      print(paste("Workign on file: ", file))
      data = read.table(paste(path, file, sep=""), header=TRUE)
      
      #calculate correlation.
      #correlation = cor(deltas)
      #print(correlation)
      
      #Plot deltas, marking which should be filtered
      plot_colour_filter(file, data)
      
      #Plot deltas, indicating with the colour the approx timestamp
      #plot_colour_timestamp(file, data)
      
      #Plot deltas coloured by cluster
      #plot_colour_cluster(file, data, num_centers)
      
      #Mixture of cluster models
      plot_colour_mcluster(file, data)
      
    }
  }
  dev.off()
}

# 5 Centers seemed to give me a good result.
# 6 centers sometimes gave a nice result.
path = paste("./Anns/formatted/b", 5, "_f", 5, "/", sep="")
file = "I02_ann.txt"
data = read.table(paste(path,file, sep=""), header=TRUE)

deltas = data[,1:length(data)-1]
cl = kmeans(deltas, centers=num_centers)
plot(deltas, col=cl$cluster, pch=".", cex=3, main=file)
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





