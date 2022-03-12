# use tidyr
install.packages("tidyr")
install.packages("tidyverse")
install.packages("GGally")
library("tidyverse")
library("GGally")

generate_PDF = F
plot_filter = F
plot_cluster = F
plot_mcluster = F
generate_COR = T
write_cluster_report = F

window = 5


num_centers = 6

get_deltas_from_data <- function(data){
  return(data[,2:(length(data)-2)])
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
  legend(x='topleft', title="CLUSTERS", box.lwd=1,legend=1:cl$G, fill=1:cl$G)
  return(cl)
}


milliseconds_to_string <- function( millisecs ) {
  seconds = floor(millisecs/1000)
  millisecs = millisecs%%1000
  minutes = floor(seconds/60)
  seconds = seconds%%60
  return(paste(minutes, seconds, millisecs, sep=":"))
}

cluster_to_color <- function(cl){
  # Only works for clusters generated with mclust, and up to 10 clusters
  colours=c('black', 'red', 'blue', 'orange', 'yellow', 'purple', 'green', 
            'magenta', 'plum', 'peru')
  for( i in 1:cl$G ){
    cl$classification<-replace(cl$classification, cl$classification==i, colours[i])
  }
}

write_cl_report <- function(path, file, data, cl) {
  #First we make sure the Cluster_Reports directory exists
  path = paste(path, "/Cluster_Reports/")
  if ( !file.exists(path)) dir.create(path)
  
  # Then we make sure the file doesn't exist yet, otherwise we delete it
  file_output = paste(path, file_output, "_cluster_report.txt", sep="")
  if ( file.exists(file_output)) file.remove(file_output)
  
  for (i in 1:cl$G) {
    rows_to_get = cl$classification==i
    times = data.frame(data[rows_to_get, 1], 
                       milliseconds_to_string(data[rows_to_get, length(data)]))
    filter = data[cl$classification==i, length(data)-1]
    
    write.table(paste("In cluster", i, "we found:\n\t", length(filter[filter==1]), " beats to filter and",
                      length(filter[filter==0]), "normal filters out of", length(filter)
                      , sep=" "), file=file_output, append = TRUE, sep="", row.names=FALSE,
                col.names=F)
    write.table(times, file=file_output, append=TRUE, sep="\t", row.names=FALSE,
                col.names=F)
  }
}

write_cor_report <- function(path, cor_report, file_name, data ) {
  #First we make sure the Correlation_Report directory exists
  path = paste(path, "Correlation_Report/")
  if ( !file.exists(path)) dir.create(path)
  
  file_output = paste(path, cor_report, sep="")
  if ( !file.exists(file_output)) file.create(file_output)
  
  deltas <- get_deltas_from_data(data)
  
  cat( c(paste("Correlation for file ", file_name)) ,file=file_output, sep="\n",append=TRUE)
  
  write.table(cor(deltas), file=file_output, append = TRUE, sep=";")
}

for ( i in 1:window ) {
  path = paste("./Anns/formatted/b", i, "_f", i, "/", sep="")
  print(paste("Working on directory:", path))
  
  files = list.files(path)
  
  if ( generate_PDF ) {
    destination = paste("plots_b", i, "_f", i, ".pdf", sep="")
    pdf(file=paste(path,destination, sep=""), title=destination)
    print(paste("Saving to file: ", destination))
  }
  
  for ( f in files ) {
    if (endsWith(f, ".txt")) {
      print(paste("Workign on file: ", f))
      data = read.table(paste(path, f, sep=""), header=TRUE)
      
      #Plot deltas, marking which should be filtered
      if ( plot_filter ) plot_colour_filter(f, data)
      
      #Plot deltas, indicating with the colour the approx timestamp
      #plot_colour_timestamp(f, data)
      
      #Plot deltas coloured by cluster
      if ( plot_cluster ) plot_colour_cluster(f, data, num_centers)
      
      #Mixture of cluster models
      if ( plot_mcluster ) cl = plot_colour_mcluster(f, data)
      
      if ( write_cluster_report ) write_cl_report(path, substr(f, 0, nchar(f)-4), data, cl)
      
      if ( generate_COR ) {
        write_cor_report(path, "Cor_report.csv", substr(f, 0, nchar(f)-4), data)
        write_cor_report(path, "Cor_report_filtered.csv", substr(f, 0, nchar(f)-4), data[data$Filter==1,])
        write_cor_report(path, "Cor_report_non_filtered.csv", substr(f, 0, nchar(f)-4), data[data$Filter==0,])
      }
      
      
      
      
    }
  }
  if ( generate_PDF ) dev.off()
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





