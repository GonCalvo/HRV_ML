library(proxy)

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


get_deltas_from_data <- function(data){
  return(data[,1:(length(data)-1)])
}

#https://iopscience.iop.org/article/10.1088/1755-1315/31/1/012012/pdf

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


path = "./Anns/formatted/b1_f1/"
print(paste("Working on directory:", path))

destination = "eps_b1_f1.pdf"
pdf(file=paste(path,destination, sep=""), title=destination)

files = list.files(path)

for (f in files){
  if (endsWith(f, ".txt")) {
    data = read.table(paste(path, f, sep=""), header=TRUE)
    deltas = get_deltas_from_data(data)
    normalized_deltas = min_max_norm(deltas)
    len = nrow(deltas)
    distances <- as.matrix(dist(normalized_deltas))
    min_distances = c()
    for( i in 1:len ){
      if(i == len ) i = i-1
      min_distances = append(min_distances, min(distances[i, (i+1):len]))
    }
    min_distances = sort(min_distances)
    
    ylabel <- seq(0, 1, by = 0.1)
    plot(1:len, min_distances, "l", main=f, ylab = "epsilon", xlab="Points", axis=F)
    axis(2, at = ylabel)
    axis(1)
    box()
  }
}  

dev.off()
