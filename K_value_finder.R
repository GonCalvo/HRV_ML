min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


get_deltas_from_data <- function(data){
  return(data[,1:(length(data)-1)])
}


setwd(dirname(rstudioapi::getSourceEditorContext()$path))


path = "./Anns/formatted/b1_f1/"
print(paste("Working on directory:", path))

destination = "wcss_b1_f1.pdf"
pdf(file=paste(path,destination, sep=""), title=destination)

files = list.files(path)
range = 15
for (f in files){
  if (endsWith(f, ".txt")) {
    wcss <- vector()
    data = read.table(paste(path, f, sep=""), header=TRUE)
    deltas = get_deltas_from_data(data)
    normalized_deltas = min_max_norm(deltas)
    for ( i in 1:range ){
      wcss[i] <- sum(kmeans(normalized_deltas, centers=i)$withinss)
    }
    plot(1:range, wcss, "b", main=f, xlab = "Number of clusters")
  }
}  

dev.off()
