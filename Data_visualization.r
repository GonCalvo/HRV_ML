#install.packages("tidyr")
#install.packages("tidyverse")
#install.packages("GGally")
#library("tidyverse")
#library("GGally")
library(mclust)
library("dbscan")
library(factoextra)
library(cluster)

#=====================================================================
#                 Parameters definition                              =
#=====================================================================

generate_PDF = F

generate_PDF_per_file = F

plot_filter = F
plot_cluster = F
plot_mcluster = F
generate_COR = F
generate_COR_ALL = F
write_cluster_report = T
generate_plot_ALL = F
plot_dbcluster = F
plot_histogram = F

window = 1

#=====================================================================
#                 Functions definition                               =
#=====================================================================


min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


get_deltas_from_data <- function(data){
  return(data[,1:(length(data)-1)])
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
  normalized_deltas = min_max_norm(deltas)
  file = substring(file, 1, 3)
  # png(paste("All_data_plots/plots/", file,"_4_colours.png", sep = ""),width=800,height=800)
  plot(normalized_deltas, col=data$Filter+1, pch=".", cex=3, main=paste(f, "coloured by filter"))
}


# KNN cluster
plot_colour_cluster <- function(file, normalized_deltas, num_centers) {
  cl = kmeans(normalized_deltas, centers=num_centers)
  title = paste(file, "_", num_centers, "NN")
  plot(normalized_deltas, col=cl$cluster, pch=".", cex=3, main=title)
  return(cl)
}


# Mixture of models
plot_colour_mcluster <- function(file, normalized_deltas) {
  cl = Mclust(normalized_deltas)
  plot(cl, what=c("classification"), main=paste(file, "_MixtureOfModels", sep=""))
  #legend(x='topleft', title="CLUSTERS", box.lwd=1,legend=1:cl$G, fill=1:cl$G)
  return(cl)
}


# Density based models
plot_colour_dbcluster <- function(file, normalized_deltas){
  ds_db = dbscan(normalized_deltas, eps=0.10,MinPts = 3)
  plot(as.data.frame(normalized_deltas),col=(ds_db$cluster+1), main=paste(file, "_DBScan,eps=0.10,MinPts=3", sep=""))
  return(ds_db)
}

plot_hist <- function(file, normalized_deltas) {
  h<-hist(normalized_deltas[,2:2], main=file, xlab="IBIs", plot = FALSE)
  plot(h)
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
  path = paste(path, "Cluster_Reports/")
  if ( !file.exists(path)) dir.create(path)
  
  # Then we make sure the file doesn't exist yet, otherwise we delete it
  file_output = paste(path, file_output, "_cluster_report.txt", sep="")
  if ( file.exists(file_output)) file.remove(file_output)
  purity = get_model_purity(cl, data)
  
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


write_cor_report <- function(path, cor_report, file_name, deltas ) {
  #First we make sure the Correlation_Report directory exists
  path = paste(path, "Correlation_Report/")
  deltas = deltas[, 1:length(deltas)-1]
  if ( !file.exists(path)) dir.create(path)
  
  file_output = paste(path, cor_report, sep="")
  if ( !file.exists(file_output)) file.create(file_output)
  
  cat( c(paste("Correlation for file ", file_name)) ,file=file_output, sep="\n",append=TRUE)
  
  # This is here to adjust the columns names tot he right columns.
  cat( "-," ,file=file_output, sep="",append=TRUE)
  
  write.table(cor(deltas), file=file_output, append = TRUE, sep=",")
}


get_model_purity <- function(classifiers, data){
  purity = 0
  for (cluster in 1:max(classifiers)){
    delta_class_in_this_cluster = data[classifiers == cluster, (length(data))]
    to_filter = length(which(!(delta_class_in_this_cluster == 0)))
    to_keep = length(which(delta_class_in_this_cluster == 0))
    purity = purity + max(to_filter, to_keep)
  }
  purity = purity / length(data$i.1)
  
  return(purity)
}

get_model_confusion_matrix <- function(classifiers, data){
  total_combinations = choose(length(data$i.1), 2)
  true_positives = 0
  true_negatives = 0
  total_to_filters = length(which(!(data[,length(data)] == 0)))
  total_to_keep = length(which((data[,length(data)] == 0)))
  cluster_combinations = 0
  for (cluster in 1:max(classifiers)){
    delta_class_in_this_cluster = data[classifiers == cluster, (length(data))]
    cluster_combinations = cluster_combinations + choose(length(delta_class_in_this_cluster), 2)
    to_filter = length(which(!(delta_class_in_this_cluster == 0)))
    to_keep = length(which(delta_class_in_this_cluster == 0))
    true_positives = true_positives + choose(to_filter, 2) + choose(to_keep, 2)
    total_to_filters = total_to_filters - to_filter
    total_to_keep = total_to_keep - to_keep
    true_negatives = true_negatives + to_keep*total_to_filters + to_filter*total_to_keep
  }
  false_positives = cluster_combinations - true_positives
  false_negatives = total_combinations - (true_positives+false_positives) - true_negatives
  
  confusion_matrix = matrix(nrow=2, ncol=2, data = c(true_positives,false_positives, false_negatives, true_negatives))
  
  return(confusion_matrix)
}


get_model_accuracy <- function(confusion_matrix){
  # This is accuracy applied to clustering
  TP = confusion_matrix[1,1]
  FP = confusion_matrix[2,1]
  FN = confusion_matrix[1,2]
  TN = confusion_matrix[2,2]
  return((TP+TN)/(TP+FP+FN+TN))
}

# sensitivity = number of beats filtered that should be filtered
get_model_sensitivity <- function(confusion_matrix){
  TP = confusion_matrix[1,1]
  FN = confusion_matrix[1,2]
  return(TP/(FN+TP))
  
}

# specificity = number of normal beats that were considered normal
get_model_specificity <- function(confusion_matrix){
  FP = confusion_matrix[2,1]
  TN = confusion_matrix[2,2]
  return(TN/(FP+TN))
}

print_model_stats <- function(model, data) {
  purity = get_model_purity(model, data = data)
  confusion_matrix = get_model_confusion_matrix(model, data = data)
  print(confusion_matrix)
  RI = get_model_accuracy(confusion_matrix)
  sensitivity = get_model_sensitivity(confusion_matrix)
  specificity = get_model_specificity(confusion_matrix)
  #print(paste("For ", num, " neighbours: "))
  print(paste("\tPurity: ",purity))
  print(paste("\tRI: ", RI))
  print(paste("\tSensitivity: ", sensitivity))
  print(paste("\tSpecificity: ", specificity))
}

KNN_cross_validation <- function( file, data, num_centers, times=5 ){
  deltas = get_deltas_from_data(data)
  normalized_deltas = min_max_norm(deltas)
  purities = c()
  accuracies = c()
  sensitivities = c()
  specificities = c()
  for(test in 1:times){
    knn_cl = kmeans(normalized_deltas, centers=num_centers)
    purities = append(purities, get_model_purity(knn_cl$cluster, data))
    confusion_matrix = get_model_confusion_matrix(knn_cl$cluster, data)
    accuracies = append(accuracies, get_model_accuracy(confusion_matrix))
    sensitivities = append(sensitivities, get_model_sensitivity(confusion_matrix))
    specificities = append(specificities, get_model_specificity(confusion_matrix))
  }
  scores = data.frame(  purity = purities,
                        accuracy = accuracies,
                        sensitivity = sensitivities,
                        specificity = specificities,
                        clusters = c(num_centers))
  return(scores)
}

MM_cross_validation <- function( file, data, times=5 ){
  deltas = get_deltas_from_data(data)
  normalized_deltas = min_max_norm(deltas)
  purities = c()
  accuracies = c()
  sensitivities = c()
  specificities = c()
  clusters = c()
  for(test in 1:times){
    cl = Mclust(normalized_deltas)
    purities = append(purities, get_model_purity(cl$classification, data))
    confusion_matrix = get_model_confusion_matrix(cl$classification, data)
    accuracies = append(accuracies, get_model_accuracy(confusion_matrix))
    sensitivities = append(sensitivities, get_model_sensitivity(confusion_matrix))
    specificities = append(specificities, get_model_specificity(confusion_matrix))
    clusters = append(clusters, cl$G)
  }
  scores = data.frame(  purity = purities,
                        accuracy = accuracies,
                        sensitivity = sensitivities,
                        specificity = specificities, 
                        clusters = clusters)
  return(scores)
}

# Doesn't really require cross validation
DB_cross_validation <- function( file, data, eps = 0.10, MinPts = 3, times = 5){
  deltas = get_deltas_from_data(data)
  normalized_deltas = min_max_norm(deltas)
  purities = c()
  accuracies = c()
  sensitivities = c()
  specificities = c()
  clusters = c()
  for(test in 1:times){
    ds_db = dbscan(normalized_deltas, eps=eps,MinPts = MinPts)
    purities = append(purities, get_model_purity(ds_db$cluster, data))
    confusion_matrix = get_model_confusion_matrix(ds_db$cluster, data)
    accuracies = append(accuracies, get_model_accuracy(confusion_matrix))
    sensitivities = append(sensitivities, get_model_sensitivity(confusion_matrix))
    specificities = append(specificities, get_model_specificity(confusion_matrix))
    clusters = append(clusters, max(ds_db$cluster)+1)
  }
  scores = data.frame(  purity = purities,
                        accuracy = accuracies,
                        sensitivity = sensitivities,
                        specificity = specificities,
                        clusters = clusters)
  
  return(scores)
}

HDB_cross_validation <- function( file, data, minPts = 3, times=5){
  deltas = get_deltas_from_data(data)
  normalized_deltas = min_max_norm(deltas)
  purities = c()
  accuracies = c()
  sensitivities = c()
  specificities = c()
  for(test in 1:times){
    ds_db = hdbscan(normalized_deltas, minPts = minPts)
    purities = append(purities, get_model_purity(ds_db$cluster, data))
    confusion_matrix = get_model_confusion_matrix(ds_db$cluster, data)
    accuracies = append(accuracies, get_model_accuracy(confusion_matrix))
    sensitivities = append(sensitivities, get_model_sensitivity(confusion_matrix))
    specificities = append(specificities, get_model_specificity(confusion_matrix))
  }
  scores = data.frame(  purity = purities,
                        accuracy = accuracies,
                        sensitivity = sensitivities,
                        specificity = specificities,
                        clusters = c(max(ds_db$cluster)+1))
  
  return(scores)
}


KMENOIDS_cross_validation <- function ( file, data, num_centers=8, times = 5){
  deltas = get_deltas_from_data(data)
  normalized_deltas = min_max_norm(deltas)
  
  purities = c()
  accuracies = c()
  sensitivities = c()
  specificities = c()
  
  for(test in 1:times){
    cl = pam(normalized_deltas, num_centers)
    confusion_matrix = get_model_confusion_matrix(cl$clustering, data)
    accuracies = append(accuracies, get_model_accuracy(confusion_matrix))
    sensitivities = append(sensitivities, get_model_sensitivity(confusion_matrix))
    specificities = append(specificities, get_model_specificity(confusion_matrix))
    purities = append(purities, get_model_purity(cl$cluster, data))
  }
  
  
  scores = data.frame(  purity = purities,
                        accuracy = accuracies,
                        sensitivity = sensitivities,
                        specificity = specificities,
                        clusters = c(max(cl$clustering)+1))
  
  return(scores)
}

#=====================================================================
#                 One Case Use                                       =
#=====================================================================


one_shot <- function(f){
  data = read.table(f, header=TRUE)
  deltas = get_deltas_from_data(data)
  normalized_deltas = min_max_norm(deltas)
  
  print("KNN Clustering")
  num = 3
  knn_cl = plot_colour_cluster(f, normalized_deltas, num)
  print_model_stats(knn_cl$cluster, data)
  
  print("Mixture of Models Clustering")
  cl = plot_colour_mcluster(f, normalized_deltas)
  print_model_stats(cl$classification, data)
  
  print("Density Based Clustering")
  db = plot_colour_dbcluster(f, normalized_deltas)
  print_model_stats(db$cluster, data)
}

#one_shot(f = "./New_Anns/formatted/b1_f1/I03_ann.txt")

#=====================================================================
#                 Code definition                                    =
#=====================================================================


setwd(dirname(rstudioapi::getSourceEditorContext()$path))

for ( i in 1:1 ) {
  path = paste("./Anns/formatted/b", i, "_f", i, "/", sep="")
  print(paste("Working on directory:", path))
  
  files = list.files(path)
  
  if(write_cluster_report){
    file_output = paste(path, "Clustering_report.csv", sep="")
    if ( file.exists(file_output)) file.remove(file_output)
    
    models <- c("KNN_5", "KNN_7", "KNN_9", "MM", "DB_0.1_3", "DB_0.1_5", "DB_0.05_5", "DB_0.07_5", "HDB_5", "KMENOIDS_5",  "KMENOIDS_7",  "KMENOIDS_9")
    #models <- c("KNN_5", "KNN_7", "KNN_9")
    overall_values <- list(1) # we have to declare it and fill it.
    for ( model in models){
      
      overall_values[[model]] <- list()
    }
  }
  
  
  if ( generate_PDF ) {
    destination = paste("plots_b", i, "_f", i, ".pdf", sep="")
    pdf(file=paste(path,destination, sep=""), title=destination)
    print(paste("Saving to file: ", destination))
  }
  
  if ( generate_COR_ALL || generate_plot_ALL ) all_deltas = ""
  if ( generate_plot_ALL ) all_filter = ""
  
  
  for ( f in files ) {
    if (endsWith(f, ".txt") && f != "I27_ann.txt") {
      
      print(paste("Working on file: ", f))
      if( generate_PDF_per_file ){
        destination = paste("plots_", substr(f, 0, nchar(f)-4), ".pdf", sep="")
        pdf(file=paste(path,destination, sep=""), title=destination)
        print(paste("Saving to file: ", destination))
      }
      
      data = read.table(paste(path, f, sep=""), header=TRUE)
      deltas = get_deltas_from_data(data)
      normalized_deltas = min_max_norm(deltas)
      
      if ( generate_COR_ALL || generate_plot_ALL) {
        if ( class(all_deltas) == "character"){
          all_deltas = normalized_deltas
        } else {
          all_deltas = rbind(all_deltas, normalized_deltas)
        }
        
        if( class(all_filter) == "character"){
          all_filter = data["Filter"]
        } else {
          all_filter = rbind(all_filter, data["Filter"])
        }
      }
      
      # Plot deltas, marking which should be filtered
      if ( plot_filter ) plot_colour_filter(f, data)
      
      # Plot deltas coloured by cluster
      if ( plot_cluster ){
        num_centers = c(3,5,7)
        for (num in num_centers) {
          knn_cl = plot_colour_cluster(f, normalized_deltas, num)
          print_model_stats(knn_cl, data)
        }
      }
      
      # Mixture of cluster models
      if ( plot_mcluster ) {
        cl = plot_colour_mcluster(f, normalized_deltas)
        print_model_stats(cl, data)
      }
      
      # Density Based clustering
      if ( plot_dbcluster ) {
        db = plot_colour_dbcluster(f, normalized_deltas)
        print_model_stats(db, data)
      }
      
      if ( plot_histogram ) plot_hist(f, normalized_deltas)
      
      if ( write_cluster_report ) {
        total_scores <- data.frame(row.names = models)
        for( model in models ){
          print(paste("applying model:", model))
          if ( startsWith(model, "KNN") ){
            num = as.numeric(strsplit(model, "_")[[1]][2])
            scores = KNN_cross_validation(f, data, num_centers = num)
          } else if (startsWith(model, "DB") ){
            model_data = strsplit(model, "_")[[1]]
            eps = as.numeric(model_data[2])
            min_pts = as.numeric(model_data[3])
            scores = DB_cross_validation(f, data, eps, MinPts = min_pts )
          } else if (startsWith(model, "MM")){
            scores = MM_cross_validation(f, data)
          } else if (startsWith(model, "HDB")){
            model_data = strsplit(model, "_")[[1]]
            min_pts = as.numeric(model_data[2])
            scores = HDB_cross_validation(f, data, min_pts)
          } else if (startsWith(model, "KMENOIDS") ){
            model_data = strsplit(model, "_")[[1]]
            num_centers = as.numeric(model_data[2])
            scores = KMENOIDS_cross_validation(f, data, num_centers )
          }

          purity_mean = mean(scores$purity)
          purity_sd = sd(scores$purity)
          total_scores[model, "purity_mean"] = purity_mean
          total_scores[model, "purity_sd"] = purity_sd
          if(is.null(overall_values[[model]][["purity_mean"]])){
            overall_values[[model]][["purity_mean"]] <- c(purity_mean)
          }else{
            #l <- length(overall_values[[model]][["purity_mean"]])+1
            #overall_values[[model]][["purity_mean"]][[l]] <- purity_mean
            overall_values[[model]][["purity_mean"]] <- append(overall_values[[model]][["purity_mean"]], purity_mean)
          }
          
          accuracy_mean = mean(scores$accuracy)
          accuracy_sd = sd(scores$accuracy)
          total_scores[model, "accuracy_mean"] = accuracy_mean
          total_scores[model, "accuracy_sd"] = accuracy_sd
          if(is.null(overall_values[[model]][["accuracy_mean"]])){
            overall_values[[model]][["accuracy_mean"]] <- c(accuracy_mean)
          }else{
            #l <- length(overall_values[[model]][["accuracy_mean"]])+1
            #overall_values[[model]][["accuracy_mean"]][[l]] <- accuracy_mean
            overall_values[[model]][["accuracy_mean"]] <- append(overall_values[[model]][["accuracy_mean"]], accuracy_mean)
          }
          
          specificity_mean = mean(scores$specificity)
          specificity_sd = sd(scores$specificity)
          total_scores[model, "specificity_mean"] = specificity_mean
          total_scores[model, "specificity_sd"] = specificity_sd
          if(is.null(overall_values[[model]][["specificity_mean"]])){
            overall_values[[model]][["specificity_mean"]] <- c(specificity_mean)
          }else{
            #l <- length(overall_values[[model]][["specificity_mean"]])+1
            #overall_values[[model]][["specificity_mean"]][[l]] <- specificity_mean
            overall_values[[model]][["specificity_mean"]] = append(overall_values[[model]][["specificity_mean"]], specificity_mean)
          }
          sensitivities_mean = mean(scores$sensitivity)
          sensitivities_sd = sd(scores$sensitivity)
          total_scores[model, "sensitivities_mean"] = sensitivities_mean
          total_scores[model, "sensitivities_sd"] = sensitivities_sd
          if(is.null(overall_values[[model]][["sensitivities_mean"]])){
            overall_values[[model]][["sensitivities_mean"]] <- c(sensitivities_mean)
          }else{
            #l <- length(overall_values[[model]][["sensitivities_mean"]])+1
            #overall_values[[model]][["sensitivities_mean"]][[l]] <- sensitivities_mean
            overall_values[[model]][["sensitivities_mean"]] <- append(overall_values[[model]][["sensitivities_mean"]], sensitivities_mean)
          }
          clusters_mean = mean(scores$clusters)
          clusters_sd = sd(scores$clusters)
          total_scores[model, "clusters_mean"] = clusters_mean
          total_scores[model, "clusters_sd"] = clusters_sd
          if(is.null(overall_values[[model]][["clusters_mean"]])){
            overall_values[[model]][["clusters_mean"]] <- c(clusters_mean)
          }else{
            #l <- length(overall_values[[model]][["clusters_mean"]])+1
            #overall_values[[model]][["clusters_mean"]][[l]] <- clusters_mean
            overall_values[[model]][["clusters_mean"]] = append(overall_values[[model]][["clusters_mean"]], clusters_mean)
          }
          
        }
        print(total_scores)
        file_output = paste(path, "Clustering_report.csv", sep="")
        if ( !file.exists(file_output)) file.create(file_output)
        
        cat( c(paste("Clustering results for file ", f)) ,file=file_output, sep="\n",append=TRUE)
        cat( "-," ,file=file_output, sep="",append=TRUE)
        write.table(total_scores, file=file_output, append = TRUE, sep=",")
        
        
      }
      
      if ( generate_COR ) {
        normalized_deltas["Filter"] = data["Filter"]
        write_cor_report(path, "Cor_report.csv", f, normalized_deltas)
        write_cor_report(path, "Cor_report_filtered.csv", f, normalized_deltas[data$Filter!=0,])
        write_cor_report(path, "Cor_report_non_filtered.csv", f, normalized_deltas[data$Filter==0,])
      }
      
      if(generate_PDF_per_file){
        dev.off()
      }
    }
  }
  
  if ( generate_COR_ALL ) {
    write_cor_report(path, "Cor_report_ALL.csv", "Correlation between all deltas", normalized_deltas)
    write_cor_report(path, "Cor_report_filtered_ALL.csv", "Correlation between all FILTERED deltas", normalized_deltas[data$Filter==1,])
    write_cor_report(path, "Cor_report_non_filtered_ALL.csv", "Correlation between all non-filtered deltas", normalized_deltas[data$Filter==0,])
  }
  
  if ( generate_plot_ALL ) {
    all_deltas["Filter"] = all_filter
    plot_colour_filter("All files", all_deltas)
  }
  
  
  if ( write_cluster_report ) {
    file_output = paste(path, "Clustering_report.csv", sep="")
    if ( !file.exists(file_output)) file.create(file_output)
    overall_scores = data.frame(row.names=models)
    for( model in models ){
      overall_scores[model, "purity_mean"] = mean(overall_values[[model]][["purity_mean"]])
      overall_scores[model, "purity_sd"] = sd(overall_values[[model]][["purity_mean"]])
      
      overall_scores[model, "accuracy_mean"] = mean(overall_values[[model]][["accuracy_mean"]])
      overall_scores[model, "accuracy_sd"] = sd(overall_values[[model]][["accuracy_mean"]])
      
      overall_scores[model, "specificity_mean"] = mean(overall_values[[model]][["specificity_mean"]])
      overall_scores[model, "specificity_sd"] = sd(overall_values[[model]][["specificity_mean"]])
      
      overall_scores[model, "sensitivities_mean"] = mean(overall_values[[model]][["sensitivities_mean"]])
      overall_scores[model, "sensitivity_sd"] = sd(overall_values[[model]][["sensitivities_mean"]])
      
      overall_scores[model, "clusters_mean"] = mean(overall_values[[model]][["clusters_mean"]])
      overall_scores[model, "clusters_sd"] = sd(overall_values[[model]][["clusters_mean"]])
      
      
    }
                            
    cat( c(paste("Clustering results overall")) ,file=file_output, sep="\n",append=TRUE)
    cat( "-," ,file=file_output, sep="",append=TRUE)
    write.table(overall_scores, file=file_output, append = TRUE, sep=",")
  }
  
  if ( generate_PDF ) dev.off()
}

