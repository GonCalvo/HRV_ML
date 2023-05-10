#install.packages("tidyr")
#install.packages("tidyverse")
#install.packages("GGally")
library("tidyverse")
library("GGally")
library(mclust)
library("dbscan")

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


get_model_rand_index <- function(confusion_matrix){
  # This is accuracy applied to clustering
  TP = confusion_matrix[1,1]
  FP = confusion_matrix[2,1]
  FN = confusion_matrix[1,2]
  TN = confusion_matrix[2,2]
  return((TP+TN)/(TP+FP+FN+TN))
}

# Higher Sensitivity = less clusters per class
get_model_sensitivity <- function(confusion_matrix){
  TP = confusion_matrix[1,1]
  FN = confusion_matrix[1,2]
  return(TP/(FN+TP))
  
}

# Higher specificity = clusters have asmaller mix of classes
get_model_specificity <- function(confusion_matrix){
  FP = confusion_matrix[2,1]
  TN = confusion_matrix[2,2]
  return(TN/(FP+TN))
}

print_model_stats <- function(model, data) {
  purity = get_model_purity(model, data = data)
  confusion_matrix = get_model_confusion_matrix(model, data = data)
  print(confusion_matrix)
  RI = get_model_rand_index(confusion_matrix)
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
  rand_indexes = c()
  sensitivities = c()
  specificities = c()
  for(test in 1:times){
    knn_cl = kmeans(normalized_deltas, centers=num_centers)
    purities = append(purities, get_model_purity(knn_cl$cluster, data))
    confusion_matrix = get_model_confusion_matrix(knn_cl$cluster, data)
    rand_indexes = append(rand_indexes, get_model_rand_index(confusion_matrix))
    sensitivities = append(sensitivities, get_model_sensitivity(confusion_matrix))
    specificities = append(specificities, get_model_specificity(confusion_matrix))
  }
  scores = data.frame(  purity = purities,
                        rand_index = rand_indexes,
                        sensitivity = sensitivities,
                        specificity = specificities)
  return(scores)
}

MM_cross_validation <- function( file, data, times=5 ){
  deltas = get_deltas_from_data(data)
  normalized_deltas = min_max_norm(deltas)
  purities = c()
  rand_indexes = c()
  sensitivities = c()
  specificities = c()
  for(test in 1:times){
    cl = Mclust(normalized_deltas)
    purities = append(purities, get_model_purity(cl$classification, data))
    confusion_matrix = get_model_confusion_matrix(cl$classification, data)
    rand_indexes = append(rand_indexes, get_model_rand_index(confusion_matrix))
    sensitivities = append(sensitivities, get_model_sensitivity(confusion_matrix))
    specificities = append(specificities, get_model_specificity(confusion_matrix))
  }
  scores = data.frame(  purity = purities,
                        rand_index = rand_indexes,
                        sensitivity = sensitivities,
                        specificity = specificities)
  return(scores)
}

DB_cross_validation <- function( file, data){
  deltas = get_deltas_from_data(data)
  normalized_deltas = min_max_norm(deltas)
  purities = c()
  rand_indexes = c()
  sensitivities = c()
  specificities = c()
  ds_db = dbscan(normalized_deltas, eps=0.10,MinPts = 3)
  purities = append(purities, get_model_purity(ds_db$cluster, data))
  confusion_matrix = get_model_confusion_matrix(ds_db$cluster, data)
  rand_indexes = append(rand_indexes, get_model_rand_index(confusion_matrix))
  sensitivities = append(sensitivities, get_model_sensitivity(confusion_matrix))
  specificities = append(specificities, get_model_specificity(confusion_matrix))
  scores = data.frame(  purity = purities,
                        rand_index = rand_indexes,
                        sensitivity = sensitivities,
                        specificity = specificities)
  
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

#one_shot(f = "./Anns/formatted/b1_f1/I03_ann.txt")

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
  }
  
  
  if ( generate_PDF ) {
    destination = paste("plots_b", i, "_f", i, ".pdf", sep="")
    pdf(file=paste(path,destination, sep=""), title=destination)
    print(paste("Saving to file: ", destination))
  }
  
  if ( generate_COR_ALL || generate_plot_ALL ) all_deltas = ""
  if ( generate_plot_ALL ) all_filter = ""
  
  knn_purity_means = c()
  knn_RI_means = c()
  knn_sensitivity_means = c()
  knn_specificity_means = c()
  
  mm_purity_means = c()
  mm_RI_means = c()
  mm_sensitivity_means = c()
  mm_specificity_means = c()
  
  db_purity_means = c()
  db_RI_means = c()
  db_sensitivity_means = c()
  db_specificity_means = c()
  
  for ( f in files ) {
    if (endsWith(f, ".txt")) {
      
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
        knn_scores = KNN_cross_validation(f, data, num_centers = 3)
        knn_purity_mean = mean(knn_scores$purity)
        knn_purity_means = append(knn_purity_means, knn_purity_mean)
        knn_purity_sd = sd(knn_scores$purity)
        knn_RI_mean = mean(knn_scores$rand_index)
        knn_RI_means = append(knn_RI_means, knn_RI_mean)
        knn_RI_sd = sd(knn_scores$rand_index)
        knn_specificity_mean = mean(knn_scores$specificity)
        knn_specificity_means = append(knn_specificity_means, knn_specificity_mean)
        knn_specificity_sd = sd(knn_scores$specificity)
        knn_sensitivity_mean = mean(knn_scores$sensitivity)
        knn_sensitivity_means = append(knn_sensitivity_means, knn_sensitivity_mean)
        knn_sensitivity_sd = sd(knn_scores$sensitivity)
        
        mm_scores = MM_cross_validation(f, data)
        mm_purity_mean = mean(mm_scores$purity)
        mm_purity_means = append(mm_purity_means, mm_purity_mean)
        mm_purity_sd = sd(mm_scores$purity)
        mm_RI_mean = mean(mm_scores$rand_index)
        mm_RI_means = append(mm_RI_means, mm_RI_mean)
        mm_RI_sd = sd(mm_scores$rand_index)
        mm_specificity_mean = mean(mm_scores$specificity)
        mm_specificity_means = append(mm_specificity_means, mm_specificity_mean)
        mm_specificity_sd = sd(mm_scores$specificity)
        mm_sensitivity_mean = mean(mm_scores$sensitivity)
        mm_sensitivity_means = append(mm_sensitivity_means, mm_sensitivity_mean)
        mm_sensitivity_sd = sd(mm_scores$sensitivity)
        
        db_scores = DB_cross_validation(f, data)
        db_purity_means = append(db_purity_means, db_scores$purity)
        db_RI_means = append(db_RI_means, db_scores$rand_index)
        db_specificity_means = append(db_specificity_means, db_scores$specificity)
        db_sensitivity_means = append(db_sensitivity_means, db_scores$sensitivity)
        
        
        total_scores = data.frame(row.names=c("KNN", "MM", "DBSCAN"),
                                  purity_mean=c(knn_purity_mean, mm_purity_mean, db_scores$purity),
                                  purity_sd=c(knn_purity_sd, mm_purity_sd, 0),
                                  RI_mean=c(knn_RI_mean, mm_RI_mean, db_scores$rand_index),
                                  RI_sd=c(knn_RI_sd, mm_RI_sd, 0),
                                  specificity_mean=c(knn_specificity_mean, mm_specificity_mean, db_scores$specificity),
                                  specificity_sd=c(knn_specificity_sd, mm_specificity_sd, 0),
                                  sensitivity_mean=c(knn_sensitivity_mean, mm_specificity_mean, db_scores$sensitivity),
                                  sensitivity_sd=c(knn_sensitivity_sd, mm_sensitivity_sd, 0)
        )
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
    
    total_scores = data.frame(row.names=c("KNN", "MM", "DBSCAN"),
                              purity_mean=c(mean(knn_purity_means), mean(mm_purity_means), mean(db_purity_means)),
                              purity_sd=c(sd(knn_purity_means), sd(mm_purity_means), sd(db_purity_means)),
                              RI_mean=c(mean(knn_RI_means), mean(mm_RI_means), mean(db_RI_means)),
                              RI_sd=c(sd(knn_RI_means), sd(mm_RI_means), sd(db_RI_means)),
                              specificity_mean=c(mean(knn_specificity_means), mean(mm_specificity_means), mean(db_specificity_means)),
                              specificity_sd=c(sd(knn_specificity_means), sd(mm_specificity_means), sd(db_specificity_means)),
                              sensitivity_mean=c(mean(knn_sensitivity_means), mean(mm_sensitivity_means), mean(db_sensitivity_means)),
                              sensitivity_sd=c(sd(knn_sensitivity_means), sd(mm_sensitivity_means), sd(db_sensitivity_means))
    )
    
    cat( c(paste("Clustering results overall")) ,file=file_output, sep="\n",append=TRUE)
    cat( "-," ,file=file_output, sep="",append=TRUE)
    write.table(total_scores, file=file_output, append = TRUE, sep=",")
  }
  
  if ( generate_PDF ) dev.off()
}

