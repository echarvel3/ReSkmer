
library(data.table)
library(ggplot2)

setwd("~/Desktop/calab_data/code_bases/Skmer-2/R_skmer/")
options(scipen = 100, digits = 5)

skmer1_mtrx = fread("./skmer1_matrices/skmer1_moss_skims_dist_mat.txt", header=TRUE)
genome_dists = skmer1_mtrx[1]

true_dist =  data.table(true_dist = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2), 
                               c1 = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2),
                               c2 = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2),
                               c4 = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2),
                               c8 = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2))
  
skmer1_dists = data.table(true_dist = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2), 
                          c1 = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2),
                          c2 = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2),
                          c4 = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2),
                          c8 = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2))

for (x in c(2:ncol(genome_dists))){
  name = names(genome_dists[,..x])
  name_split = strsplit(name, split = "_")
  t_d = name_split[[1]][2]
  t_d = ifelse(t_d=="genome", 0, t_d)
  cov = paste("c", name_split[[1]][3], sep = "")
  
  skmer1_dists[(true_dist == t_d), (cov) := genome_dists[,..x][[1]]]
}

skmer1_dists = skmer1_dists[,c(1,2,3,4,5)]
true_dist = true_dist[,c(1,2,3,4,5)]

error_matrix = skmer1_dists - true_dist
error_matrix$true_dist = c("0", "0.001", "0.003", "0.005", "0.007", "0.01", "0.1", "0.2")
error_melt = melt(error_matrix, id.vars = c("true_dist"))

skmer1 = ggplot(data = error_melt, aes(x = variable, y = true_dist, fill = value)) +
  geom_tile() + geom_text(aes(label=round(value, digits = 5))) +
  scale_fill_distiller(palette = "BrBG") + ggtitle("Skmer1 Error")
skmer1

#############

skmer2_dists = data.table(true_dist = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2), 
                          c1 = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2),
                          c2 = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2),
                          c4 = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2),
                          c8 = c(0, 0.001, 0.003, 0.005, 0.007, 0.01, 0.1, 0.2))

skmer2_mtrx = fread("./skmer2_matrices/skmer2_moss_skims_dist_mat.txt", header=TRUE)
genome_dists = skmer2_mtrx[1]

for (x in c(2:ncol(genome_dists))){
  name = names(genome_dists[,..x])
  name_split = strsplit(name, split = "_")
  t_d = name_split[[1]][2]
  t_d = ifelse(t_d=="genome", 0, t_d)
  cov = paste("c", name_split[[1]][3], sep = "")
  
  skmer2_dists[(true_dist == t_d), (cov) := genome_dists[,..x][[1]]]
}

skmer2_dists = skmer2_dists[,c(1,2,3,4,5)]
true_dist = true_dist[,c(1,2,3,4,5)]

error_matrix = skmer2_dists - true_dist
error_matrix$true_dist = c("0", "0.001", "0.003", "0.005", "0.007", "0.01", "0.1", "0.2")
error_melt = melt(error_matrix, id.vars = c("true_dist"))

skmer2 = ggplot(data = error_melt, aes(x = variable, y = true_dist, fill = value)) +
  geom_tile() + geom_text(aes(label=round(value, digits = 5))) +
  scale_fill_distiller(palette = "BrBG") + ggtitle("Skmer2 Error")
skmer2

par(mfrow = c(1, 2))
skmer1
skmer2
