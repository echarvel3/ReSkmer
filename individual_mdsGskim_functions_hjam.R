## This script contains some essential functions for the individual level PCA and DAPC analysis and visualization butchered from Nicolas Lou
# These functions use a covariance matrix as the input
# Individual ID and population labels should also be supplied
# It can be imported to any R script using: source("individual_mdsGskim_functions_hjam.R")
# Please read each function for further details

## geom_enterotype ####################
geom_enterotype <- function(mapping = NULL, data = NULL, stat = "identity",  position = "identity",
                            alpha = 0.3, prop = 0.5, ..., lineend = "butt", linejoin = "round", 
                            linemitre = 1, arrow = NULL, na.rm = FALSE, parse = FALSE, 
                            nudge_x = 0, nudge_y = 0, label.padding = unit(0.10, "lines"), 
                            label.r = unit(0.10, "lines"), label.size = 0.1, 
                            show.legend = TRUE, inherit.aes = TRUE, show.point=T, show.label=T, show.ellipse=T, show.line=T) {
  ## This function is used to create an enterotype plot from two dimemsional data. Features can be turned on and off using show.point, show.label, show.ellipse, and show.line options. 
  library(ggplot2)
  # create new stat and geom for PCA scatterplot with ellipses
  StatEllipse <- ggproto("StatEllipse", Stat, 
                         required_aes = c("x", "y"), 
                         compute_group = function(., data, scales, level = 0.75, segments = 51, ...) {
                           library(MASS)
                           dfn <- 2
                           dfd <- length(data$x) - 1
                           if (dfd < 3) {
                             ellipse <- rbind(c(NA, NA))
                           } else {
                             v <- cov.trob(cbind(data$x, data$y))
                             shape <- v$cov
                             center <- v$center
                             radius <- sqrt(dfn * qf(level, dfn, dfd))
                             angles <- (0:segments) * 2 * pi/segments
                             unit.circle <- cbind(cos(angles), sin(angles))
                             ellipse <- t(center + radius * t(unit.circle %*% chol(shape)))
                           }
                           ellipse <- as.data.frame(ellipse)
                           colnames(ellipse) <- c("x", "y")
                           return(ellipse)
                         })
  
  # write new ggproto 
  GeomEllipse <- ggproto("GeomEllipse", Geom, 
                         draw_group = function(data, panel_scales, coord) {
                           n <- nrow(data)
                           if (n == 1) 
                             return(zeroGrob())
                           munched <- coord_munch(coord, data, panel_scales)
                           munched <- munched[order(munched$group), ]
                           first_idx <- !duplicated(munched$group)
                           first_rows <- munched[first_idx, ]
                           grid::pathGrob(munched$x, munched$y, default.units = "native", 
                                          id = munched$group, 
                                          gp = grid::gpar(col = first_rows$colour, 
                                                          fill = alpha(first_rows$fill, first_rows$alpha), lwd = first_rows$size * .pt, lty = first_rows$linetype))
                         }, 
                         default_aes = aes(colour = "NA", fill = "grey20", size = 0.5, linetype = 1, alpha = NA, prop = 0.5), 
                         handle_na = function(data, params) {
                           data
                         }, 
                         required_aes = c("x", "y"), 
                         draw_key = draw_key_path
  )
  
  # create a new stat for PCA scatterplot with lines which totally directs to the center
  StatConline <- ggproto("StatConline", Stat, 
                         compute_group = function(data, scales) {
                           library(miscTools)
                           library(MASS)
                           df <- data.frame(data$x,data$y)
                           mat <- as.matrix(df)
                           center <- cov.trob(df)$center
                           names(center)<- NULL 
                           mat_insert <- insertRow(mat, 2, center )
                           for(i in 1:nrow(mat)) {
                             mat_insert <- insertRow( mat_insert, 2*i, center )
                             next
                           }
                           mat_insert <- mat_insert[-c(2:3),]
                           rownames(mat_insert) <- NULL
                           mat_insert <- as.data.frame(mat_insert,center)
                           colnames(mat_insert) =c("x","y")
                           return(mat_insert)
                         },
                         required_aes = c("x", "y")
                         
  )
  
  # create a new stat for PCA scatterplot with center labels
  StatLabel <- ggproto("StatLabel" ,Stat,
                       compute_group = function(data, scales) {
                         library(MASS)
                         df <- data.frame(data$x,data$y)
                         center <- cov.trob(df)$center
                         names(center)<- NULL 
                         center <- t(as.data.frame(center))
                         center <- as.data.frame(cbind(center))
                         colnames(center) <- c("x","y")
                         rownames(center) <- NULL
                         return(center)
                       },
                       required_aes = c("x", "y")
  )
  
  
  layer1 <- layer(data = data, mapping = mapping, stat = stat, geom = GeomPoint, 
                  position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                  params = list(na.rm = na.rm, ...))
  layer2 <- layer(stat = StatEllipse, data = data, mapping = mapping, geom = GeomEllipse, position = position, show.legend = FALSE, 
                  inherit.aes = inherit.aes, params = list(na.rm = na.rm, prop = prop, alpha = alpha, ...))
  layer3 <- layer(data = data, mapping = mapping, stat =  StatConline, geom = GeomPath, 
                  position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                  params = list(lineend = lineend, linejoin = linejoin, 
                                linemitre = linemitre, arrow = arrow, alpha=0.3, na.rm = na.rm, ...))
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", 
           call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  layer4 <- layer(data = data, mapping = mapping, stat = StatLabel, geom = GeomLabel, 
                  position = position, show.legend = FALSE, inherit.aes = inherit.aes, 
                  params = list(parse = parse, label.padding = label.padding, 
                                label.r = label.r, label.size = label.size, na.rm = na.rm, ...))
  return(list(layer1,layer2,layer3,layer4)[c(show.point, show.ellipse, show.line, show.label)])
}

## PCA #############

PCA <- function(cov_matrix, ind_label, pop_label, x_axis, y_axis, show.point=T, show.label=T, show.ellipse=T, show.line=T, alpha=0, index_exclude=vector())
 {
  ## This function takes a covariance matrix and performs PCA. 
  # cov_matrix: a square covariance matrix generated by most pca softwares
  # ind_label: a vector in the same order and length as cov_matrix; it contains the individual labels of the individuals represented in the covariance matrix
  # pop_label: a vector in the same order and length as cov_matrix; it contains the population labels of the individuals represented in the covariance matrix
  # x_axis: an integer that determines which principal component to plot on the x axis
  # y_axis: an integer that determines which principal component to plot on the y axis
  # show.point: whether to show individual points
  # show.label: whether to show population labels
  # show.ellipse: whether to show population-specific ellipses
  # show.line: whether to show lines connecting population means with each individual point
  # alpha: the transparency of ellipses
  # index_exclude: the indices of individuals to exclude from the analysis
  index_include <- setdiff(seq_along(ind_label), index_exclude)
  m <- as.matrix(cov_matrix)
  m[is.na(m)]<- median(m, na.rm = T)
  m<-m[index_include, index_include] ## Remove 4SJH, four 3Ps individuals, and contaminated ones
  e <- eigen(m)
  e_value<-e$values
  x_variance<-e_value[x_axis]/sum(e_value)*100
  y_variance<-e_value[y_axis]/sum(e_value)*100
  e <- as.data.frame(e$vectors)
  e <- cbind(ind_label[index_include], pop_label[index_include], e) ## with the above individuals removed
  #colnames(e)[3:331]<-paste0("PC",1:329)
  colnames(e)[3:(dim(e)[1])]<-paste0("PC",1:(dim(e)[1]-2)) ## with the above individuals removed
  colnames(e)[1:2]<-c("individual", "population")
  assign("pca_table", e, .GlobalEnv)
  
  PCA_plot<-ggplot(data=e[,],aes(x=e[,x_axis+2], y=e[,y_axis+2], color=population,label=population, shape=population)) + 
    geom_enterotype(alpha=alpha, show.point=show.point, show.label=show.label, show.ellipse=show.ellipse, show.line=show.line) +
    scale_colour_manual(values =c( "#A02353", 
                                   "#CC480C",  
                                   "#969696",
                                   "#000000",
                                   "#D38C89", 
                                   "#C89AD1", 
                                   "#7210A0", 
                                   "#91BD96", 
                                   "#02630C", 
                                   "#588cad", 
                                   "#240377"   ))+
    scale_shape_manual(values=c(16,17,18,
                              16,
                              18,
                              16,17,
                              11,
                              16,
                              16, 17, 18, 
                              18,16,
                              17,16,
                              17,18,
                              10, 6, 9, 8, 14,11,
                              10, 6, 9, 8, 14,
                              10, 6, 9, 8, 14,
                              16, 17, 18, 15))+   
    xlab(paste0("PC", x_axis, "(",round(x_variance,2),"%)"))+
    ylab(paste0("PC", y_axis ,"(",round(y_variance,2),"%)"))+
    theme(legend.key = element_blank()) +
    theme(legend.title=element_blank()) +
    theme(axis.title.x = element_text(size = 10, color="#000000", face="bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 10, color="#000000", face="bold", margin = margin(t = 0, r = 20, b = 0, l = 0))) +
    theme(legend.text=element_text(size=11)) +
    theme(panel.background = element_rect(fill = '#FAFAFA')) +
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
    theme(axis.line = element_line(colour = "#000000", size = 0.3)) +
    theme(panel.border = element_blank())

  print(PCA_plot)
}

## DAPC ########

DAPC <- function (n=50, x_axis, y_axis, show.point=T, show.label=T, show.ellipse=T, show.line=T, alpha=0) {
  ## This function should follow immediately after a PCA function. It takes the PCA output and performs a linear discriminant analysis
  # n: the number of principal components to use for discriminant analysis
  # x_axis: an integer that determines which linear discriminant to plot on the x axis
  # y_axis: an integer that determines which linear discriminant to plot on the y axis
  # show.point: whether to show individual points
  # show.label: whether to show population labels
  # show.ellipse: whether to show population-specific ellipses
  # show.line: whether to show lines connecting population means with each individual point
  # alpha: the transparency of ellipses
  fit <- lda(population ~ ., data=pca_table[,2:(n+1)], na.action="na.omit", CV=F, output = "Scatterplot")
  plda <- predict(object = fit,
                  newdata = pca_table[,2:(n+1)])
  prop.lda = fit$svd^2/sum(fit$svd^2)
  dataset = data.frame(group = pca_table[,2], lda = plda$x)
  DAPC_plot<- ggplot(dataset, aes(dataset[,1+x_axis], dataset[,1+y_axis], color= group, label=group, shape=group)) + 
    geom_enterotype(show.point=show.point, show.label=show.label, show.ellipse=show.ellipse, show.line=show.line, alpha=alpha) +
    scale_colour_manual(values =c( "#A02353", 
                                   "#CC480C",  
                                   "#969696",
                                   "#000000",
                                   "#D38C89", 
                                   "#C89AD1", 
                                   "#7210A0", 
                                   "#91BD96", 
                                   "#02630C", 
                                   "#588cad", 
                                   "#240377"  ))+
    scale_shape_manual(values=c(16,17,18,
                              16,
                              18,
                              16,17,
                              11,
                              16,
                              16, 17, 18, 
                              18,16,
                              17,16,
                              17,18,
                              10, 6, 9, 8, 14,11,
                              10, 6, 9, 8, 14,
                              10, 6, 9, 8, 14,
                              16, 17, 18, 15))+   
    theme_cowplot() +
    labs(x = paste0("LD", x_axis," (", percent(prop.lda[x_axis]), ")", sep=""),
         y = paste0("LD", y_axis, " (", percent(prop.lda[y_axis]), ")", sep=""))+
    theme(legend.key = element_blank()) +
    theme(legend.title=element_blank()) +
    theme(axis.title.x = element_text(size = 10, color="#000000", face="bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 10, color="#000000", face="bold", margin = margin(t = 0, r = 20, b = 0, l = 0))) +
    theme(legend.text=element_text(size=11)) +
    theme(panel.background = element_rect(fill = '#FAFAFA')) +
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
    theme(axis.line = element_line(colour = "#000000", size = 0.3)) +
    theme(panel.border = element_blank())

  print(DAPC_plot)
}

## PCoA ########

PCoA <- function(dist_matrix, ind_label, pop_label, k, x_axis, y_axis, show.point=T, show.label=T, show.ellipse=T, show.line=T, alpha=0, index_exclude=vector())
{
  ## This function takes a pairwise distance matrix and performs PCoA
  # dist_matrix: a square distance matrix generated by most pca softwares
  # ind_label: a vector in the same order and length as dist_matrix; it contains the individual labels of the individuals represented in the covariance matrix
  # pop_label: a vector in the same order and length as dist_matrix; it contains the population labels of the individuals represented in the covariance matrix
  # x_axis: an integer that determines which principal component to plot on the x axis
  # y_axis: an integer that determines which principal component to plot on the y axis
  # show.point: whether to show individual points
  # show.label: whether to show population labels
  # show.ellipse: whether to show population-specific ellipses
  # show.line: whether to show lines connecting population means with each individual point
  # alpha: the transparency of ellipses
  # index_exclude: the indices of individuals to exclude from the analysis
  
  index_include <- setdiff(seq_along(ind_label), index_exclude)
  m <- as.matrix(dist_matrix)
  m[is.na(m)]<- median(m, na.rm = T)
  m <- m[index_include, index_include] 
  mds <- cmdscale(as.dist(m), k=k)
  mds <- as.data.frame(mds)
  colnames(mds) <- paste0("dist_", 1:k)
  mds <- cbind(ind_label[index_include], pop_label[index_include], mds)
  colnames(mds)[1:2]<-c("individual", "population")
  eigen_value <- cmdscale(as.dist(m), k=k, eig = T)$eig
  var_explained <- round(eigen_value/sum(eigen_value)*100, 2)
  assign("pcoa_table", mds, .GlobalEnv)
  assign("var_explained", var_explained, .GlobalEnv)
  
  PCoA_plot<-ggplot(data=mds[,], aes(x=mds[,x_axis+2], y=mds[,y_axis+2], color=population,label=population, shape=population)) + 
    geom_enterotype(alpha=alpha, show.point=show.point, show.label=show.label, show.ellipse=show.ellipse, show.line=show.line) +
    scale_colour_manual(values =c( "#A02353", 
                                 "#CC480C",  
                                 "#969696",
                                 "#000000",
                                 "#D38C89", 
                                 "#C89AD1", 
                                 "#7210A0", 
                                 "#91BD96", 
                                 "#02630C", 
                                 "#588cad", 
                                 "#240377" ))+
    scale_shape_manual(values=c(16,17,18,
                              16,
                              18,
                              16,17,
                              11,
                              16,
                              16, 17, 18, 
                              18,16,
                              17,16,
                              17,18,
                              10, 6, 9, 8, 14,11,
                              10, 6, 9, 8, 14,
                              10, 6, 9, 8, 14,
                              16, 17, 18, 15))+   
    theme_cowplot() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    ) +
    xlab(paste0("PCo", x_axis, "(",var_explained[x_axis],"%)")) +
    ylab(paste0("PCo", y_axis ,"(",var_explained[y_axis],"%)"))
  print(PCoA_plot)
}



## PCoA2, with continuous variables ########

PCoA2 <- function(dist_matrix, ind_label, pop_label, k, x_axis, y_axis, show.point=T, show.label=T, show.ellipse=T, show.line=T, alpha=0, index_exclude=vector())
{
  ## This function takes a pairwise distance matrix and performs PCoA
  # dist_matrix: a square distance matrix generated by most pca softwares
  # ind_label: a vector in the same order and length as dist_matrix; it contains the individual labels of the individuals represented in the covariance matrix
  # pop_label: a vector in the same order and length as dist_matrix; it contains the population labels of the individuals represented in the covariance matrix
  # x_axis: an integer that determines which principal component to plot on the x axis
  # y_axis: an integer that determines which principal component to plot on the y axis
  # show.point: whether to show individual points
  # show.label: whether to show population labels
  # show.ellipse: whether to show population-specific ellipses
  # show.line: whether to show lines connecting population means with each individual point
  # alpha: the transparency of ellipses
  # index_exclude: the indices of individuals to exclude from the analysis
  
  index_include <- setdiff(seq_along(ind_label), index_exclude)
  m <- as.matrix(dist_matrix)
  m[is.na(m)]<- median(m, na.rm = T)
  m <- m[index_include, index_include] ## Remove 4SJH, four 3Ps individuals, and contaminated ones
  mds <- cmdscale(as.dist(m), k=k)
  mds <- as.data.frame(mds)
  colnames(mds) <- paste0("dist_", 1:k)
  mds <- cbind(ind_label[index_include], pop_label[index_include], mds)
  colnames(mds)[1:2]<-c("individual", "population")
  eigen_value <- cmdscale(as.dist(m), k=k, eig = T)$eig
  var_explained <- round(eigen_value/sum(eigen_value)*100, 2)
  assign("pcoa_table", mds, .GlobalEnv)
  assign("var_explained", var_explained, .GlobalEnv)
  
  PCoA2_plot<-ggplot(data=mds[,], aes(x=mds[,x_axis+2], y=mds[,y_axis+2], color=population,label=population)) + 
    geom_enterotype(alpha=alpha, show.point=show.point, show.label=show.label, show.ellipse=show.ellipse, show.line=show.line) +
    scale_color_discrete()+
    theme_cowplot() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    ) +
    xlab(paste0("PCo", x_axis, "(",var_explained[x_axis],"%)")) +
    ylab(paste0("PCo", y_axis ,"(",var_explained[y_axis],"%)"))
  print(PCoA2_plot)
}