### The BEGINNING ~~~~
##
# ~ Plots FST heatmap from fst calculator module | By Eduardo Charvel  and  Homère J. Alves Monteiro

# Cleans the environment ~ 
rm(list=ls())

#install.package("pacman")
# Loads required packages ~
pacman::p_load(pheatmap, tidyverse, reshape2)


# Loads Fst table ~
data <- read.table("/Users/sjr729/Desktop/GitHub/Skmer-2/with_fam_nas/wc_fst_mat_fam_nas.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE,)


#Remove the redundant first column, but the scalability of this need to be checked 


# Melt the Fst data frame to long format
melted_Fst <- melt(data, id.vars = "sample", variable.name = "SRR_pair", value.name = "Fst")
#melted_Fst <- melted_Fst[melted_Fst$sample != melted_Fst$SRR_pair, ]

#Replace NAs by 0
#melted_Fst$Fst <- ifelse(is.na(melted_Fst$Fst), 0, melted_Fst$Fst)

# Adds column names ~
colnames(melted_Fst) <- c("Pop1", "Pop2", "Weighted")


# Melts datasets ~
Fst_Pops = union(melted_Fst$Pop1, melted_Fst$Pop2)
n = length(Fst_Pops)
n

# Creates Fst-Fst matrix ~
Fst_Fst <- matrix(0, nrow = n, ncol = n, dimnames = list(Fst_Pops, Fst_Pops))
for (i in 1:nrow(Fst)) {
  Fst_Fst[melted_Fst[i, "Pop1"], melted_Fst[i, "Pop2"]] = melted_Fst[i, "Weighted"]
  Fst_Fst[melted_Fst[i, "Pop2"], melted_Fst[i, "Pop1"]] = melted_Fst[i, "Weighted"]}

Fst_Fst
# Gets Fst midpoint ~
Pops <- which(upper.tri(Fst_Fst), arr.ind = TRUE)
Fst_df <- data.frame(Site1 = dimnames(Fst_Fst)[[2]][Pops[, 2]],
                     Site2 = dimnames(Fst_Fst)[[1]][Pops[, 1]],
                     Value = Fst_Fst[Pops] %>% round(digits = 6))
Fstmiddle = max(Fst_df$Value) / 2
Fst_df
# Gets Fst label ~
Fst.label = expression(italic("F")[ST])

str(melted_Fst)

#Convert Pop2 to a character variable and reorder its levels to match the order of levels in Pop1.
melted_Fst$Pop2 <- factor(melted_Fst$Pop2, levels = levels(factor(melted_Fst$Pop1)))

Fst_Plot <- ggplot(data = melted_Fst, aes(x = Pop1, y = Pop2, fill = Weighted)) +
  geom_tile(color = "#ffffff", lwd = 1.5, linetype = 1, width = 1, height = 1) +
  coord_fixed() +
  geom_text(aes(label = round(Weighted, digits = 3)), color = "#ffffff", size = 2) +
  scale_fill_gradientn(colors = c("#fde0dd", "#e46e2a", "#481a03"), 
                       name = "Weighted Fst", 
                       limits = c(0, 1)) +
  scale_x_discrete(labels = Pop1) +
  scale_y_discrete(labels = Pop2, expand = c(0, 0), position = "right") +
  labs(x = "Population 1", y = "Population 2", 
       title = "Fst Values between Populations", 
       fill = "Fst") +
  theme(panel.background = element_rect(fill = "#ffffff"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 0.005, b = 0.005, r = .4, l = .4, unit = "cm"),
        axis.line = element_blank(),
        axis.text = element_text(colour = "#000000", size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.ticks = element_line(color = "#000000", size = .3),
        legend.position = "right",
        legend.key = element_rect(fill = NA),
        legend.background = element_blank(),
        legend.title = element_text(colour = "#000000", size = 12, face = "bold"),
        legend.text = element_text(colour = "#000000", size = 12, face = "bold"))


#print(Fst_Plot)

ggsave(Fst_Plot, file = "/Users/sjr729/Desktop/GitHub/Skmer-2/with_fam_nas/wc_fst_with_fam_nas_heatmap2.jpeg",
       scale = 1, width = 40, height = 40, dpi = 200)


