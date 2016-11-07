# In this use case we will show how DeepBlueR can be used to generate a nice
# overview heatmap for all BLUEPRINT DNA Methylation experiments 

# Load packages for data retrieval, matrix operations and plotting
library(DeepBlueR)
library(gplots)
library(RColorBrewer)
library(matrixStats)
library(stringr)

# List all BLUEPRINT DNA methylation experiments
blueprint_DNA_meth <- deepblue_list_experiments(genome = "GRCh38", 
                          epigenetic_mark = "DNA Methylation",
                          technique = "Bisulfite-Seq",
                          project = "BLUEPRINT EPIGENOME")

# Filter for call files (opposed to coverage files)
blueprint_DNA_meth <- blueprint_DNA_meth[grep("bs_call", 
    deepblue_extract_names(blueprint_DNA_meth)),]

# Create a list of experiments with the appropriate column (beta value) seelcted
exp_columns <- deepblue_select_column(blueprint_DNA_meth, "VALUE")

# Select regions from the BLUEPRINT regulatory build
blueprint_regulatory_regions <- deepblue_select_annotations(
    annotation_name = "Blueprint Ensembl Regulatory Build",
    genome = "GRCh38")

# Some alternatives...

#tiling_regions <- deepblue_tiling_regions(size=5000, 
#                                          chromosome = "chr1", 
#                                          genome="GRCh38")

#cpg_islands <- deepblue_select_annotations(annotation_name = "Cpg Islands",
#                                           genome = "GRCh38",
#                                           chromosome = "chr1")

# Ask DeepBlue to build a score matrix in which each region of the regulatory
# build is aggregated for each experiment file. 
request_id <- deepblue_score_matrix(
    experiments_columns = exp_columns,
    aggregation_function = "mean", 
    aggregation_regions_id = blueprint_regulatory_regions)

# Download said score matrix
score_matrix <- deepblue_download_request_data(request_id = request_id)

# RColorBrewer palettes typically have around 9 colors. We need more
# and thus build a custom color scale based on the Set1 color scheme.
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

# We collect meta data for each experiment
experiments_info <- deepblue_info(deepblue_extract_ids(blueprint_DNA_meth))

# We extract the biosource name, i.e. the cell type
biosource <- unlist(lapply(experiments_info, function(x){ x$sample_info$biosource_name}))

# We replace positive with + and negative with - to save some space
biosource <- str_replace_all(biosource, "-positive", "+")
biosource <- str_replace_all(biosource, "-negative", "-")

# We remove 'terminally differentiated' to save some space
biosource <- str_replace(biosource, ", terminally differentiated", "")

# We collect experiment names 
exp_names <- unlist(lapply(experiments_info, function(x){ x$name}))

# We build a color map, i.e. for each cell type we assign a unique color
color_map <- data.frame(biosource = unique(biosource), 
                        color = getPalette(length(unique(biosource))))

# We now use the color map to assign the colors to each experiment 
# according to its cell type / biosource
biosource_colors <- data.frame(name = exp_names, biosource = biosource)
biosource_colors <- dplyr::left_join(biosource_colors, color_map, by = "biosource")

# We transform this data frame into a vector 
color_vector <- as.character(biosource_colors$color)
names(color_vector) <-  biosource_colors$biosource

# We remove the first three columns (CHROMOSOME, START, END)
# and convert the data frame to a numeric matrix
filtered_score_matrix <- as.matrix(score_matrix[,-c(1:3), with = FALSE])

# We compute row variance
filtered_score_matrix_rowVars <- rowVars(filtered_score_matrix, na.rm = T)

# We retain only genomic regions with variance > 0.05 for plotting
filtered_score_matrix <- filtered_score_matrix[which(filtered_score_matrix_rowVars > 0.05),]

# We remove regions that have missing values in at least one of the experiments
filtered_score_matrix <- filtered_score_matrix[which(complete.cases(filtered_score_matrix)),]

# Order matrix by experiment names in color map - this is important to make 
# sure you assign the correct cell type to each sample!
filtered_score_matrix <- filtered_score_matrix[,exp_names]

# We set the margins for plotting. These values are optimized for plotting on A4
par(mar=c(5.1, 8.1, 4.1, 4.1), xpd=TRUE)

# We plot a heatmap in which the variable regions are shown across all samples
# On top of the columns we create a dendrogram based on Spearman correlation
# More precisely, we convert the Spearman correlation which is a similarity 
# measure to a distance such that it can be used with hierarchical clustering.
heatmap.2(filtered_score_matrix,labRow = NA, labCol = NA, trace = "none", ColSideColors = color_vector,
          hclust=function(x) hclust(x,method="complete"),
          distfun=function(x) as.dist(1-cor(t(x), method = "pearson")), Rowv = TRUE, dendrogram = "column",
          key.xlab = "beta value", denscol = "black", keysize = 1.5,
          key.par = list(mar = c(8.5, 2.5, 1, 1)), key.title = NA)

# Next, we add a legend showing which cell type has which color
par(lend = 1)           # square line ends for the color legend
legend("bottomleft",      # location of the legend on the heatmap plot
       legend = color_map$biosource, # category labels
       col = as.character(color_map$color),  # color key
       text.width = 0.28,
       lty= 1,             
       lwd = 8, 
       cex = 0.7,
       y.intersp = 0.7,
       inset=c(-0.21,-0.11))

# Finished!