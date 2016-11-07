### USECASE: Download all gene expression data from BLUEPRINT samples
### and build a heatmap

# Load dependencies
library(DeepBlueR)
library(dplyr)
library(tidyr)
library(matrixStats)
library(stringr)
library(RColorBrewer)
library(gplots)

# List all BLUEPRINT samples
blueprint_samples <- deepblue_list_samples(
    extra_metadata = list("source" = "BLUEPRINT Epigenome"))

# Extract their ids
blueprint_samples_ids <- deepblue_extract_ids(blueprint_samples)

# Select gene expression data. We assign gene names using Gencode 22
gene_exprs_query <- deepblue_select_expressions(sample_ids = 
    blueprint_samples_ids, 
    expression_type = "gene",
    gene_model = "gencode v22")

# We request the data and define the output format
request = deepblue_get_regions(query_id = gene_exprs_query, 
   "@GENE_ID(gencode v22),FPKM,@BIOSOURCE,@SAMPLE_ID")

# We download the data
gene_regions <- deepblue_download_request_data(request)

# We retain a table mapping sample ids to bisources
sample_names <- dplyr::select(gene_regions, `@BIOSOURCE`, `@SAMPLE_ID`) %>% 
    dplyr::distinct()

# We filter out duplicated gene entries
genes_one_sample <- dplyr::filter(gene_regions, `@SAMPLE_ID` == "s10678")
duplicated_genes <- genes_one_sample[
    which(duplicated(genes_one_sample$`@GENE_ID(gencode v22)`)),
    "@GENE_ID(gencode v22)"]

# We convert the gene expression from a list to a data frame and subsequently...
genes_matrix = dplyr::filter(gene_regions, 
     !(`@GENE_ID(gencode v22)` %in% duplicated_genes)) %>% 
     dplyr::select(-`@BIOSOURCE`) %>%
     tidyr::spread(key = `@SAMPLE_ID`, value = FPKM)

# ...to a numeric matrix 
genes <- genes_matrix[,1]
genes_matrix <- data.matrix(genes_matrix[,-1])
rownames(genes_matrix) <- genes

### INTERMEDIATE OUTPUT
### genes_matrix : The gene expression matrix for all BLUEPRINT samples
### sample_names : A mapping table from sample id to cell type / biosource


### Generate a HEATMAP and to cluster samples by gene expression

# Select a unique color for each biosource

# We replace positive with + and negative with - to save some space
sample_names$`@BIOSOURCE` <- str_replace_all(sample_names$`@BIOSOURCE`, 
                                             "-positive", "+")
sample_names$`@BIOSOURCE` <- str_replace_all(sample_names$`@BIOSOURCE`, 
                                             "-negative", "-")

# We remove 'terminally differentiated' to save some space
sample_names$`@BIOSOURCE` <- str_replace(sample_names$`@BIOSOURCE`, 
                                         ", terminally differentiated", "")

# RColorBrewer palettes typically have around 9 colors. We need more
# and thus build a custom color scale based on the Set1 color scheme.
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

# We build a color map, i.e. for each cell type we assign a unique color
color_map <- data.frame(biosource = unique(sample_names$`@BIOSOURCE`), 
    color = getPalette(length(unique(sample_names$`@BIOSOURCE`))))

# We now use the color map to assign the colors to each experiment 
# according to its cell type / biosource
biosource_colors <- dplyr::left_join(sample_names, color_map, 
     by = c("@BIOSOURCE" = "biosource"))

# We transform this data frame into a vector 
color_vector <- as.character(biosource_colors$color)
names(color_vector) <-  biosource_colors$biosource

# We compute row variance
filtered_score_matrix <- genes_matrix
#filtered_score_matrix[filtered_score_matrix < 1] <- NA
filtered_score_matrix <- log2(filtered_score_matrix + 0.000000001)

filtered_score_matrix_rowVars <- rowVars(filtered_score_matrix, na.rm = T)
filtered_score_matrix_medians <- rowMedians(filtered_score_matrix, na.rm = T)

# We retain only genomic regions with variance > 0.05 for plotting
filtered_score_matrix <- filtered_score_matrix[
    which(filtered_score_matrix_rowVars > 70 &
          filtered_score_matrix_medians > -10),]

# We remove regions that have missing values in at least one of the experiments
#filtered_score_matrix <- filtered_score_matrix[
#    which(complete.cases(filtered_score_matrix)),]

# Order matrix by experiment names in sample map - this is important to make 
# sure you assign the correct cell type to each sample!
filtered_score_matrix <- filtered_score_matrix[,sample_names$`@SAMPLE_ID`]

# We set the margins for plotting. These values are optimized for plotting on A4
par(mar=c(5.1, 8.1, 4.1, 4.1), xpd=TRUE)

# We plot a heatmap in which the variable regions are shown across all samples
# On top of the columns we create a dendrogram based on Spearman correlation
# More precisely, we convert the Spearman correlation which is a similarity 
# measure to a distance such that it can be used with hierarchical clustering.
heatmap.2(filtered_score_matrix,labRow = NA, labCol = NA, trace = "none", 
          ColSideColors = color_vector,
          hclust=function(x) hclust(x,method="complete"),
          distfun=function(x) as.dist(1-cor(t(x), method = "pearson")), 
          Rowv = TRUE, dendrogram = "column", scale = "row",
          key.xlab = "scaled log2(FPKM) value", denscol = "black", keysize = 1.5,
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