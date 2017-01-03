## ---- eval = FALSE, echo=TRUE, warning=FALSE, message=FALSE--------------
#  source("https://bioconductor.org/biocLite.R")
#  biocLite("DeepBlueR")

## ---- echo=FALSE, warning=FALSE, message=FALSE, error=FALSE--------------
library(DeepBlueR)

## ---- echo=TRUE, eval = FALSE, warning=FALSE, message=FALSE--------------
#  deepblue_info("me")

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
# We are selecting the experiments with terms 'H3k27AC', 'blood', and
# 'peak' in the metadata.
experiments_found = deepblue_search(
    keyword="'H3k27AC' 'blood' 'peak'", type="experiments")

custom_table = do.call("rbind", apply(experiments_found, 1, function(experiment){
  experiment_id = experiment[1]
  # Obtain the information about the experiment_id
  info = deepblue_info(experiment_id)

  # Print the experiment name, project, biosource, and epigenetic mark.
  with(info, { data.frame(name = name, project = project,
    biosource = sample_info$biosource_name, epigenetic_mark = epigenetic_mark)
      })
}))
  head(custom_table)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
experiments = deepblue_list_experiments(type="peaks", epigenetic_mark="H3K4me3",
    biosource=c("inflammatory macrophage", "macrophage"),
    project="BLUEPRINT Epigenome")

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
info = deepblue_info("e30000")
print(info$extra_metadata$file_url)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue_select_experiments(
    experiment_name=c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed",
        "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"))
# Count how many regions where selected
request_id = deepblue_count_regions(query_id=query_id)
# Download the request data as soon as processing is finished
requested_data = deepblue_download_request_data(request_id=request_id)
print(paste("The selected experiments have", requested_data, "regions."))

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue_select_experiments (
    experiment_name = c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed",
        "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"),
        chromosome="chr1", start=0, end=50000000)

# Retrieve the experiments data (The @NAME meta-column is used to include the
# experiment name and @BIOSOURCE for experiment's biosource
request_id = deepblue_get_regions(query_id=query_id,
    output_format="CHROMOSOME,START,END,SIGNAL_VALUE,PEAK,@NAME,@BIOSOURCE")
regions = deepblue_download_request_data(request_id=request_id)
regions

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
samples = deepblue_list_samples(
    biosource="myeloid cell",
    extra_metadata = list("source" = "BLUEPRINT Epigenome"))
samples_ids = deepblue_extract_ids(samples)
query_id = deepblue_select_regions(genome="GRCh38", sample=samples_ids,
    chromosome="chr1", start=0, end=50000)
request_id = deepblue_get_regions(query_id=query_id,
    output_format="CHROMOSOME,START,END,@NAME,@SAMPLE_ID,@BIOSOURCE")
regions = deepblue_download_request_data(request_id=request_id)
head(regions,1)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue_select_experiments(
    experiment_name = c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed",
        "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"),
    chromosome="chr1", start=0, end=50000000)
query_id_filter_signal = deepblue_filter_regions(
    query_id=query_id, field="SIGNAL_VALUE", operation=">",
    value="10", type="number")
query_id_filters = deepblue_filter_regions(
    query_id=query_id_filter_signal, field="PEAK", operation=">",
    value="1000", type="number")
request_id = deepblue_get_regions(query_id=query_id_filters,
    output_format="CHROMOSOME,START,END,SIGNAL_VALUE,PEAK,@NAME,@BIOSOURCE")
regions = deepblue_download_request_data(request_id=request_id)
regions

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue_select_experiments(
    experiment_name = c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed",
        "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"),
    chromosome="chr1", start=0, end=50000000)
promoters_id = deepblue_select_annotations(annotation_name="promoters",
    genome="GRCh38", chromosome="chr1")
intersect_id = deepblue_intersection(
    query_data_id=query_id, query_filter_id=promoters_id)
request_id = deepblue_get_regions(
    query_id=intersect_id,
    output_format="CHROMOSOME,START,END,SIGNAL_VALUE,PEAK,@NAME,@BIOSOURCE")
regions = deepblue_download_request_data(request_id=request_id)
regions

## ---- message=FALSE, error=FALSE, warning=FALSE--------------------------
library(Gviz)
atrack <- AnnotationTrack(regions, name = "Overlapping regions",
    group = regions$`@BIOSOURCE`, genome="hg38")
gtrack <- GenomeAxisTrack()
itrack <- IdeogramTrack(genome = "hg38", chromosome = "chr1")
plotTracks(list(itrack, atrack, gtrack), groupAnnotation="group", fontsize=18,
           background.panel = "#FFFEDB", background.title = "darkblue")

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue_select_experiments(
    experiment_name = c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed",
        "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"),
    chromosome="chr1", start=0, end=50000000)
query_id_filter_signal = deepblue_filter_regions(query_id=query_id,
    field="SIGNAL_VALUE", operation=">", value="10", type="number")
query_id_filters = deepblue_filter_regions(query_id=query_id_filter_signal,
    field="PEAK", operation=">", value="1000", type="number")
query_id_filter_length = deepblue_filter_regions (query_id=query_id_filters,
    field="@LENGTH", operation="<", value="2000", type="number")
request_id = deepblue_get_regions(query_id=query_id_filter_length,
    output_format="CHROMOSOME,START,END,@NAME,@BIOSOURCE,@LENGTH,@SEQUENCE")
regions = deepblue_download_request_data(request_id=request_id)
head(regions, 1)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
tataa_regions = deepblue_select_annotations(
    annotation_name="Pattern TATAAA (non-overlap) in the genome GRCh38",
    genome="GRCh38", chromosome="chr1")
query_id = deepblue_select_experiments(
    experiment_name= c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed",
        "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"),
    chromosome="chr1", start=0, end=50000000)
overlapped = deepblue_intersection(query_data_id=query_id,
                                   query_filter_id=tataa_regions)
request_id=deepblue_get_regions(overlapped,
    "CHROMOSOME,START,END,SIGNAL_VALUE,PEAK,@NAME,@BIOSOURCE,@LENGTH,@SEQUENCE,@PROJECT")
regions = deepblue_download_request_data(request_id=request_id)
head(regions, 3)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
experiment_data = deepblue_select_experiments(
  "DG-75_c01.ERX297417.H3K27ac.bwa.GRCh38.20150527.bed")
fmt = "CHROMOSOME,START,END,@LENGTH,@COUNT.MOTIF(C),@COUNT.MOTIF(G),@COUNT.MOTIF(CG),@COUNT.MOTIF(GC),@SEQUENCE"
request_id=deepblue_get_regions(experiment_data, fmt)
regions = deepblue_download_request_data(request_id=request_id)
head(regions, 3)

## ---- echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE----------------
#  q_genes = deepblue_select_genes(genes="RP11-34P13", gene_model="gencode v23")
#  q_filter = deepblue_filter_regions(query_id=q_genes,
#      field="@GENE_ATTRIBUTE(gene_type)", operation="==",
#      value="lincRNA", type="string")
#  request_id=deepblue_get_regions(q_filter, "CHROMOSOME,START,END,GTF_ATTRIBUTES")
#  regions = deepblue_download_request_data(request_id=request_id)
#  regions

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue_select_experiments (
    experiment=c("GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20160531.wig"),
    chromosome="chr1", start=0, end=50000000)
cpg_islands = deepblue_select_annotations(annotation_name="CpG Islands",
    genome="GRCh38", chromosome="chr1", start=0, end=50000000)
# Aggregate
overlapped = deepblue_aggregate (data_id=query_id, ranges_id=cpg_islands,
    column="VALUE" )

# Retrieve the experiments data (The @NAME meta-column is used to include
# the experiment name and @BIOSOURCE for experiment's biosource
request_id = deepblue_get_regions(query_id=overlapped,
    output_format=
      "CHROMOSOME,START,END,@AGG.MIN,@AGG.MAX,@AGG.MEAN,@AGG.VAR")
regions = deepblue_download_request_data(request_id=request_id)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
hsc_children = deepblue_get_biosource_children("hematopoietic stem cell")

hsc_children_name = deepblue_extract_names(hsc_children)

hsc_children_samples = deepblue_list_samples(
    biosource = hsc_children_name,
    extra_metadata = list(source="BLUEPRINT Epigenome"))

hsc_samples_ids = deepblue_extract_ids(hsc_children_samples)

# Note that BLUEPRINT uses Ensembl Gene IDs
gene_exprs_query = deepblue_select_expressions(
    expression_type = "gene",
    sample_ids = hsc_samples_ids,
    identifiers = c("ENSG00000074771.3", "ENSG00000188747.7", "ENSG00000086991.11"),
    gene_model = "gencode v22")

request_id = deepblue_get_regions(
    query_id = gene_exprs_query,
    output_format ="@GENE_NAME(gencode v22),CHROMOSOME,START,END,FPKM,@BIOSOURCE")

regions = deepblue_download_request_data(request_id = request_id)
regions

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
# Selecting the data from 2 experiments:
#    GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20160531.wig
# As we already know the experiments names, we keep all others fields empty.
# We are selecting all regions of chromosome 1
query_id = deepblue_select_experiments(
    experiment=c("GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20160531.wig"),
    chromosome="chr1")

#  Tiling regions of 100.000 base pairs
tiling_id = deepblue_tiling_regions(size=100000,
    genome="GRCh38", chromosome="chr1")

# Aggregate
overlapped = deepblue_aggregate (data_id=query_id,
    ranges_id=tiling_id, column="VALUE")

# Retrieve the experiments data (The @NAME meta-column is used to include the
# experiment name and @BIOSOURCE for experiment's biosource
request_id = deepblue_get_regions(query_id=overlapped,
    output_format="CHROMOSOME,START,END,@AGG.MEAN,@AGG.SD")

regions = deepblue_download_request_data(request_id=request_id)
regions

## ------------------------------------------------------------------------
library(ggplot2)
plot_data <- as.data.frame(regions)
plot_data[,grepl("X.", colnames(plot_data))] <-
    apply(plot_data[,grepl("X.", colnames(plot_data))], 2, as.numeric)
AGG.plot <- ggplot(plot_data, aes(start)) +
    geom_ribbon(aes(ymin = X.AGG.MEAN - (X.AGG.SD / 2),
        ymax = X.AGG.MEAN + (X.AGG.SD / 2)), fill = "grey70") +
    geom_line(aes(y = X.AGG.MEAN))
print(AGG.plot)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
# Select the RP11-34P13 gene locations from gencode v23
q_genes = deepblue_select_genes(
    genes=
        c("RNU6-1100P", "CICP7", "MRPL20", "ANKRD65",
            "HES2", "ACOT7", "HES3", "ICMT"), gene_model="gencode v19")

# Obtain the regions that starts 2500 bases pair before the regions start and
# have 2000 base pairs.
# The 4th argument inform that DeepBlue must consider the region strand
# (column STRAND) to calculate the new region
before_flank_id = deepblue_flank(query_id=q_genes,
     start=-2500, length=2000, use_strand=TRUE)

# Obtain the regions that starts 1500 bases pair after the
# regions end and have 500 base pairs.
# The 4th argument inform that DeepBlue must consider the
# region strand (column STRAND) to calculate the new region
after_flank_id = deepblue_flank(query_id=q_genes,
    start=1500, length=500, use_strand=TRUE)

# Merge both flanking regions set and genes set
flank_merge_id = deepblue_merge_queries(
    query_a_id =before_flank_id, query_b_id=after_flank_id)
all_merge_id = deepblue_merge_queries(
    query_a_id=q_genes, query_b_id=flank_merge_id)

# Request the regions
request_id = deepblue_get_regions(query_id=all_merge_id,
    output_format="CHROMOSOME,START,END,STRAND,@LENGTH")

regions = deepblue_download_request_data(request_id=request_id)
regions

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
# Select the RP11-34P13 gene locations from gencode v23

# Selecting the data from 2 experiments:
#    GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20160531.wig
# As we already know the experiments names, we keep all others fields empty.
# We are selecting all regions of chromosome 1
query_id = deepblue_select_experiments(
    experiment="GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20160531.wig",
    chromosome="chr1")

# Select the CpG Islands annotation from GRCh38
cpg_islands = deepblue_select_annotations(
    annotation="CpG Islands", genome="GRCh38", chromosome="chr1")

# Aggregate
overlapped = deepblue_aggregate(
    data_id=query_id, ranges_id=cpg_islands, column="VALUE")

# Select the aggregated regions that aggregated at least one region from the
# selected experiments (@AGG.COUNT > 0)
filtered = deepblue_filter_regions(query_id=overlapped,
    field="@AGG.COUNT", operation=">", value="0", type="number")

# We remove all regions where the aggregation mean is zero.
filtered_zeros = deepblue_filter_regions(query_id=filtered,
    field="@AGG.MEAN", operation="!=", value="0.0", type="number")

# Retrieve the experiments data (The @NAME meta-column is used to include the
# experiment name and @BIOSOURCE for experiment's biosource
request_id = deepblue_get_regions(query_id=filtered_zeros,
    output_format=
    "CHROMOSOME,START,END,@CALCULATED(return math.log(value_of('@AGG.MEAN'))),@AGG.MEAN,@AGG.COUNT")

regions = deepblue_download_request_data(request_id=request_id)

# We have to perform a manual conversion because the
# package can't know the type for calculated columns
regions$`@CALCULATED(return math.log(value_of('@AGG.MEAN')))` =
    as.numeric(regions$`@CALCULATED(return math.log(value_of('@AGG.MEAN')))`)

head(regions, 5)

## ---- warning=FALSE, error=FALSE-----------------------------------------
library(Gviz)
atrack <- AnnotationTrack(regions,
    name = "CpGs", group = regions$`@BIOSOURCE`, genome="hg38")
gtrack <- GenomeAxisTrack()
itrack <- IdeogramTrack(genome = "hg38", chromosome = "chr1")
dtrack <- DataTrack(regions,
    data="@AGG.MEAN", name = "Log of average methylation value")
plotTracks(list(itrack, atrack, dtrack, gtrack), type="histogram", fontsize=18,
           background.panel = "#FFFEDB", background.title = "darkblue")

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
experiments =
    c("GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20160531.wig",
        "C003N351.CPG_methylation_calls.bs_call.GRCh38.20160531.wig",
        "C005VG51.CPG_methylation_calls.bs_call.GRCh38.20160531.wig",
        "S002R551.CPG_methylation_calls.bs_call.GRCh38.20160531.wig",
        "NBC_NC11_41.CPG_methylation_calls.bs_call.GRCh38.20160531.wig",
        "bmPCs-V156.CPG_methylation_calls.bs_call.GRCh38.20160531.wig",
        "S00BS451.CPG_methylation_calls.bs_call.GRCh38.20160531.wig",
        "S00D1DA1.CPG_methylation_calls.bs_call.GRCh38.20160531.wig",
        "S00D39A1.CPG_methylation_calls.bs_call.GRCh38.20160531.wig")

experiments_columns = list()
for (experiment_name in experiments) {
    experiments_columns[[experiment_name]] = "VALUE"
}

cpgs = deepblue_select_annotations(
    annotation_name="Cpg Islands",
    chromosome="chr22", start=0, end=18000000, genome="GRCh38")

request_id = deepblue_score_matrix(
    experiments_columns=experiments_columns,
    aggregation_function="mean", aggregation_regions_id=cpgs)

score_matrix = deepblue_download_request_data(request_id=request_id)
head(score_matrix, 5)

## ---- fig.width = 11, warning=FALSE, echo=TRUE, error=FALSE, eval=TRUE----
library(ggplot2)
score_matrix_plot = tidyr::gather(score_matrix,
    "experiment", "methylation", -CHROMOSOME, -START, -END)
score_matrix_plot$START <- as.factor(score_matrix_plot$START)
ggplot(score_matrix_plot, aes(x=START, y=experiment, fill=methylation)) +
    geom_tile() +
    theme(axis.text.x=element_text(angle=-90))

## ------------------------------------------------------------------------
deepblue_export_tab(score_matrix, file.name = "my_score_matrix")

## ---- eval=FALSE---------------------------------------------------------
#  request_id = deepblue_get_regions(query_id=overlapped,
#      output_format="CHROMOSOME,START,END,@AGG.MEAN,@AGG.SD")
#  
#  regions = deepblue_download_request_data(request_id=request_id)
#  deepblue_export_bed(regions,
#                      file.name = "my_tiling_regions",
#                      score.field = "@AGG.MEAN")

## ---- eval=FALSE---------------------------------------------------------
#  exp_id <- deepblue_name_to_id(
#      "GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20160531.wig",
#      collection = "experiments")$id
#  
#  deepblue_export_meta_data(exp_id, file.name = "GC_T14")

## ---- eval=FALSE---------------------------------------------------------
#  deepblue_export_meta_data(list("e30035", "e30036"),
#  file.name = "test_export")

## ---- eval=FALSE---------------------------------------------------------
#  experiments = deepblue_list_experiments(type="peaks", epigenetic_mark="H3K4me3",
#      biosource=c("inflammatory macrophage", "macrophage"),
#      project="BLUEPRINT Epigenome")
#  experiment_names = deepblue_extract_names(experiments)
#  
#  request_ids = foreach(experiment = experiment_names) %do%{
#    query_id = deepblue_select_experiments(experiment_name = experiment,
#                                          chromosome = "chr21")
#  
#    request_id = deepblue_get_regions(query_id =query_id,
#      output_format = "CHROMOSOME,START,END")
#  }
#  request_data = deepblue_batch_export_results(request_ids,
#                               target.directory = "BLUEPRINT macrophages chr21")

## ----dependencies, message=FALSE, warning=FALSE--------------------------
library(DeepBlueR)
library(gplots)
library(RColorBrewer)
library(matrixStats)
library(stringr)

## ------------------------------------------------------------------------
blueprint_DNA_meth <- deepblue_list_experiments(genome = "GRCh38",
                          epigenetic_mark = "DNA Methylation",
                          technique = "Bisulfite-Seq",
                          project = "BLUEPRINT EPIGENOME")

blueprint_DNA_meth

## ------------------------------------------------------------------------
blueprint_DNA_meth <- blueprint_DNA_meth[grep("bs_call",
    deepblue_extract_names(blueprint_DNA_meth)),]

blueprint_DNA_meth

## ------------------------------------------------------------------------
exp_columns <- list(nrow(blueprint_DNA_meth))

for(i in 1:nrow(blueprint_DNA_meth)){
    exp_columns[[i]] <- "VALUE"
}
names(exp_columns) <- deepblue_extract_names(blueprint_DNA_meth)

## ------------------------------------------------------------------------
exp_columns <- deepblue_select_column(blueprint_DNA_meth, "VALUE")

## ------------------------------------------------------------------------
blueprint_regulatory_regions <- deepblue_select_annotations(
    annotation_name = "Blueprint Ensembl Regulatory Build",
    genome = "GRCh38")

blueprint_regulatory_regions

## ---- eval=FALSE---------------------------------------------------------
#  deepblue_select_annotations(annotation_name = "Cpg Islands",
#                              genome = "GRCh38")

## ------------------------------------------------------------------------
deepblue_list_annotations(genome = "GRCh38")

## ---- eval=FALSE---------------------------------------------------------
#  tiling_regions <- deepblue_tiling_regions(size=5000,
#                                            genome="GRCh38")

## ------------------------------------------------------------------------
request_id <- deepblue_score_matrix(
    experiments_columns = exp_columns,
    aggregation_function = "mean",
    aggregation_regions_id = blueprint_regulatory_regions)

request_id

## ------------------------------------------------------------------------
deepblue_info(request_id)$state

## ------------------------------------------------------------------------
score_matrix <- deepblue_download_request_data(request_id = request_id)
score_matrix[,1:5, with=FALSE]

## ------------------------------------------------------------------------
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

## ------------------------------------------------------------------------
experiments_info <- deepblue_info(deepblue_extract_ids(blueprint_DNA_meth))

## ------------------------------------------------------------------------
head(experiments_info[[1]], 10)

## ------------------------------------------------------------------------
biosource <- unlist(lapply(experiments_info, function(x){ x$sample_info$biosource_name}))
head(biosource)

## ------------------------------------------------------------------------
biosource <- str_replace_all(biosource, "-positive", "+")
biosource <- str_replace_all(biosource, "-negative", "-")

## ------------------------------------------------------------------------
biosource <- str_replace(biosource, ", terminally differentiated", "")

## ------------------------------------------------------------------------
color_map <- data.frame(biosource = unique(biosource),
                        color = getPalette(length(unique(biosource))))

head(color_map)

## ------------------------------------------------------------------------
exp_names <- unlist(lapply(experiments_info, function(x){ x$name}))

biosource_colors <- data.frame(name = exp_names, biosource = biosource)
biosource_colors <- dplyr::left_join(biosource_colors, color_map, by = "biosource")
head(biosource_colors)

## ------------------------------------------------------------------------
color_vector <- as.character(biosource_colors$color)
names(color_vector) <-  biosource_colors$biosource
head(color_vector)

## ------------------------------------------------------------------------
filtered_score_matrix <- as.matrix(score_matrix[,-c(1:3), with=FALSE])
head(filtered_score_matrix[,1:3])

## ------------------------------------------------------------------------
message("regions before: ", nrow(filtered_score_matrix))
filtered_score_matrix_rowVars <- rowVars(filtered_score_matrix, na.rm = T)
filtered_score_matrix <- filtered_score_matrix[which(filtered_score_matrix_rowVars > 0.05),]
message("regions after: ", nrow(filtered_score_matrix))

## ------------------------------------------------------------------------
message("regions before: ", nrow(filtered_score_matrix))
filtered_score_matrix <- filtered_score_matrix[which(complete.cases(filtered_score_matrix)),]
message("regions after: ", nrow(filtered_score_matrix))

## ------------------------------------------------------------------------
filtered_score_matrix <- filtered_score_matrix[,exp_names]

## ---- fig.width=11-------------------------------------------------------
 heatmap.2(filtered_score_matrix,labRow = NA, labCol = NA,
          trace = "none", ColSideColors = color_vector,
          hclust=function(x) hclust(x,method="complete"),
          distfun=function(x) as.dist(1-cor(t(x), method = "pearson")),
          Rowv = TRUE, dendrogram = "column",
          key.xlab = "beta value", denscol = "black", keysize = 1.5,
          key.title = NA)
 

## ---- fig.height=8-------------------------------------------------------
  plot.new()
  
  legend(x = 0, y = 1,
       legend = color_map$biosource,
       col = as.character(color_map$color),
       text.width = 0.6,
       lty= 1,
       lwd = 6,
       cex = 0.7,
       y.intersp = 0.7,
       x.intersp = 0.7,
       inset=c(-0.21,-0.11))
  

## ---- eval=FALSE---------------------------------------------------------
#  demo(package = "DeepBlueR")

## ---- eval=FALSE---------------------------------------------------------
#  demo("use_case1", package = "DeepBlueR")

