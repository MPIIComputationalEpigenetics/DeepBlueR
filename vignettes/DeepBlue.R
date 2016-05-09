## ---- eval = FALSE, echo=TRUE, warning=FALSE, message=FALSE--------------
#  devtools::install_github("MPIIComputationalEpigenetics/DeepBlue-R", build_vignettes=TRUE)

## ---- echo=FALSE, warning=FALSE, message=FALSE, error=FALSE--------------
library(DeepBlue)

## ---- echo=TRUE, eval = FALSE, warning=FALSE, message=FALSE--------------
#  deepblue.info("me")

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
# We are selecting the experiments with terms 'H3k27AC', 'blood', and 'peak' in the metadata.
experiments_found = deepblue.search(keyword="'H3k27AC' 'blood' 'peak'", type="experiments")

custom_table = do.call("rbind", lapply(experiments_found, function(experiment){
  experiment_id = experiment[[1]]
  # Obtain the information about the experiment_id
  info = deepblue.info(experiment_id)
  experiment_info = info[[1]]
  # Print the experiment name, project, biosource, and epigenetic mark.
  with(experiment_info, { data.frame(name = name, project = project, biosource = sample_info$biosource_name, epigenetic_mark = epigenetic_mark) })
}))
  head(custom_table)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
experiments = deepblue.list_experiments(type="peaks", epigenetic_mark="H3K4me3", biosource=c("inflammatory macrophage", "macrophage"), project="BLUEPRINT Epigenome")
do.call("rbind", lapply(experiments, function(x){ data.frame(id = x[[1]], experiment_name = x[[2]])}))

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
info = deepblue.info("e30000")
print(info[[1]]$extra_metadata$file_url)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue.select_experiments(experiment_name=c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed", "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"))
# Count how many regions where selected
request_id = deepblue.count_regions(query_id=query_id)
# Download the request data as soon as processing is finished
requested_data = deepblue.download_request_data(request_id=request_id)
print(paste("The selected experiments have", requested_data, "regions."))

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue.select_experiments (experiment_name = c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed", "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"), chromosome="chr1", start=0, end=50000000)

# Retrieve the experiments data (The @NAME meta-column is used to include the experiment name and @BIOSOURCE for experiment's biosource
request_id = deepblue.get_regions(query_id=query_id, output_format="CHROMOSOME,START,END,SIGNAL_VALUE,PEAK,@NAME,@BIOSOURCE")
regions = deepblue.download_request_data(request_id=request_id)
regions

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
samples = deepblue.list_samples(biosource="myeloid cell", extra_metadata = list("source" = "BLUEPRINT Epigenome"))
samples_ids = deepblue.extract_ids(samples)
query_id = deepblue.select_regions(genome="GRCh38", sample=samples_ids, chromosome="chr1", start=0, end=50000)
request_id = deepblue.get_regions(query_id=query_id, output_format="CHROMOSOME,START,END,@NAME,@SAMPLE_ID,@BIOSOURCE")
regions = deepblue.download_request_data(request_id=request_id)
head(regions,1)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue.select_experiments(experiment_name = c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed", "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"), chromosome="chr1", start=0, end=50000000)
query_id_filter_signal = deepblue.filter_regions (query_id=query_id, field="SIGNAL_VALUE", operation=">", value="10", type="number")
query_id_filters = deepblue.filter_regions (query_id=query_id_filter_signal, field="PEAK", operation=">", value="1000", type="number")
request_id = deepblue.get_regions(query_id=query_id_filters, output_format="CHROMOSOME,START,END,SIGNAL_VALUE,PEAK,@NAME,@BIOSOURCE")
regions = deepblue.download_request_data(request_id=request_id)
regions

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue.select_experiments(experiment_name = c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed", "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"), chromosome="chr1", start=0, end=50000000)
promoters_id = deepblue.select_annotations(annotation_name="promoters", genome="GRCh38", chromosome="chr1")
intersect_id = deepblue.intersection(query_a_id=query_id, query_b_id=promoters_id)
request_id = deepblue.get_regions(query_id=intersect_id, output_format="CHROMOSOME,START,END,SIGNAL_VALUE,PEAK,@NAME,@BIOSOURCE")
regions = deepblue.download_request_data(request_id=request_id)
regions

## ---- message=FALSE, error=FALSE, warning=FALSE--------------------------
library(Gviz)
atrack <- AnnotationTrack(regions, name = "Overlapping regions", group = regions$`@BIOSOURCE`, genome="hg38")
gtrack <- GenomeAxisTrack()
itrack <- IdeogramTrack(genome = "hg38", chromosome = "chr1")
plotTracks(list(itrack, atrack, gtrack), groupAnnotation="group", fontsize=18,
           background.panel = "#FFFEDB", background.title = "darkblue")

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue.select_experiments(experiment_name = c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed", "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"), chromosome="chr1", start=0, end=50000000)
query_id_filter_signal = deepblue.filter_regions (query_id=query_id, field="SIGNAL_VALUE", operation=">", value="10", type="number")
query_id_filters = deepblue.filter_regions (query_id=query_id_filter_signal, field="PEAK", operation=">", value="1000", type="number")
query_id_filter_length = deepblue.filter_regions (query_id=query_id_filters, field="@LENGTH", operation="<", value="2000", type="number")
request_id = deepblue.get_regions(query_id=query_id_filter_length, output_format="CHROMOSOME,START,END,SIGNAL_VALUE,PEAK,@NAME,@BIOSOURCE,@LENGTH,@SEQUENCE")
regions = deepblue.download_request_data(request_id=request_id)
head(regions, 1)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
tataa_regions = deepblue.select_annotations(annotation_name="Pattern TATAAA (non-overlap) in the genome GRCh38", genome="GRCh38", chromosome="chr1")
query_id = deepblue.select_experiments (experiment_name= c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed", "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"), chromosome="chr1", start=0, end=50000000)
overlapped = deepblue.intersection(query_a_id=query_id, query_b_id=tataa_regions)
request_id=deepblue.get_regions(overlapped, "CHROMOSOME,START,END,SIGNAL_VALUE,PEAK,@NAME,@BIOSOURCE,@LENGTH,@SEQUENCE")
regions = deepblue.download_request_data(request_id=request_id)
head(regions, 5)

## ---- echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE----------------
#  q_genes = deepblue.select_genes(genes_name="RP11-34P13", gene_set="gencode v23")
#  q_filter = deepblue.filter_regions(query_id=q_genes, field="@GENE_ATTRIBUTE(gene_type)", operation="==", value="lincRNA", type="string")
#  request_id=deepblue.get_regions(q_filter, "CHROMOSOME,START,END,GTF_ATTRIBUTES")
#  regions = deepblue.download_request_data(request_id=request_id)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue.select_experiments (experiment=c("GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20150707.wig"), chromosome="chr1", start=0, end=50000000)
cpg_islands = deepblue.select_annotations(annotation_name="CpG Islands", genome="GRCh38", chromosome="chr1", start=0, end=50000000)
# Aggregate
overlapped = deepblue.aggregate (data_id=query_id, ranges_id=cpg_islands, column="VALUE" )

# Retrieve the experiments data (The @NAME meta-column is used to include the experiment name and @BIOSOURCE for experiment's biosource
request_id = deepblue.get_regions(query_id=overlapped, output_format="CHROMOSOME,START,END,@AGG.MIN,@AGG.MAX,@AGG.MEDIAN,@AGG.MEAN,@AGG.VAR,@AGG.SD,@AGG.COUNT")
regions = deepblue.download_request_data(request_id=request_id)
regions

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
# Selecting the data from 2 experiments: GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20150707.wig
# As we already know the experiments names, we keep all others fields empty.
# We are selecting all regions of chromosome 1
query_id = deepblue.select_experiments (experiment=c("GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20150707.wig"), chromosome="chr1")

#  Tiling regions of 100.000 base pairs
tiling_id = deepblue.tiling_regions(size=100000, genome="GRCh38", chromosome="chr1")

# Aggregate
overlapped = deepblue.aggregate (data_id=query_id, ranges_id=tiling_id, column="VALUE")

# Retrieve the experiments data (The @NAME meta-column is used to include the experiment name and @BIOSOURCE for experiment's biosource
request_id = deepblue.get_regions(query_id=overlapped, output_format="CHROMOSOME,START,END,@AGG.MIN,@AGG.MAX,@AGG.MEDIAN,@AGG.MEAN,@AGG.VAR,@AGG.SD,@AGG.COUNT")

regions = deepblue.download_request_data(request_id=request_id)
regions

## ------------------------------------------------------------------------
library(ggplot2)
plot_data <- as.data.frame(regions)
plot_data[,grepl("X.", colnames(plot_data))] <- apply(plot_data[,grepl("X.", colnames(plot_data))], 2, as.numeric)
AGG.plot <- ggplot(plot_data, aes(start)) +
    geom_ribbon(aes(ymin = X.AGG.MEAN - (X.AGG.SD / 2), ymax = X.AGG.MEAN + (X.AGG.SD / 2)), fill = "grey70") +
    geom_line(aes(y = X.AGG.MEAN))
print(AGG.plot)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
# Select the RP11-34P13 gene locations from gencode v23
q_genes = deepblue.select_genes(genes_name=c("RNU6-1100P", "CICP7", "MRPL20", "ANKRD65", "HES2", "ACOT7", "HES3", "ICMT"), gene_set="gencode v19")

# Obtain the regions that starts 2500 bases pair before the regions start and have 2000 base pairs.
# The 4th argument inform that DeepBlue must consider the region strand (column STRAND) to calculate the new region
before_flank_id = deepblue.flank(query_id=q_genes, start=-2500, length=2000, use_strand=TRUE)

# Obtain the regions that starts 1500 bases pair after the regions end and have 500 base pairs.
# The 4th argument inform that DeepBlue must consider the region strand (column STRAND) to calculate the new region
after_flank_id = deepblue.flank(query_id=q_genes, start=1500, length=500, use_strand=TRUE)

# Merge both flanking regions set and genes set
flank_merge_id = deepblue.merge_queries(query_a_id =before_flank_id, query_b_id=after_flank_id)
all_merge_id = deepblue.merge_queries(query_a_id=q_genes, query_b_id=flank_merge_id)

# Request the regions
request_id = deepblue.get_regions(query_id=all_merge_id, output_format="CHROMOSOME,START,END,STRAND,@LENGTH")

regions = deepblue.download_request_data(request_id=request_id)
regions

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
# Select the RP11-34P13 gene locations from gencode v23

# Selecting the data from 2 experiments: GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20150707.wig
# As we already know the experiments names, we keep all others fields empty.
# We are selecting all regions of chromosome 1
query_id = deepblue.select_experiments (experiment="GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20150707.wig", chromosome="chr1")

# Select the CpG Islands annotation from GRCh38
cpg_islands = deepblue.select_annotations(annotation="CpG Islands", genome="GRCh38", chromosome="chr1")

# Aggregate
overlapped = deepblue.aggregate (data_id=query_id, ranges_id=cpg_islands, column="VALUE")

# Select the aggregated regions that aggregated at least one region from the selected experiments (@AGG.COUNT > 0)
filtered = deepblue.filter_regions(query_id=overlapped, field="@AGG.COUNT", operation=">", value="0", type="number")

# We remove all regions where the aggregation mean is zero.
filtered_zeros = deepblue.filter_regions(query_id=filtered, field="@AGG.MEAN", operation="!=", value="0.0", type="number")

# Retrieve the experiments data (The @NAME meta-column is used to include the experiment name and @BIOSOURCE for experiment's biosource
request_id = deepblue.get_regions(query_id=filtered_zeros, output_format="CHROMOSOME,START,END,@CALCULATED(return math.log(value_of('@AGG.MEAN'))),@AGG.MEAN,@AGG.COUNT")

regions = deepblue.download_request_data(request_id=request_id)

# We have to perform a manual conversion because the package can't know the type for calculated columns
regions$`@CALCULATED(return math.log(value_of('@AGG.MEAN')))` =  as.numeric(regions$`@CALCULATED(return math.log(value_of('@AGG.MEAN')))`)

head(regions, 5)

## ---- warning=FALSE, error=FALSE-----------------------------------------
library(Gviz)
atrack <- AnnotationTrack(regions, name = "CpGs", group = regions$`@BIOSOURCE`, genome="hg38")
gtrack <- GenomeAxisTrack()
itrack <- IdeogramTrack(genome = "hg38", chromosome = "chr1")
dtrack <- DataTrack(regions, data="@AGG.MEAN", name = "Log of average methylation value")
plotTracks(list(itrack, atrack, dtrack, gtrack), type="histogram", fontsize=18,
           background.panel = "#FFFEDB", background.title = "darkblue")

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
experiments = c("GC_T14_10.CPG_methylation_calls.bs_call.GRCh38.20150707.wig", "C003N351.CPG_methylation_calls.bs_call.GRCh38.20150707.wig", "C005VG51.CPG_methylation_calls.bs_call.GRCh38.20150707.wig", "S002R551.CPG_methylation_calls.bs_call.GRCh38.20150707.wig", "NBC_NC11_41.CPG_methylation_calls.bs_call.GRCh38.20150707.wig", "bmPCs-V156.CPG_methylation_calls.bs_call.GRCh38.20150707.wig", "S00BS451.CPG_methylation_calls.bs_call.GRCh38.20150707.wig", "S00D1DA1.CPG_methylation_calls.bs_call.GRCh38.20150707.wig", "S00D39A1.CPG_methylation_calls.bs_call.GRCh38.20150707.wig")

experiments_columns = list()
for (experiment_name in experiments) {
    experiments_columns[[experiment_name]] = "VALUE"
}

cpgs = deepblue.select_annotations(annotation_name="Cpg Islands", chromosome="chr22", start=0, end=18000000, genome="hg19")

request_id = deepblue.score_matrix(experiments_columns=experiments_columns, aggregation_function="mean", aggregation_regions_id=cpgs)

score_matrix = deepblue.download_request_data(request_id=request_id)
head(score_matrix, 5)

## ---- warning=FALSE, echo=TRUE, error=FALSE------------------------------
library(ggplot2)
score_matrix = tidyr::gather(score_matrix, "experiment", "methylation", -CHROMOSOME, -START, -END)
score_matrix$START <- as.factor(score_matrix$START)
ggplot(score_matrix, aes(x=START, y=experiment, fill=methylation)) +
    geom_tile() +
    theme(axis.text.x=element_text(angle=-90))

