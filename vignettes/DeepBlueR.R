## ---- eval = FALSE, echo=TRUE, warning=FALSE, message=FALSE--------------
#  devtools::install_github("MPIIComputationalEpigenetics/DeepBlueR",
#      build_vignettes=TRUE)

## ---- echo=FALSE, warning=FALSE, message=FALSE, error=FALSE--------------
library(DeepBlueR)

## ---- echo=TRUE, eval = FALSE, warning=FALSE, message=FALSE--------------
#  deepblue.info("me")

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
# We are selecting the experiments with terms 'H3k27AC', 'blood', and
# 'peak' in the metadata.
experiments_found = deepblue.search(
    keyword="'H3k27AC' 'blood' 'peak'", type="experiments")

custom_table = do.call("rbind", lapply(experiments_found, function(experiment){
  experiment_id = experiment[[1]]
  # Obtain the information about the experiment_id
  info = deepblue.info(experiment_id)
  experiment_info = info[[1]]
  # Print the experiment name, project, biosource, and epigenetic mark.
  with(experiment_info, { data.frame(name = name, project = project,
    biosource = sample_info$biosource_name, epigenetic_mark = epigenetic_mark) 
      })
}))
  head(custom_table)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
experiments = deepblue.list_experiments(type="peaks", epigenetic_mark="H3K4me3",
    biosource=c("inflammatory macrophage", "macrophage"),
    project="BLUEPRINT Epigenome")
do.call("rbind", lapply(experiments,
    function(x){ data.frame(id = x[[1]], experiment_name = x[[2]])}))

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
info = deepblue.info("e30000")
print(info[[1]]$extra_metadata$file_url)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue.select_experiments(
    experiment_name=c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed",
        "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"))
# Count how many regions where selected
request_id = deepblue.count_regions(query_id=query_id)
# Download the request data as soon as processing is finished
requested_data = deepblue.download_request_data(request_id=request_id)
print(paste("The selected experiments have", requested_data, "regions."))

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue.select_experiments (
    experiment_name = c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed",
        "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"),
        chromosome="chr1", start=0, end=50000000)

# Retrieve the experiments data (The @NAME meta-column is used to include the
# experiment name and @BIOSOURCE for experiment's biosource
request_id = deepblue.get_regions(query_id=query_id,
    output_format="CHROMOSOME,START,END,SIGNAL_VALUE,PEAK,@NAME,@BIOSOURCE")
regions = deepblue.download_request_data(request_id=request_id)
regions

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
samples = deepblue.list_samples(
    biosource="myeloid cell",
    extra_metadata = list("source" = "BLUEPRINT Epigenome"))
samples_ids = deepblue.extract_ids(samples)
query_id = deepblue.select_regions(genome="GRCh38", sample=samples_ids,
    chromosome="chr1", start=0, end=50000)
request_id = deepblue.get_regions(query_id=query_id,
    output_format="CHROMOSOME,START,END,@NAME,@SAMPLE_ID,@BIOSOURCE")
regions = deepblue.download_request_data(request_id=request_id)
head(regions,1)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue.select_experiments(
    experiment_name = c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed",
        "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"),
    chromosome="chr1", start=0, end=50000000)
query_id_filter_signal = deepblue.filter_regions(
    query_id=query_id, field="SIGNAL_VALUE", operation=">",
    value="10", type="number")
query_id_filters = deepblue.filter_regions(
    query_id=query_id_filter_signal, field="PEAK", operation=">",
    value="1000", type="number")
request_id = deepblue.get_regions(query_id=query_id_filters,
    output_format="CHROMOSOME,START,END,SIGNAL_VALUE,PEAK,@NAME,@BIOSOURCE")
regions = deepblue.download_request_data(request_id=request_id)
regions

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
query_id = deepblue.select_experiments(
    experiment_name = c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed",
        "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"),
    chromosome="chr1", start=0, end=50000000)
promoters_id = deepblue.select_annotations(annotation_name="promoters",
    genome="GRCh38", chromosome="chr1")
intersect_id = deepblue.intersection(
    query_a_id=query_id, query_b_id=promoters_id)
request_id = deepblue.get_regions(
    query_id=intersect_id,
    output_format="CHROMOSOME,START,END,SIGNAL_VALUE,PEAK,@NAME,@BIOSOURCE")
regions = deepblue.download_request_data(request_id=request_id)
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
query_id = deepblue.select_experiments(
    experiment_name = c("BL-2_c01.ERX297416.H3K27ac.bwa.GRCh38.20150527.bed",
        "S008SGH1.ERX406923.H3K27ac.bwa.GRCh38.20150728.bed"),
    chromosome="chr1", start=0, end=50000000)
query_id_filter_signal = deepblue.filter_regions(query_id=query_id,
    field="SIGNAL_VALUE", operation=">", value="10", type="number")
query_id_filters = deepblue.filter_regions(query_id=query_id_filter_signal,
    field="PEAK", operation=">", value="1000", type="number")
query_id_filter_length = deepblue.filter_regions (query_id=query_id_filters,
    field="@LENGTH", operation="<", value="2000", type="number")
request_id = deepblue.get_regions(query_id=query_id_filter_length,
    output_format="CHROMOSOME,START,END,@NAME,@BIOSOURCE,@LENGTH,@SEQUENCE")
regions = deepblue.download_request_data(request_id=request_id)
head(regions, 1)

