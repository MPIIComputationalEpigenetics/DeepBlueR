## ---- eval = FALSE, echo=TRUE, warning=FALSE, message=FALSE--------------
#  library(devtools)
#  install_github("MPIIComputationalEpigenetics/DeepBlueR")

## ---- echo=FALSE, warning=FALSE, message=FALSE, error=FALSE--------------
library(DeepBlueR)

## ---- echo=TRUE, eval = FALSE, warning=FALSE, message=FALSE--------------
#  deepblue_info("me")

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
# We are selecting the experiments with terms 'H3k27AC', 'blood', and
# 'peak' in the metadata.
experiments_found = deepblue_search(
    keyword="'H3k27AC' 'blood' 'peak'", type="experiments")

custom_table = do.call("rbind", lapply(experiments_found, function(experiment){
  experiment_id = experiment[[1]]
  # Obtain the information about the experiment_id
  info = deepblue_info(experiment_id)
  experiment_info = info[[1]]
  # Print the experiment name, project, biosource, and epigenetic mark.
  with(experiment_info, { data.frame(name = name, project = project,
    biosource = sample_info$biosource_name, epigenetic_mark = epigenetic_mark)
      })
}))
  head(custom_table)

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
experiments = deepblue_list_experiments(type="peaks", epigenetic_mark="H3K4me3",
    biosource=c("inflammatory macrophage", "macrophage"),
    project="BLUEPRINT Epigenome")
do.call("rbind", lapply(experiments,
    function(x){ data.frame(id = x[[1]], experiment_name = x[[2]])}))

## ---- echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----------------
info = deepblue_info("e30000")
print(info[[1]]$extra_metadata$file_url)

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

