query_id = deepblue_select_experiments(
  experiment_name="S00VEQA1.hypo_meth.bs_call.GRCh38.20150707.bed")

filtered_query_id = deepblue_filter_regions(
  query_id = query_id,
  field = "AVG_METHYL_LEVEL",
  operation = "<",
  value = "0.0025",
  type="number")

rg_10kb_tilling = deepblue_tiling_regions(
    size = 1000,
    genome = "hg19")

# We could have included more Epigenetic Marks here
epigenetic_marks <- c("h3k27ac", "H3K27me3", "H3K4me3")

histones_datasets = c()
for (i in 1:length(epigenetic_marks)) {
  experiments_list <- deepblue_list_experiments(
    epigenetic_mark=epigenetic_marks[[i]],
    type="peaks",
    genome="grch38",
    project="BLUEPRINT Epigenome");

    experiment_names = deepblue_extract_names(experiments_list)
    histones_datasets[[epigenetic_marks[[i]]]] = experiment_names
}

deepblue_enrich_region_overlap(
  query_id=filtered_query_id,
  background_query=rg_10kb_tilling,
  datasets=histones_datasets,
  genome="grch38")
