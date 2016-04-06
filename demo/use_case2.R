H1_hESC_samples = deepblue.list_samples("H1-hESC", list("source"="ENCODE"))
H1_hESC_samples_ids = deepblue.extract_ids(H1_hESC_samples)
experiments = deepblue.list_experiments(genome="hg19", type="peaks", epigenetic_mark="H3K4me3", sample=H1_hESC_samples_ids, project="ENCODE")
experiments_id = deepblue.extract_ids(experiments)
exps_infos = deepblue.info(experiments_id)

filter_original_bed_files = function (e) {
	grepl("bed.gz$", e$extra_metadata$original_file_url)
}

# Filter the files that ends with "bed,gz" and return their name
h1_hESC_H3K4me3_experiment_name = sapply(exps_infos[lapply(exps_infos, filter_original_bed_files) > 0], function (e) { e$name; })

if (length(h1_hESC_H3K4me3_experiment_name) != 1) {
    stop("It was expected only one h1_hESC_H3K4me3 experiment")
}

h1_hESC_H3K4me3_exp = h1_hESC_H3K4me3_experiment_name[[1]]
h1_hESC_H3K4me3 = deepblue.select_experiments(experiment_name=h1_hESC_H3K4me3_exp)


## List the liver and hepatocyte experiments
liver_experiments = deepblue.list_experiments(genome="hg19", epigenetic_mark="DNA Methylation",  biosource=c('liver', 'hepatocyte'))
liver_experiments_names = deepblue.extract_names(liver_experiments)

print(liver_experiments_names)

requests = lapply(liver_experiments_names, function (liver_experiment) {
	print(paste("Processing", liver_experiment))
	length(liver_experiment)

	q_liver_data = deepblue.select_experiments(experiment_name=liver_experiment)
	agg_id = deepblue.aggregate(data_id=q_liver_data, ranges_id=h1_hESC_H3K4me3, column='SCORE')
	q_filter = deepblue.filter_regions(query_id=agg_id, field="@AGG.COUNT", operation=">", value="0", type="number")

	deepblue.get_regions(query_id=q_filter, output_format="CHROMOSOME,START,END,@AGG.MEAN,@AGG.COUNT,@AGG.MAX,@AGG.MIN")
})

print(requests)

deepblue.batch_export_results(requests, target.directory="liver_aggregation", suffix="_result", prefix="DeepBlue")
