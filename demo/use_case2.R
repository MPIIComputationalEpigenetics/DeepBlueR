# DeepBlue Epigenomic Data Server - R access library
# Use Case 2
# Author: Felipe Albrecht, Markus List

message("Extract all H3k27ac from BLUEPRINT Epigenome 
        project (for chromosome 1 only)")

message("We list and extract all samples IDs with the 
        biosource H1-hESC from the ENCODE project.")
readline("press ENTER to continue")

H1_hESC_samples = deepblue_list_samples("H1-hESC", list("source"="ENCODE"))
H1_hESC_samples_ids = deepblue_extract_ids(H1_hESC_samples)

message("We list all peaks experiments that contains the previously 
        selected samples IDS, the histone modification H3K4me3 
        from the ENCODE project.")
readline("press ENTER to continue")

experiments = deepblue_list_experiments(genome="hg19", 
                                        type="peaks", 
                                        epigenetic_mark="H3K4me3", 
                                        sample=H1_hESC_samples_ids, 
                                        project="ENCODE")

message("We extract the IDs from the listed experiments.")
message("Obtain information about the experiment using the ID")

readline("press ENTER to continue")

experiments_id = deepblue_extract_ids(experiments)
exps_infos = deepblue_info(experiments_id)

message("and generate a list of experiments that the original 
        file name ends with 'bed.gz'.")
readline("press ENTER to continue")


filter_original_bed_files = function (e) {
    grepl("bed.gz$", e$extra_metadata$original_file_url)
}
h1_hESC_H3K4me3_experiment_name = sapply(
    exps_infos[lapply(exps_infos, filter_original_bed_files) > 0], 
    function (e) { e$name; })

message("As we are interested in only one experiment file, we do a 
        check if found only one experiment.")
readline("press ENTER to continue")

if (length(h1_hESC_H3K4me3_experiment_name) != 1) {
    stop("It was expected only one h1_hESC_H3K4me3 experiment")
}

message("We select the regions of the selected h1-hESC H3K4me3 experiment.")
readline("press ENTER to continue")

h1_hESC_H3K4me3 = deepblue_select_experiments(experiment_name=
                                                  h1_hESC_H3K4me3_experiment_name)

message("Then, we list and extract the names of the DNA Methylation experiments 
        that contain liver or hepatocyte biosource.")
readline("press ENTER to continue")

liver_experiments = deepblue_list_experiments(
    genome="hg19", 
    epigenetic_mark="DNA Methylation", 
    biosource=c('liver', 'hepatocyte'))

liver_experiments_names = deepblue_extract_names(liver_experiments)

print(liver_experiments_names)

message("We iterate over each selected DNA Methylation experiment")
readline("press ENTER to continue")

requests = lapply(liver_experiments_names, function (liver_experiment) {
    print(paste("Sending request for processing", liver_experiment))
    
    
    # Selecting the regions of the experiment
    q_liver_data = deepblue_select_experiments(experiment_name=liver_experiment)
    # We aggregate the selected regions using the H1-hESC regions. 
    # We perform the aggregation on the column 'SCORE'.
    agg_id = deepblue_aggregate(data_id=q_liver_data, 
                                ranges_id=h1_hESC_H3K4me3, 
                                column='SCORE')
    # We filter and remove the aggregated regions 
    # that did not aggregate any region.
    q_filter = deepblue_filter_regions(query_id=agg_id, 
                                       field="@AGG.COUNT", 
                                       operation=">", 
                                       value="0", 
                                       type="number")
    
    # Finally, we request the regions with the desired columns.
    # We return the request ID that will be used to download the data.
    deepblue_get_regions(
        query_id=q_filter, 
        output_format="CHROMOSOME,START,END,@AGG.MEAN,@AGG.COUNT,@AGG.MAX,@AGG.MIN")
})

head(requests)

message("We use the auxiliar function batch_export_results 
        to download and store the request data in the target.directory")
readline("press ENTER to continue")
results <- deepblue_batch_export_results(requests, 
                                         target.directory="liver_aggregation")

message("Some of the requests failed. 
        You can access the problematic commands like this")

readline("press ENTER to continue")

attr(results, "errors")

message("This will allow you to chceck this errors individually, 
        for example with the first error")
readline("press ENTER to finish")

deepblue_info(unlist(attr(results, "errors")))
