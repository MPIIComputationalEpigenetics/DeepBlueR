data_id = deepblue_select_experiments(
    experiment_name="E002-H3K9ac.narrowPeak.bed")
deepblue_get_regions(query_id =data_id,
    output_format = "CHROMOSOME,START,END")