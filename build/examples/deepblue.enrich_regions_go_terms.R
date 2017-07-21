data_id = deepblue_select_experiments(
    experiment_name="E002-H3K9ac.narrowPeak.bed")

filtered_id = deepblue_filter_regions(query_id = data_id,
    field = "VALUE",
    operation = ">",
    value = "100",
    type = "number",
    user_key = "anonymous_key")

deepblue_enrich_regions_go_terms(query_id = filtered_id,
   gene_model = "gencode v23")