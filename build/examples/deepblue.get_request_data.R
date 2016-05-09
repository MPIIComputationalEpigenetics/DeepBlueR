data_id = deepblue.select_experiments(
	experiment_name="E002-H3K9ac.narrowPeak.bed", chromosome="chr1")
request_id = deepblue.get_regions(query_id =data_id, output_format = "CHROMOSOME,START,END")
deepblue.get_request_data(request_id = request_id)