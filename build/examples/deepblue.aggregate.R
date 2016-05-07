annotation_id = deepblue.select_annotations(annotation_name="CpG Islands",
	genome="hg19", chromosome="chr1")
data_id = deepblue.select_experiments(
	experiment_name="E002-H3K9ac.narrowPeak.bed")
deepblue.aggregate(data_id = data_id, ranges_id=annotation_id,
	column = "SCORE")