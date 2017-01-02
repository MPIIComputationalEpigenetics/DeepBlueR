annotation_id = deepblue_select_annotations(
    annotation_name="CpG Islands",
    genome="hg19", chromosome="chr1")
experiment_id = deepblue_select_experiments(
    experiment_name="S00XDKH1.ERX712765.H3K27ac.bwa.GRCh38.20150527.bed")
deepblue_overlap(query_data_id = experiment_id, query_filter_id = annotation_id, 
    overlap = TRUE, amount=10, amount_type="%")

