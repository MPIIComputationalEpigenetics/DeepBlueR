annotation_id = deepblue_select_annotations(
    annotation_name="CpG Islands",
    genome="hg19", chromosome="chr1")
deepblue_flank(query_id = annotation_id,
    start = 0, length = 2000,
    use_strand = TRUE)