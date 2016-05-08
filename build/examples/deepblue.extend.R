annotation_id = deepblue.select_annotations(annotation_name="CpG Islands",
	genome="hg19", chromosome="chr1")
deepblue.extend(query_id = annotation_id, length = 2000, direction = "BOTH",
  use_strand = TRUE)