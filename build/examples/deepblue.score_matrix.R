tiling_regions = deepblue_tiling_regions(
    size=100000, genome="mm10", chromosome="chr1")
deepblue_score_matrix(
    experiments_columns =
        list(ENCFF721EKA="VALUE", ENCFF781VVH="VALUE"),
  aggregation_function = "mean",
  aggregation_regions_id = tiling_regions)
