h3k27ac_regions = deepblue_select_regions(
    genome ='GRCh38',
    epigenetic_mark ='H3k27ac',
    project ='BLUEPRINT Epigenome',
    chromosome ='chr1')
deepblue_query_experiment_type(
    query_id = h3k27ac_regions,
    type = "peaks")