h3k27ac_regions = deepblue.select_regions(genome ='GRCh38',
                                      epigenetic_mark ='H3k27ac',
                                      project ='BLUEPRINT Epigenome',
                                      chromosome ='chr1')
deepblue.query_experiment_type(query_id = h3k27ac_regions, type = "peaks")