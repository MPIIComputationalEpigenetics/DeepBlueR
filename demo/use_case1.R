# DeepBlue Epigenomic Data Server - R access library
# Use Case 1
# Author: Markus List

#Extract all H3k27ac from BLUEPRINT Epigenome project (for chromosome 1 only)

#deepblue_select_regions takes 11 parameters as input, namely;
#eperiment_name,genome,epigentic_mark,sample_id,technique,project,
#chromosome,start,end, and user_key. All parameters have default
#NULL value which can be changed according to requirement.
#user_key is set to anonymous_key which
#can also be changed with your specific user key.
sel_regions = deepblue_select_regions(genome = 'GRCh38',
                                      epigenetic_mark = 'H3k27ac',
                                      project = 'BLUEPRINT Epigenome',
                                      chromosome = 'chr1')

readline("press ENTER key to continue")

#Extract peaks for H3k27ac

#deepblue_query_experiment_type takes 3 input parameters, namely;
#query_id, type, and user_key.
#user_key is set to anonymous_key by default
sel_regions_peaks = deepblue_query_experiment_type(query_id = sel_regions,
                                                   type = 'peaks')

readline("press ENTER key to continue")

#Extract annotated promoter regions from GRCh38 genome assembly

#deepblue_select_annotations takes 6 input parameters, namely;
#annotation_name, genome, chromosome, start, end, and user_key.
#All values are set to NULL as default.
promoters = deepblue_select_annotations(annotation_name = 'promoters',
                                        genome = 'GRCh38',
                                        chromosome = 'chr1')

readline("press ENTER key to continue")

#filter for promoter regions having H3k27ac modification associated with them


#deepblue_intersection takes 3 parameters as input, namely;
#query_data_id, query_filter_id, and user_key.
#user_key is set to anonymous_key which can also be
#changed with your specific user key.
sel_promoters = deepblue_intersection(query_data_id = sel_regions_peaks,
                                      query_filter_id = promoters)

readline("press ENTER key to continue")

#Extract transcription factors AG01,AG02,and AG03 from ENCODE project
tf = deepblue_select_regions(genome = 'hg19', epigenetic_mark = 'SP1',
                             project = 'ENCODE', chromosome = 'chr1')

readline("press ENTER key to continue")

#Extract signal for transcription factors
sel_tf_signal = deepblue_query_experiment_type(query_id = tf, type = 'signal')

readline("press ENTER key to continue")

#Extract TFs overlaping with sel_promoters

sel_tf = deepblue_intersection(query_data_id = tf,
                               query_filter_id = sel_promoters)

readline("press ENTER key to continue")

#Get chromosome regions

#deepblue_get_regions expects 3 input parameters;
#query_id, output_format, and user_key."
req_regions = deepblue_get_regions(
    query_id = sel_tf,
    output_format = "CHROMOSOME,START,END,@NAME,@EPIGENETIC_MARK,@BIOSOURCE"
)

readline("press ENTER key to continue")

#get regions that contains the TFs that overlap with the H3K27ac
#and the promoters regions.
requested_data = deepblue_download_request_data(req_regions)

#print results
requested_data
