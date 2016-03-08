##############################
"
      DeepBlue Project
         Use Case 1
"
##############################

source("deepblue.R")




"result of functions is a 2 elements list. Extract 2nd element convert it to string,
since it's a struct type object, and then use it to query the server for extracting
more details."

#Extract all H3k27ac from BLUEPRINT Epigenome project (for chromosome 1 only)
sel_regions = deepblue.select_regions(genome ='GRCh38',epigenetic_mark ='H3k27ac',
                                      project ='BLUEPRINT Epigenome',chromosome ='chr1')

#Extract peaks for H3k27ac 
sel_regions_peaks=deepblue.query_experiment_type(query_id =as.character(sel_regions[2]),
                                                 type ='peaks')

#Extract annotated promoter regions from GRCh38 genome assembly

promoters = deepblue.select_annotations(annotation_name ='promoters',genome ='GRCh38',
                                        chromosome ='chr1')

#filter for promoter regions having H3k27ac modification associated with them

sel_promoters = deepblue.intersection(query_a_id =as.character(sel_regions_peaks[2]), 
                                      query_b_id =as.character(promoters[2]))

#Extract transcription factors AG01,AG02,and AG03 from ENCODE project

tf = deepblue.select_regions(genome ='hg19',epigenetic_mark ='SP1',
                             project ='ENCODE',chromosome ='chr1')

#Extract signal for transcription factors

sel_tf_signal = deepblue.query_experiment_type(query_id =as.character(tf[2]),type ='signal')

#Extract TFs overlaping with sel_promoters

sel_tf = deepblue.intersection(query_a_id =as.character(tf[2]),query_b_id =
                              as.character(sel_promoters[2]))

#Get chromosome regions

req_regions = deepblue.get_regions(query_id =as.character(sel_tf[2]),output_format =
                                   "CHROMOSOME,START,END,@NAME,@EPIGENETIC_MARK,@BIOSOURCE")

#Process request

process.request(requested_regions =req_regions) # with sleep time default 1s. It can be
                                              # by passing sleep time as 2nd argument.

# get regions that contains the TFs that overlap with the H3K27ac and the promoters regions.

final_regions = deepblue.get_request_data(request_id =as.character(req_regions[2]))

# storing output in data frame
final = unlist(strsplit(as.character(final_regions[2]),'\n'))
final = as.data.frame(final, stringsAsFactors=FALSE)
regions = data.frame(do.call('rbind', strsplit(as.character(final$final),'\t',fixed=TRUE)),stringsAsFactors=FALSE)
colnames(regions) = c('chromosome','start','end','name','epigenetic_mark','biosource')


