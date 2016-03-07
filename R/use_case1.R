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
sel_regions = deepblue.select_regions('','GRCh38','H3k27ac','','','BLUEPRINT Epigenome',
                                      'chr1',NULL,NULL,deepblue.USER_KEY)

#Extract peaks for H3k27ac 
sel_regions_peaks=deepblue.query_experiment_type(as.character(sel_regions[2]),
                                                 'peaks',deepblue.USER_KEY)

#Extract annotated promoter regions from GRCh38 genome assembly

promoters = deepblue.select_annotations('promoters', 'GRCh38','chr1', NULL, NULL, 
                                        deepblue.USER_KEY)

#filter for promoter regions having H3k27ac modification associated with them

sel_promoters = deepblue.intersection(as.character(sel_regions_peaks[2]), 
                                      as.character(promoters[2]), deepblue.USER_KEY)

#Extract transcription factors AG01,AG02,and AG03 from ENCODE project

tf = deepblue.select_regions('','hg19', 'SP1', '', '', 'ENCODE', 'chr1', 
                                        NULL, NULL, deepblue.USER_KEY)

#Extract signal for transcription factors

sel_tf_signal = deepblue.query_experiment_type(as.character(tf[2]), 'signal',
                                              deepblue.USER_KEY)

#Extract TFs overlaping with sel_promoters

sel_tf = deepblue.intersection(as.character(tf[2]), as.character(sel_promoters[2]),
                               deepblue.USER_KEY)

#Get chromosome regions

req_regions = deepblue.get_regions(as.character(sel_tf[2]), 
                                   "CHROMOSOME,START,END,@NAME,@EPIGENETIC_MARK,@BIOSOURCE",
                                   deepblue.USER_KEY)

#Process request

info = deepblue.info(as.character(req_regions[2]), deepblue.USER_KEY)

while (info[2]['state'] != 'done' & info[2]['state'] != 'error')
{
  Sys.sleep(5)
  info = deepblue.info(as.character(req_regions[2]), deepblue.USER_KEY)
}

# get regions that contains the TFs that overlap with the H3K27ac and the promoters regions.

final_regions = deepblue.get_request_data(as.character(req_regions[2]), deepblue.USER_KEY)

