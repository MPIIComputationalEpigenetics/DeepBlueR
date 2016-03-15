

#      DeepBlue Project
#       Use Case 1



source("deepblue.R")




#result of functions is a 2 elements list. Extract 2nd element convert it to string,
#since it's a struct type object, and then use it to query the server for extracting
#more details.

#Extract all H3k27ac from BLUEPRINT Epigenome project (for chromosome 1 only)


#deepblue.select_regions takes 11 parameters as input, namely; eperiment_name,genome,epigentic_
#mark,sample_id,technique,project,chromosome,start,end, and user_key. All parameters have default
#NULL value which can be changed according to requirement. user_id is set to anonymous_key which 
#can also be changed with your specific user key.


sel_regions = deepblue.select_regions(genome ='GRCh38',epigenetic_mark ='H3k27ac',
                                      project ='BLUEPRINT Epigenome',chromosome ='chr1')

#Extract peaks for H3k27ac 


#deepblue.query_experiment_type takes 3 input parameters, namely; query_id, type, and user_key.
#All values are set to NULL as default.


val.regions = get.value(sel_regions)
sel_regions_peaks=deepblue.query_experiment_type(query_id =val.regions,
                                                 type ='peaks')

#Extract annotated promoter regions from GRCh38 genome assembly


#deepblue.select_annotations takes 6 input parameters, namely; annotation_name, genome, chromosome,
#start, end, and user_key. All values are set to NULL as default.


promoters = deepblue.select_annotations(annotation_name ='promoters',genome ='GRCh38',
                                        chromosome ='chr1')

#filter for promoter regions having H3k27ac modification associated with them


#deepblue.intersection takes 3 parameters as input, namely; query_a_id, query_b_id, and user_key.
#All parameters are set to NULL as default.


val.peaks = get.value(sel_regions_peaks)
val.prom = get.value(promoters)
sel_promoters = deepblue.intersection(query_a_id = val.peaks, 
                                      query_b_id = val.prom)

#Extract transcription factors AG01,AG02,and AG03 from ENCODE project

tf = deepblue.select_regions(genome ='hg19',epigenetic_mark ='SP1',
                             project ='ENCODE',chromosome ='chr1')

#Extract signal for transcription factors

qid.tf =  get.value(tf)
sel_tf_signal = deepblue.query_experiment_type(query_id =qid.tf,type ='signal')

#Extract TFs overlaping with sel_promoters

qid.prom = get.value(sel_promoters)
sel_tf = deepblue.intersection(query_a_id =qid.tf,query_b_id =qid.prom)

#Get chromosome regions


#deepblue.get_regions expects 3 input parameters; query_id, output_format, and user_key."

id.sel.tf = get.value(sel_tf)
req_regions = deepblue.get_regions(query_id = id.sel.tf,output_format =
                                     "CHROMOSOME,START,END,@NAME,@EPIGENETIC_MARK,@BIOSOURCE")


#Process request

id.req.regions = get.value(req_regions)
info = deepblue.info(id.req.regions)


#process.request expects 3 input parameters; requested_regions, sleep.time, user_key. sleep.time
#is set to 1s as default sleeping time.

process.request(requested_regions = req_regions) 

#get regions that contains the TFs that overlap with the H3K27ac and the promoters regions.
#Output is a granges object.

requested.data = get_request_data(req.id = id.req.regions, data.info = info)

