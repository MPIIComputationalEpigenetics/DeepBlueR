# DeepBlue Epigenomic Data Server - R access library
# Use Case 1
# Author: Nadia Ashraf

source("deepblue.R")
source("helpingFunctions.R")


#result of functions is a 2 elements list. Extract 2nd element convert it to string,
#since it's a struct type object, and then use it to query the server for extracting
#more details.

#Extract all H3k27ac from BLUEPRINT Epigenome project (for chromosome 1 only)


#deepblue.select_regions takes 11 parameters as input, namely; eperiment_name,genome,epigentic_
#mark,sample_id,technique,project,chromosome,start,end, and user_key. All parameters have default
#NULL value which can be changed according to requirement. user_key is set to anonymous_key which
#can also be changed with your specific user key.


sel_regions = deepblue.select_regions(genome ='GRCh38',epigenetic_mark ='H3k27ac',
                                      project ='BLUEPRINT Epigenome',chromosome ='chr1')[[2]]

#Extract peaks for H3k27ac


#deepblue.query_experiment_type takes 3 input parameters, namely; query_id, type, and user_key.
#user_key is set to anonymous_key by default

sel_regions_peaks=deepblue.query_experiment_type(query_id =sel_regions,
                                                 type ='peaks')[[2]]

#Extract annotated promoter regions from GRCh38 genome assembly


#deepblue.select_annotations takes 6 input parameters, namely; annotation_name, genome, chromosome,
#start, end, and user_key. All values are set to NULL as default.


promoters = deepblue.select_annotations(annotation_name ='promoters',genome ='GRCh38',
                                        chromosome ='chr1')[[2]]

#filter for promoter regions having H3k27ac modification associated with them


#deepblue.intersection takes 3 parameters as input, namely; query_a_id, query_b_id, and user_key.
#user_key is set to anonymous_key which can also be changed with your specific user key.



sel_promoters = deepblue.intersection(query_a_id = sel_regions_peaks,
                                      query_b_id = promoters)[[2]]

#Extract transcription factors AG01,AG02,and AG03 from ENCODE project

tf = deepblue.select_regions(genome ='hg19',epigenetic_mark ='SP1',
                             project ='ENCODE',chromosome ='chr1')[[2]]

#Extract signal for transcription factors


sel_tf_signal = deepblue.query_experiment_type(query_id =tf,type ='signal')[[2]]

#Extract TFs overlaping with sel_promoters


sel_tf = deepblue.intersection(query_a_id =tf,query_b_id =sel_promoters)[[2]]

#Get chromosome regions


#deepblue.get_regions expects 3 input parameters; query_id, output_format, and user_key."


req_regions = deepblue.get_regions(query_id =sel_tf,output_format =
                                       "CHROMOSOME,START,END,@NAME,@EPIGENETIC_MARK,@BIOSOURCE")[[2]]


#Process request

info = deepblue.info(req_regions)


#process.request expects 3 input parameters; requested_regions, sleep.time, user_key. sleep.time
#is set to 1s as default sleeping time.

request_info = process_request(requested_regions = req_regions)
if (request_info$value$state == "done") {
    print("The processing was finished")
} else {
    stop(request_info$value$message)
}

#get regions that contains the TFs that overlap with the H3K27ac and the promoters regions.
#Output is a granges object.
requested_data = get_request_data(request_info=info)

