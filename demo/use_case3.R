# DeepBlue Epigenomic Data Server - R access library
# Use Case 3
# Author: Nadia Ashraf

# Obtain genes regions
gene_names = c('CCR1', 'CD164', 'CD1D', 'CD2', 'CD34', 'CD3G', 'CD44')

#select regions of selected genes
q_genes = deepblue.select_genes(gene_names, "gencode v23")

#Filter regions that are protein coding
q_genes_regions = deepblue.filter_regions(query_id = q_genes, field = "@GENE_ATTRIBUTE(gene_type)",
                                         operation = "==", value = "protein_coding", type = "string")

#Select all liver and hepatocyte biosources
biosources = c('liver', 'Hematopoietic', 'hematopoietic stem cell')
liver_biosource_names = c()
for (biosource in biosources)
{
  bio_sources = deepblue.get_biosource_related(biosource)
  liver_biosource_names =c(liver_biosource_names, deepblue.extract_names(bio_sources))
}

# Obtain the mRNA experiments names 

experiments = deepblue.list_experiments(genome = "GRCh38",type = "signal",epigenetic_mark = "mRNA", 
                                        biosource = liver_biosource_names,
                                        project = "BLUEPRINT Epigenome")
hsc_experiment_names = deepblue.extract_names(experiments)

#perform aggregation 

requests = lapply(hsc_experiment_names, function (hsc_experiment) {
  print(paste("Processing", hsc_experiment))
  length(hsc_experiment)
  
  q_exp = deepblue.select_experiments(experiment_name = hsc_experiment)
  q_agg = deepblue.aggregate(data_id = q_exp, ranges_id = q_genes_regions, column='VALUE')
  q_filtered = deepblue.filter_regions(query_id = q_agg, field="@AGG.MEAN", operation=">", 
                                       value="0", type="number")
  
  req = deepblue.get_regions(query_id = q_filtered, output_format = "CHROMOSOME,START,END,@AGG.MEAN,
                       @AGG.MAX,@AGG.MIN")
})
print (requests)

deepblue.batch_export_results(requests, target.directory="D:/hsc_aggregation", suffix="_result", prefix="DeepBlue")

while (length(requests) > 0)
{
  print (paste("It is still missing",as.character(length(requests))," requests"))  
  for (req in requests)
  {
  ss = deepblue.info(req)
  print (ss)
  print (ss[[1]]$state)
  if (ss[[1]]$state == "done")
  {
    print (ss)
  
    print (paste("getting data from ", ss[[1]]$`_id`))
    data = deepblue.get_request_data(req)
    write(data,paste("D:/data/",req,".bed"))
    deepblue.cancel_request(req)
    if (ss[[1]]$state == "failed")
    {
      print (ss)
      deepblue.cancel_request(req)
    }
  }
Sys.sleep(1.0)
  }
}
