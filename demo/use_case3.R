# DeepBlue Epigenomic Data Server - R access library
# Use Case 3
# Author: Nadia Ashraf

# Obtain genes regions
gene_names = c('CCR1', 'CD164', 'CD1D', 'CD2', 'CD34', 'CD3G', 'CD44')

#select regions of selected genes
q_genes = deepblue_select_genes(gene_names, "gencode v23")

#Filter regions that are protein coding
q_genes_regions = deepblue_filter_regions(
    query_id = q_genes, 
    field = "@GENE_ATTRIBUTE(gene_type)",
    operation = "==", value = "protein_coding", type = "string")

#Select all liver and hepatocyte biosources
biosources = c('liver', 'Hematopoietic', 'hematopoietic stem cell')
liver_biosource_names = c()
for (biosource in biosources)
{
    related_biosources = deepblue_get_biosource_related(biosource)
    related_biosources_names = deepblue_extract_names(related_biosources)
    liver_biosource_names =c(liver_biosource_names, related_biosources_names)
}

# Obtain the mRNA experiments names

experiments = deepblue_list_experiments(genome = "GRCh38",
                                        type = "signal",
                                        epigenetic_mark = "mRNA",
                                        biosource = liver_biosource_names,
                                        project = "BLUEPRINT Epigenome")
hsc_experiment_names = deepblue_extract_names(experiments)

#perform aggregation

requests = lapply(hsc_experiment_names, function (hsc_experiment) {
    print(paste("Sending request for aggregating", hsc_experiment))
    
    q_exp = deepblue_select_experiments(experiment_name = hsc_experiment)
    q_agg = deepblue_aggregate(data_id = q_exp, 
                               ranges_id = q_genes_regions, 
                               column='VALUE')
    q_filtered = deepblue_filter_regions(query_id = q_agg, 
                                         field="@AGG.MEAN", 
                                         operation=">",
                                         value="0", type="number")
    
    req = deepblue_get_regions(
        query_id = q_filtered,
        output_format = "CHROMOSOME,START,END,@AGG.MEAN,@AGG.MAX,@AGG.MIN")
})

#download the results and save them to disk
results <- deepblue_batch_export_results(
    requests,
    target.directory="data/hsc_aggregation")

#look at first three results
head(results, 3)