# DeepBlue Epigenomic Data Server - R access library
# Use Case 3
# Author: Markus List

# Obtain genes regions
gene_names = c('CCR1', 'CD164', 'CD1D', 'CD2', 'CD34', 'CD3G', 'CD44')

#select regions of selected genes
q_genes = deepblue_select_genes(gene_names, "gencode v23")

#Filter regions that are protein coding
q_genes_regions = deepblue_filter_regions(
    query_id = q_genes,
    field = "@GENE_ATTRIBUTE(gene_type)",
    operation = "==", value = "protein_coding", type = "string")

#Select all T cell related biosources
related_biosources = deepblue_get_biosource_related("Regulatory T cell")
biosource_names = deepblue_extract_names(related_biosources)

# Obtain the mRNA experiments names
experiments = deepblue_list_experiments(genome = "GRCh38",
                                        type = "signal",
                                        epigenetic_mark = "RNA",
                                        biosource = biosource_names,
                                        project = "BLUEPRINT Epigenome")
experiment_names = deepblue_extract_names(experiments)

#perform aggregation
requests = lapply(experiment_names, function (experiment) {
    print(paste("Sending request for aggregating", experiment))

    q_exp = deepblue_select_experiments(experiment_name = experiment)
    q_agg = deepblue_aggregate(data_id = q_exp,
                               ranges_id = q_genes_regions,
                               column='VALUE')
    q_filtered = deepblue_filter_regions(query_id = q_agg,
                                         field="@AGG.MEAN",
                                         operation=">",
                                         value="0", type="number")

    req = deepblue_get_regions(
        query_id = q_filtered,
        output_format = "CHROMOSOME,START,END,@GENE_ID(gencode v23),@GENE_NAME(gencode v23),@AGG.MEAN,@AGG.MAX,@AGG.MIN")
})

#download the results and save them to disk
results <- deepblue_batch_export_results(
    requests,
    target.directory="data/reg_t_cell_aggregation")

#look at the results. from the four experiments listed, two contained the
#information we needed.
results