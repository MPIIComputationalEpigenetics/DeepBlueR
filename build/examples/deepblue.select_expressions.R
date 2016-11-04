genes_names =
    c('CCR1', 'CD164', 'CD1D', 'CD2', 'CD34', 'CD3G', 'CD44')
deepblue_select_expressions(
    expression_type="gene",
    sample_ids="s10205",
    identifiers = genes_names,
    gene_model = "gencode v23")
