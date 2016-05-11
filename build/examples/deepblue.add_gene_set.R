gs = 'chr1HAVANA\tgene\t11869\t14409\t.\t+\t.\tgene_id "ENSG00000223972.5";",
     "gene_type "transcribed_unprocessed_pseudogene"; gene_status "KNOWN"; ",
     "gene_name "DDX11L1";  level 2; havana_gene "OTTHUMG00000000961.2";\n",
     "chr1\tHAVANA\tgene\t14404\t29570\t.\t-\t.\tgene_id "ENSG00000227232.5";",
     " gene_type "unprocessed_pseudogene"; gene_status "KNOWN"; ",
     "gene_name "WASH7P";  level 2; havana_gene "OTTHUMG00000000958.1";\n",
     "chr1\tENSEMBL\tgene\t17369\t17436\t.\t-\t.\tgene_id "ENSG00000278267.1";",
     " gene_type "miRNA"; gene_status "KNOWN"; gene_name "MIR6859-3";  level 3;'

deepblue.add_gene_set(name = "My new gene set", description = "A gene set",
    data = gs, format = "gtf", extra_metadata = NULL,
    user_key = "my_secret_key")