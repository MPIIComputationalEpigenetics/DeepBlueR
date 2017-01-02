experiment_id = deepblue_select_experiments(
    experiment_name="S00XDKH1.ERX712765.H3K27ac.bwa.GRCh38.20150527.bed")
deepblue_binning (query_data_id=experiment_id, 
    column="SIGNAL_VALUE",
    bins=40)
