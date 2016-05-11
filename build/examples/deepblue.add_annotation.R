data = "chr1    28735   29810
chr1    135124  135563
chr1    327790  328229
chr1    437151  438164
chr1    449273  450544
chr1    533219  534114
chr1    544738  546649
chr1    713984  714547
chr1    762416  763445
chr1    788863  789211"
deepblue.add_annotation(name="Interesting annotation", genome="hg19",
    description="It is an annotation with some interesting regions",
    data=data, format="CHROMOSOME,START,END",
    extra_metadata=list(source="My own experiments"),
    user_key="my_private_user_key")