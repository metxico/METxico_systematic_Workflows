library("heatmaply")
use_counte_file_GNPS "VIEW COMPOUND and FILE"


read.csv("VIEW COMPOUND and FILE.csv")
output_file_name <- read.csv("VIEW COMPOUND and FILE.csv")

heatmaply(output_file_name)
