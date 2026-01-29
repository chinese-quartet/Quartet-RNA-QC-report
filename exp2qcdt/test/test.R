devtools::load_all()

library(exp2qcdt)

print("--- 开始测试 ---")

###### 2 sample ######
# 1. 定义您的测试文件路径
# !! 确保这些路径指向您的 *测试* 数据 (只含 D5, D6, F7)
test_meta_file <- "./test/metadata.csv"
test_exp_file  <- "./test/fpkm.csv"
test_count_file <- "./test/counts.csv"


# 2. 定义模板和输出路径
doc_template_path <- system.file("extdata", "quartet_template.docx", package = "exp2qcdt")
# doc_template_path <- './Quartet-RNA-Report_v0.1.docx'
doc_template_path
test_output_dir <- file.path(getwd(), "TEST_OUTPUTS")
dir.create(test_output_dir, showWarnings = FALSE)

# 3. 运行核心函数
print("正在运行 exp2qcdt()...")
rna_result <- exp2qcdt(
  exp_table_file = test_exp_file,
  count_table_file = test_count_file,
  phenotype_file = test_meta_file,
  result_dir = test_output_dir
)

rna_result$qc_metrics_table

# 4. 运行报告生成函数
print("正在生成报告...")
generate_rna_report(
  qc_result = rna_result,
  report_template = doc_template_path,
  report_dir = test_output_dir
)

exp2qcdt::generate_rna_report()
####### 4 samples ######

RNA_sample_fpkm <- system.file("extdata","fpkm.csv",package = "exp2qcdt")
RNA_sample_count <- system.file("extdata","count.csv",package = "exp2qcdt")
RNA_sample_metadata <- system.file("extdata","metadata.csv",package = "exp2qcdt")

# RNA_result = exp2qcdt("~/Downloads/exp2qcdt/test/fpkm_table.txt", "~/Downloads/exp2qcdt/test/counts_table.txt", "~/Downloads/exp2qcdt/test/phenotype.txt", "~/Downloads/exp2qcdt/test/")
RNA_result <- exp2qcdt(exp_table_file = RNA_sample_fpkm,count_table_file = RNA_sample_count,phenotype_file = RNA_sample_metadata,result_dir = "./TEST_OUTPUTS/")
## Get QC report template file path 
RNA_doc_temp <- system.file("extdata","Quartet_temp.docx",package = "exp2qcdt")

##  Generate QC report
GenerateRNAReport(RNA_result = RNA_result,doc_file_path = RNA_doc_temp,output_path = "./TEST_OUTPUTS/")

