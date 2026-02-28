devtools::load_all()

library(exp2qcdt)

print("--- 开始测试 ---")

###### 2 sample ######
# 1. 定义您的测试文件路径
# !! 确保这些路径指向您的 *测试* 数据 (只含 D5, D6, F7)
test_meta_file <- "./test/extracted.rename.metadata.csv"
test_exp_file  <- "./test/extracted.rename.standard.fpkm.csv"
test_count_file <- "./test/extracted.rename.standard.count.csv"

# 1. 读取您已经修改好列名的 FPKM 和 Count 文件
# check.names = FALSE 防止 R 自动修改带下划线或特殊字符的列名
dt_fpkm <- read.csv(test_exp_file, check.names = FALSE)
dt_counts <- read.csv(test_count_file, check.names = FALSE)

# 2. 计算 FPKM 矩阵中每个基因的方差
# 假设第一列是 GENE_ID，后面的列都是样本表达量
# apply 函数对每一行（1）计算方差（var）
row_vars <- apply(dt_fpkm[, -1], 1, var)

valid_idx

# 3. 找到方差大于 0 的基因行号（也可以设置一个更严谨的阈值，例如 > 1e-6）
valid_idx <- which(row_vars > 0)

# 4. 同步过滤 FPKM 和 Count 矩阵，确保两者基因顺序和数量完全一致
dt_fpkm_filtered <- dt_fpkm[valid_idx, ]
dt_counts_filtered <- dt_counts[valid_idx, ]

# 5. 将过滤后的数据保存为新的文件
filtered_exp_file <- "./filtered_exp_file.csv"
filtered_count_file <- "./filtered_count_file.csv"

write.csv(dt_fpkm_filtered, filtered_exp_file, row.names = FALSE, quote = FALSE)
write.csv(dt_counts_filtered, filtered_count_file, row.names = FALSE, quote = FALSE)

# 6. 使用清洗后的文件重新运行您的流程
rna_result <- exp2qcdt(
  exp_table_file = filtered_exp_file,
  count_table_file = filtered_count_file,
  phenotype_file = test_meta_file,
  result_dir = test_output_dir
)

rna_result

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

