devtools::load_all()

library(exp2qcdt)

print("--- 开始测试 ---")

# 1. 定义您的测试文件路径
# !! 确保这些路径指向您的 *测试* 数据 (只含 D5, D6, F7)
test_meta_file <- "./metadata.csv"
test_exp_file  <- "./fpkm.csv"
test_count_file <- "./count.csv"

# 2. 定义模板和输出路径
doc_template_path <- system.file("extdata", "Quartet_temp.docx", package = "exp2qcdt")
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

# 4. 运行报告生成函数
print("正在生成报告...")
GenerateRNAReport(
  RNA_result = rna_result,
  doc_file_path = doc_template_path,
  output_path = test_output_dir
)


