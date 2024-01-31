# ---------------------------------------------------------------------------------- #
#' @title Generate Quartet RNA report 
#'
#' @description Use calculated RNA result to generate report
#'
#' @param RNA_result list 
#' @param temp_doc_path character
#' @param output_path character
#'
#' @return word file
#' 
#' @importFrom dplyr %>%
#' @importFrom flextable flextable
#' @importFrom flextable theme_vanilla
#' @importFrom flextable color
#' @importFrom flextable set_caption
#' @importFrom flextable align
#' @importFrom flextable width
#' @importFrom flextable bold
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme
#' @importFrom officer body_add_par
#' @importFrom flextable body_add_flextable
#' @importFrom officer body_add_gg
#' @importFrom officer body_add_break
#' @importFrom officer read_docx
#' @importFrom officer fp_text
#' @importFrom officer fpar
#' @importFrom officer ftext
#' @importFrom officer body_add_fpar
#' 
#' 
#'
#'
#' @examples
#'# 加载示例 RNA_result 对象
#' RNA_result_path <- system.file("extdata", "RNA_result_example.RData", package = "exp2qcdt")
#' load(RNA_result_path)
#'
#' # 指定包内文档的路径
#' doc_file_path_example <- system.file("extdata", "Quartet_temp.docx", package = "exp2qcdt")
#'
#' # 假设输出路径是当前工作目录
#' output_path_example <- getwd()
#'
#' # 运行函数
#' GenerateRNAReport(RNA_result=RNA_result, doc_file_path=doc_file_path_example, output_path=output_path_example)
#'
#'
#' @export
#' 


GenerateRNAReport <- function(RNA_result = NULL, doc_file_path = NULL, output_path = NULL) {
  
  if(is.null(RNA_result) || is.null(doc_file_path)) {
    stop("All arguments (RNA_result, doc_file_path) are required.")
  }
  
  
  if(is.null(output_path)){
    path <- getwd()
    subDir <- "output"  
    dir.create(file.path(path, subDir), showWarnings = FALSE)
    output_path <- file.path(path,"output")
  } 
  
  ### 创建Evaluate Metrics 表格
  
  ft1 <-  flextable(RNA_result$qc_metrics_table)
  ft1 <- ft1 %>%
    color(~Performance == "Bad",color = "#B80D0D",~Performance) %>%
    color(~Performance == "Fair",color = "#D97C11",~Performance) %>%
    color(~Performance == "Good",color = "#70C404",~Performance) %>%
    color(~Performance == "Great",color = "#0F9115",~Performance) %>%
    width(width = 1.25) %>%
    align(align = "center",part = "all") %>%
    bold( i = 3, part = "body") %>%
    bold(i=1,part = "header")
  
  
  
  ### 绘制Total score 历史分数排名散点图
  # RNA_result$rank_table$scaled_score <- as.numeric(RNA_result$rank_table$scaled_score)
  
  p_rank_scatter_plot <- ggplot(data = RNA_result$quality_score) +
    # 添加四个区域
    geom_rect(aes(xmin = 1, xmax = 2.3, ymin = -Inf, ymax = Inf), fill = "#B80D0D", alpha = 0.08) +
    geom_rect(aes(xmin = 2.3, xmax = 6.7, ymin = -Inf, ymax = Inf), fill = "#D97C11", alpha = 0.08) +
    geom_rect(aes(xmin = 6.7, xmax = 8.3, ymin = -Inf, ymax = Inf), fill = "#70C404", alpha = 0.08) +
    geom_rect(aes(xmin = 8.3, xmax = 10, ymin = -Inf, ymax = Inf), fill = "#0F9115", alpha = 0.08) +
    # 添加基础点图层
    geom_point(aes(x = scaled_score, y = reorder(batch, scaled_score))) +
    # 突出显示 "QC_test" 对应的点
    geom_point(data = subset(RNA_result$quality_score, batch == "QC_test"), 
               aes(x = scaled_score, y = reorder(batch, scaled_score)), 
               color = "orange", size = 3)+
    # 自定义x轴刻度
    scale_x_continuous(breaks = c(1, 2.3, 6.7, 8.3, 10)) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(face = "bold"),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
    labs(x = " ",
         y = " ",
         title="Total Score")
  
  
  #### 设置输出文本
  
  ###### 第一部分 
  ### Assessment Summary 
  
  text_sum_intro = "This report summarizes the quality of the data generated from Quartet RNA reference materials based on several key quality control (QC) metrics. Each metric is accompanied by its current value, historical average, ranking among all datasets evaluated, and the corresponding performance grade."
  
  text_1 = "The performance of the submitted data will be graded as Bad, Fair, Good, or Great based on the ranking by comparing the total score with the historical datasets. The total score is the harmonic mean of the scaled values of the number of features, missing percentage, absolute correlation, coefficient of variation, Signal-to-Noise Ratio (SNR), and relative correlation with reference datasets (RC)."
  ### Four levels of performance
  text_1_sup_1 = "Based on the scaled total score, the submitted data will be ranked together with all Quartet historical datasets. The higher the score, the higher the ranking. After this, the performance levels will be assigned based on their ranking ranges."
  
  text_1_sup_2 = fpar(ftext("· Bad: ", fp_text(bold = TRUE)), ftext("Lowest quintile (0-20th percentile).",fp_text()))
  text_1_sup_3 = fpar(ftext("· Fair: ", fp_text(bold = TRUE)), ftext("Lower middle quartile (21st-50th percentile).",fp_text()))
  text_1_sup_4 = fpar(ftext("· Good: ", fp_text(bold = TRUE)), ftext("Upper middle quartile (51st-80th percentile).",fp_text()))
  text_1_sup_5 = fpar(ftext("· Great: ", fp_text(bold = TRUE)), ftext("Highest quintile (81st-100th percentile).",fp_text()))
  
  #### 第二部分 Quality control metric
  
  
  ### Performance Score
  # text_2 = "Scores of evaluation metrics for the current batch and all historical batches assessed. Please note that the results shown here are scaled values for all batches in each metric. The name of your data is QC_test."
  text_2 = "Scores of evaluation metrics for the current batch and all historical batches assessed. For better comparison and presentation, the total score was scaled to the interval [1, 10], with the worst dataset being 1 and the best dataset scoring 10. Please note that the results shown here are scaled values for all batches in each metric. The name of your data is QC_test."
  
  ### SNR and RC performance
  text_3 = "Performance metrics and thresholds using Quartet RNA reference materials."
  ### Signal-to-Noise Ratio
  text_4 = "Signal-to-noise ratio (SNR) is defined as the ratio of the power of a signal to the power of noise."
  ### RC
  text_5 = "RC was calculate based on the Pearson correlation coefficient between the relative expression levels of a dataset for a given pair of groups and the corresponding reference fold-change values."
  
  
  
  ### Method
  # supplementary_info_1 = "The QC pipeline starts from the expression profiles at peptide/protein levels, and enables to calculate 6 metrics. A Total score is the geometric mean of the linearly normalized values of these metrics."
  # supplementary_info_1_1 = "We expect as many proteins (mapped to gene symbols) as possible for downstreaming analyses."
  # supplementary_info_1_2 = "Too many missing values interfere with comparability. This metric is calculated globally."
  # supplementary_info_1_3 = "A CV value is calculated to indicate the dispersion within replicates feature by feature."
  # supplementary_info_1_4 = "Pearson correlation reflects overall reproducibility within replicates. We calculate correlation coefficients between each two replicates within each biological sample (D5, D6, F7, M8), and take the median as the final value for absolute correlation."
  supplementary_info_1_5 = "SNR is established to characterize the ability of a platform or lab or batch, which is able to distinguish intrinsic differences among distinct biological sample groups ('signal') from variations in technical replicates of the same sample group ('noise')."
  supplementary_info_1_6 = "RC is the Pearson correlation coefficient between the ratios of a test dataset for a given pair of samples and the corresponding ratio-based reference datasets, representing the trend of numerical consistency of the ratio-based expression profiles. To improve reliability, the mean of the three replicates of each sample group was calculated before performing ratio-based expression analysis. Fold changes were transformed using log2 scaling."
  
  
  ### Reference
  supplementary_info_ref1 <- "1. Zheng, Y. et al. Multi-omics data integration using ratio-based quantitative profiling with Quartet reference materials. Nature Biotechnology 1–17 (2023)."
  supplementary_info_ref2 <- "2. Yu, Y. et al. Quartet RNA reference materials improve the quality of transcriptomic data through ratio-based profiling. Nature biotechnology 1–15 (2023)."
  
  # ###Contact us
  # supplementary_info_2_1 = "Fudan University Pharmacogenomics Research Center"
  # supplementary_info_2_2 = "Project manager: Quartet Team"
  # supplementary_info_2_3 = "Email: quartet@fudan.edu.cn"
  
  ### Disclaimer
  # supplementary_info_3 = 'This quality control report is only for this specific test data set and doesn’t represent an evaluation of the business level of the sequencing company. This report is only used for scientific research, not for clinical or commercial use. We don’t bear any economic and legal liabilities for any benefits or losses (direct or indirect) from using the results of this report.'
  supplementary_info_3 = 'This Data Quality Report is provided as an analysis of the specific dataset evaluated and is intended for informational purposes only. While every effort has been made to ensure the accuracy and reliability of the analysis, the information is presented "AS IS" without warranty of any kind, either express or implied. The authors and distributors of this report shall not be held liable for any actions taken in reliance thereon. Users are advised that the findings within this report are not to be used as definitive statements on the quality of any product or process beyond the scope of the dataset assessed. This report is not intended for use in critical applications, commercial decision-making, or for regulatory compliance without professional verification and independent validation. No guarantee, either expressed or implied, is made regarding the use or results of the analysis, including without limitation, the correctness, accuracy, reliability, or applicability of the findings.'
  
  
  ### 读取quarter报告模板并生成报告
  output_file <- file.path(output_path, "Quartet_RNA_report.docx")
  
  
  read_docx(doc_file_path) %>%
    ## 添加报告标题
    body_add_par(value = "Quartet Report for Transcriptomics", style = "heading 1") %>% 
    
    ## 第一部分，Assessment Summary
    body_add_par(value = "Summary", style = "heading 2") %>% 
    body_add_par(value = " ",style = "Normal") %>%
    body_add_par(value = text_sum_intro,style = "Normal") %>%
    body_add_par(value = " ",style = "Normal") %>%
    body_add_flextable(ft1) %>%
    body_add_break()%>%
    
    ### 第二部分 Quality control metric
    body_add_par(value = "QC Metrics", style = "heading 2") %>%
    # body_add_par(value = supplementary_info_1,style = "Normal") %>%
    body_add_par(value = "Signal-to-Noise Ratio (SNR):",style = "heading 3") %>%
    body_add_par(value = supplementary_info_1_5,style = "Normal") %>%
    body_add_par(value = "Relative Correlation with Reference Datasets (RC):",style = "heading 3") %>%
    body_add_par(value = supplementary_info_1_6,style = "Normal") %>%
    
    body_add_par(value = "Total Score:",style = "heading 3") %>%
    body_add_par(value = text_1,style = "Normal") %>%
    ## 分页
    # body_add_break()%>%
    body_add_par(value = "Performance Category:",style = "heading 3") %>%
    body_add_par(value = text_1_sup_1,style = "Normal") %>%
    body_add_fpar(value = text_1_sup_2,style = "Normal") %>%
    body_add_fpar(value = text_1_sup_3,style = "Normal") %>%
    body_add_fpar(value = text_1_sup_4,style = "Normal") %>%
    body_add_fpar(value = text_1_sup_5,style = "Normal") %>%
    ## 分页
    body_add_break()%>%
    
    ### 排名散点图
    body_add_par(value = "Performance Grade", style = "heading 2") %>%
    body_add_gg(value = p_rank_scatter_plot,style = "centered") %>%
    body_add_par(value = text_2,style = "Normal") %>%
    
    ## 分页
    body_add_break()%>%
    
    ### SNR RC cor
    body_add_par(value = "SNR and RC Performance", style = "heading 2") %>%
    body_add_gg(RNA_result$snr_rc_cor,style = "centered")%>%
    body_add_par(value = text_3,style = "Normal") %>%
    
    ## 分页
    body_add_break()%>%
    
    ## 信噪比
    body_add_par(value = "Signal-to-Noise Ratio", style = "heading 2") %>%
    body_add_gg(RNA_result$snr_plot,style = "centered")%>%
    body_add_par(value = text_4,style = "Normal") %>%
    
    ## 分页
    body_add_break()%>%
    
    ## RC
    body_add_par(value = "Correlation with Reference Datasets", style = "heading 2") %>%
    body_add_gg(RNA_result$logfc_plot,style = "centered")%>%
    body_add_par(value = text_5,style = "Normal") %>%
    
    ## 分页
    body_add_break()%>%
    
    ### 附加信息
    body_add_par(value = "Supplementary Information", style = "heading 2") %>%
    # body_add_par(value = "Method", style = "heading 3") %>%
    # body_add_par(value = supplementary_info_1_1, style = "Normal") %>%
    # body_add_par(value = supplementary_info_1_2, style = "Normal") %>%
    # body_add_par(value = supplementary_info_1_3, style = "Normal") %>%
    
    body_add_par(value = "Reference", style = "heading 3") %>%
    body_add_par(value = supplementary_info_ref1, style = "Normal") %>%
    body_add_par(value = supplementary_info_ref2, style = "Normal") %>%
    # body_add_par(value = "Contact us", style = "heading 3") %>%
    # body_add_par(value = supplementary_info_2_1, style = "Normal") %>%
    # body_add_par(value = supplementary_info_2_2, style = "Normal") %>%
    # body_add_par(value = supplementary_info_2_3, style = "Normal") %>%
    body_add_par(value = "Disclaimer", style = "heading 3") %>%
    body_add_par(value = supplementary_info_3, style = "Normal") %>%
    
    ## 输出文件
    print(target = output_file)
  
}