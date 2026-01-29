# EXP2QCDT
Guideline for Quartet RNA QC Pipeline.
The exp2qcdt package aims to convert expression table to qc data table for quartet project.

## Installation

```R
## If you do not have devtools, please install first
library(devtools)
devtools::install_github("chinese-quartet/Quartet-RNA-QC-report", subdir = "exp2qcdt")
```

## Usage

```R
library(exp2qcdt)

## please prepare your input file in the following format. (Make sure the column names are the same format with the example)
rna_sample_fpkm <- system.file("example", "fpkm.csv", package = "exp2qcdt")
rna_sample_count <- system.file("example", "count.csv", package = "exp2qcdt")
rna_sample_metadata <- system.file("example", "metadata.csv", package = "exp2qcdt")

qc_result <- exp2qcdt(rna_sample_fpkm, rna_sample_count, rna_sample_metadata)

## Get QC report template file path 
report_template <- system.file("extdata", "quartet_template.docx", package = "exp2qcdt")

##  Generate QC report
generate_rna_report(qc_result = qc_result, report_template = report_template)
```

## metadata table example

| library                                         | group | sample |
| :---------------------------------------------- | ----- | ------ |
| Quartet_RNA_BGI_BGI2000_PolyA_BGI_D5_1_20200618 | D5_1  | D5     |
| Quartet_RNA_BGI_BGI2000_PolyA_BGI_D5_2_20200618 | D5_2  | D5     |
| Quartet_RNA_BGI_BGI2000_PolyA_BGI_D5_3_20200618 | D5_3  | D5     |
| Quartet_RNA_BGI_BGI2000_PolyA_BGI_D6_1_20200618 | D6_1  | D6     |
| Quartet_RNA_BGI_BGI2000_PolyA_BGI_D6_2_20200618 | D6_2  | D6     |
| Quartet_RNA_BGI_BGI2000_PolyA_BGI_D6_3_20200618 | D6_3  | D6     |
| Quartet_RNA_BGI_BGI2000_PolyA_BGI_F7_1_20200618 | F7_1  | F7     |
| Quartet_RNA_BGI_BGI2000_PolyA_BGI_F7_2_20200618 | F7_2  | F7     |
| Quartet_RNA_BGI_BGI2000_PolyA_BGI_F7_3_20200618 | F7_3  | F7     |
| Quartet_RNA_BGI_BGI2000_PolyA_BGI_M8_1_20200618 | M8_1  | M8     |
| Quartet_RNA_BGI_BGI2000_PolyA_BGI_M8_2_20200618 | M8_2  | M8     |
| Quartet_RNA_BGI_BGI2000_PolyA_BGI_M8_3_20200618 | M8_3  | M8     |

The fpkm and counts library IDs need to be identical to metadata

## reference data

> ref_data_fc_value.csv
> The log2fc of D5/D6, F7/D6, M8/D6 were calculated for the filtered genes based on the reference dataset.

> ref_data_qc_value.csv
> SNR, RC, Total Score value of 21 historical datasets.

## Contributors

- [Jun Shang](https://github.com/stead99)
- [Jingcheng Yang](https://github.com/yjcyxky)
