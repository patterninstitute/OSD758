library(readr)
library(dplyr)
library(mgi.report.reader)

(assoc_ensembl_ids <- mgi.report.reader::read_report(report_key = "ensembl_ids"))

counts01 <- readr::read_csv(file = "data-raw/GLDS-664_rna-seq_RSEM_Unnormalized_Counts.csv.xz")

counts02 <-
  counts01 |>
  dplyr::rename(ensembl_gen_id = "...1") |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("MHU"), as.integer))

ensembl_gen_id_to_gene_symbol <-
  tibble::tibble(ensembl_gen_id = counts02$ensembl_gen_id) |>
  dplyr::left_join(assoc_ensembl_ids[c("ensembl_gen_id", "marker_symbol")], by = "ensembl_gen_id") |>
  dplyr::mutate(gene_symbol = dplyr::if_else(!is.na(marker_symbol), .data$marker_symbol, .data$ensembl_gen_id))

counts03 <-
  counts02 |>
  dplyr::left_join(ensembl_gen_id_to_gene_symbol[c("ensembl_gen_id", "gene_symbol")], by = "ensembl_gen_id") |>
  dplyr::relocate(gene_symbol, .after = 1L)

counts04 <-
  counts03 |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("MHU"),
    names_to = "sample_id",
    values_to = "counts"
  ) |>
  dplyr::relocate(sample_id, .before = 1L) |>
  dplyr::arrange(sample_id, ensembl_gen_id)

counts <- counts04

readr::write_rds(x = counts, file = "inst/extdata/counts.rds", compress = "xz")
