#' Un-normalized count data for mouse retina RNA-seq samples
#'
#' [gene_expression()] returns gene-level raw expression counts from the
#' spaceflight and ground-based experiment involving mouse retina samples.
#'
#' The dataset comprises 35 samples and expression counts for ~57,000 genes
#' (Ensembl IDs).
#'
#' @param format A string indicating the format of the returned object. Must be
#'   one of: `"long"` (default): a tibble with one row per gene × sample combination.
#'   `"wide"`: a numeric matrix of counts with genes as rows and sample
#'   IDs as columns --- suitable for downstream differential expression
#'   analysis.
#'
#' @param only_expressed_genes Logical. If `TRUE`, filters out genes that have
#'   zero counts in all samples. Default is `FALSE`.
#'
#' @returns
#' \describe{
#'   \item{Long format:}{A tibble with columns:
#'     \describe{
#'       \item{`sample_id`}{Character. Unique sample identifier matching those in [samples()].}
#'       \item{`ensembl_gen_id`}{Character. Ensembl gene ID (e.g., `ENSMUSG00000000001`).}
#'       \item{`gene_symbol`}{Gene symbol (e.g., `Gnai3`), provided for convenience.}
#'       \item{`counts`}{Integer. Raw expression counts for that gene in that sample.}
#'     }}
#'   \item{Wide format:}{A numeric matrix. Rows correspond to genes (named by
#'   `ensembl_gen_id`), columns to samples (column names are `sample_id`s).
#'   Expression values are raw un-normalized counts.}
#' }
#'
#' @seealso [samples()]
#'
#' @examples
#' # Un-normalized counts in long format
#' gene_expression()
#'
#' # Un-normalized counts in wide format (matrix)
#' gene_expression("wide")
#'
#' @export
gene_expression <-
  function(format = c("long", "wide"),  only_expressed_genes = FALSE) {

    # Validate options
    format <- match.arg(format)

    # Get local path to `extdata/counts.arrow` (a feather file)
    file <- dataset_path("counts")

    # Import gene expression in long format.
    tbl_long <- readr::read_rds(file = file)

    if (only_expressed_genes) {
      tbl_long <- tbl_long |>
        dplyr::group_by(.data$ensembl_gen_id) |>
        # Keep genes that have non-zero expression in at least one sample.
        dplyr::filter(any(.data$counts != 0)) |>
        dplyr::ungroup()
    }

    # Remove observations with NA
    tbl_long <- stats::na.omit(tbl_long)

    # Convert to wide format (a matrix), if requested.
    if (identical(format, "wide")) {
      tbl_wide <-
        tidyr::pivot_wider(tbl_long, names_from = "sample_id", values_from = "counts")

      mat <- as.matrix(tbl_wide[, -(1:2)])
      row.names(mat) <- tbl_wide$ensembl_gen_id
      return(mat)
    }

    # Or simply return the gene expression table in long format.
    if (identical(format, "long")) {
      return(tbl_long)
    }
  }
