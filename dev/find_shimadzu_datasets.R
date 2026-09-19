# Find 'Shimadzu' (or any other) raw files across the public metabolomics
# repositories, instead of opening datasets one at a time.
#
# Two services do the work:
#
#  * The GNPS2 dataset cache indexes every *file* in MassIVE, MetaboLights and
#    Metabolomics Workbench, and exposes the index as SQL over HTTP. This is
#    what turns "which datasets have .lcd files" into one query.
#  * MassIVE's PROXI API gives dataset-level metadata, including the instrument
#    model. The file index cannot report an instrument for a vendor format it
#    cannot read -- `instrument_model` is blank for every .lcd -- so the model
#    has to come from here. It is the model that tells you what to expect:
#    LCMS-9030 is a QTOF, LCMS-8040/8045/8050/8060 are triple quadrupoles
#    (so MRM, SIM and product-ion scans).
#
#   source("dev/find_shimadzu_datasets.R")
#   ds <- sz_find_datasets(".lcd")          # one row per dataset
#   sz_find_files(".lcd", max_mb = 5)       # one row per file, smallest first

CACHE <- "https://datasetcache.gnps2.org/datasette/database.json"

sz_cache_sql <- function(sql){
  url <- paste0(CACHE, "?", paste0("sql=", utils::URLencode(sql, reserved = TRUE)),
                "&_size=max")
  res <- jsonlite::fromJSON(url)
  if (length(res$rows) == 0) return(NULL)
  # datasette returns rows as a character matrix, so numeric columns have to be
  # converted back by hand
  m <- res$rows
  out <- as.data.frame(m, stringsAsFactors = FALSE)
  names(out) <- res$columns
  for (j in names(out)){
    v <- suppressWarnings(as.numeric(out[[j]]))
    if (!anyNA(v[!is.na(out[[j]])])) out[[j]] <- v
  }
  out
}

#' Datasets containing files with a given extension, with instrument model
sz_find_datasets <- function(ext = ".lcd", massive_only = TRUE){
  sql <- sprintf(
    "select dataset, count(*) n_files, round(min(size_mb),1) min_mb,
            round(max(size_mb),1) max_mb
     from filename where lower(filepath) like '%%%s'
     group by dataset order by n_files desc", tolower(ext))
  out <- sz_cache_sql(sql)
  if (is.null(out)) return(NULL)
  if (massive_only) out <- out[grepl("^MSV", out$dataset), ]
  out$instrument <- NA_character_
  out$title <- NA_character_
  for (i in seq_len(nrow(out))){
    meta <- tryCatch(jsonlite::fromJSON(paste0(
      "https://massive.ucsd.edu/ProteoSAFe/proxi/v0.1/datasets/", out$dataset[i])),
      error = function(e) NULL)
    if (is.null(meta)) next
    if (length(meta$instruments))
      out$instrument[i] <- paste(unique(meta$instruments$name), collapse = ", ")
    if (length(meta$title)) out$title[i] <- meta$title
  }
  out[order(out$min_mb), ]
}

#' Individual files, smallest first -- useful for picking test fixtures
sz_find_files <- function(ext = ".lcd", max_mb = Inf, dataset = NULL){
  where <- sprintf("lower(filepath) like '%%%s'", tolower(ext))
  if (!is.null(dataset))
    where <- paste0(where, sprintf(" and dataset in ('%s')",
                                   paste(dataset, collapse = "','")))
  if (is.finite(max_mb))
    where <- paste0(where, sprintf(" and size_mb > 0 and size_mb <= %s", max_mb))
  out <- sz_cache_sql(sprintf(
    "select dataset, filepath, round(size_mb,1) size_mb from filename
     where %s order by size_mb asc", where))
  if (is.null(out)) return(NULL)
  out
}

#' Download one file from MassIVE
sz_download <- function(dataset, filepath, destfile){
  url <- paste0("https://massive.ucsd.edu/ProteoSAFe/DownloadResultFile?file=f.",
                dataset, "/", filepath, "&forceDownload=true")
  utils::download.file(url, destfile, mode = "wb", quiet = TRUE)
  destfile
}
