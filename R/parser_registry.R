# Format registry --------------------------------------------------------
#
# Single source of truth for the formats `read_chroms` can read. There is one
# entry per format, recording:
#
#   ext      Regular expression matching the file extension.
#   reader   Function used by the `chromconverter` parser. Absent for formats
#            that only an external parser can read.
#   parsers  Every parser that can read this format. This is both the
#            permission list, which `check_parser` inverts with
#            `parser_formats()`, and the dispatch key, which `reader_for()`
#            resolves to a function.
#   aliases  Other *names* for this same format, which therefore share
#            everything, permissions included. Not for formats that merely
#            happen to be read by the same function: the 'ChemStation' file
#            versions each get their own entry, because the external parsers
#            support different subsets of them.
#   fixed    Arguments that distinguish a format from others read by the same
#            function (e.g. `include = "fid"`), beyond the standard set that
#            `build_converter` matches against the reader's formals.
#   dir      The "file" is really a directory.
#   batch    The reader takes the whole vector of files at once, rather than
#            being applied file by file.
#   internal Names that `read_chroms` accepts but does not advertise: the
#            version-specific ones, which come from `get_filetype` rather than
#            from the user, and legacy spellings kept for compatibility.
#            Either `TRUE`, for the whole entry, or the specific names (the
#            key, its aliases, or both) to leave out of the documentation.
#
# The file version named by `format_in` never selects a parsing routine: the
# readers determine the version from the file itself. It governs only which
# parsers are permitted and how the metadata is labeled.
#
# `test-parser_registry.R` checks this table against the readers' formals and
# against the parser table it replaced.

.chrom_formats <- list(
  agilent_d = list(
    ext = "\\.d$", dir = TRUE, reader = "read_agilent_d",
    parsers = c("chromconverter", "rainbow")
  ),
  agilent_dx = list(
    ext = "\\.dx$", reader = "read_agilent_dx",
    parsers = "chromconverter",
    aliases = c("openlab_dx")
  ),
  agilent_rslt = list(
    ext = "\\.rslt$|\\.sirslt$", dir = TRUE, reader = "read_agilent_rslt",
    parsers = "chromconverter",
    aliases = c("rslt", "sirslt")
  ),
  asm = list(
    ext = "\\.json$", reader = "read_asm",
    parsers = "chromconverter",
    # ASM is the Allotrope Simple Model
    aliases = "allotrope"
  ),
  cdf = list(
    ext = "\\.cdf$", reader = "read_cdf",
    parsers = "chromconverter",
    # `read_cdf` reads both ANDI MS and ANDI chrom, so the standard's name is
    # exactly as broad as the extension it is an alias for
    aliases = "andi"
  ),
  chemstation_ch = list(
    ext = "\\.ch$", reader = "read_chemstation_ch",
    parsers = c("chromconverter", "entab", "rainbow"),
    aliases = c("chemstation", "chemstation_fid"),
    # 'ChemStation' is the software, not a format: it also writes `.uv`, `.ms`
    # and `.csv` files and `.d` directories. The bare name has meant `.ch`
    # here since before the registry, and means report files in
    # `read_peaklist`, so it is kept working but no longer offered.
    internal = "chemstation"
  ),
  chemstation_8 = list(
    ext = "\\.ch$", reader = "read_chemstation_ch",
    parsers = "chromconverter",
    internal = TRUE
  ),
  chemstation_30 = list(
    ext = "\\.ch$", reader = "read_chemstation_ch",
    parsers = c("chromconverter", "entab"),
    internal = TRUE
  ),
  chemstation_81 = list(
    ext = "\\.ch$", reader = "read_chemstation_ch",
    parsers = "chromconverter",
    internal = TRUE
  ),
  chemstation_130 = list(
    ext = "\\.ch$", reader = "read_chemstation_ch",
    parsers = c("chromconverter", "rainbow"),
    internal = TRUE
  ),
  chemstation_179 = list(
    ext = "\\.ch$", reader = "read_chemstation_ch",
    parsers = c("chromconverter", "rainbow"),
    internal = TRUE
  ),
  chemstation_181 = list(
    ext = "\\.ch$", reader = "read_chemstation_ch",
    parsers = "chromconverter",
    internal = TRUE
  ),

  chemstation_csv = list(
    ext = "\\.csv$", reader = "read_chemstation_csv",
    parsers = "chromconverter"
  ),

  chemstation_ms = list(
    ext = "\\.ms$", reader = "read_chemstation_ms",
    parsers = c("chromconverter", "entab", "rainbow")
  ),
  chemstation_2 = list(
    ext = "\\.ms$", reader = "read_chemstation_ms",
    parsers = "chromconverter",
    internal = TRUE
  ),

  # version 2 multi-wavelength files. `read_chemstation_uv` reads versions 31
  # and 131 only, so there is no internal reader for these.
  chemstation_mwd = list(
    ext = "\\.",
    parsers = "entab",
    internal = TRUE
  ),

  chemstation_uv = list(
    ext = "\\.uv$", reader = "read_chemstation_uv",
    parsers = c("chromconverter", "entab", "rainbow"),
    aliases = "chemstation_131",
    internal = "chemstation_131"
  ),
  chemstation_31 = list(
    ext = "\\.uv$", reader = "read_chemstation_uv",
    parsers = c("chromconverter", "entab"),
    internal = TRUE
  ),
  openlab_131 = list(
    ext = "\\.uv$", reader = "read_chemstation_uv",
    parsers = "chromconverter",
    internal = TRUE
  ),

  chromatotec = list(
    ext = "\\.Chrom$", reader = "read_chromatotec",
    parsers = "chromconverter",
    fixed = list(what = "chrom")
  ),
  chromeleon_uv = list(
    ext = "\\.txt$", reader = "read_chromeleon",
    parsers = "chromconverter"
  ),
  csd = list(
    ext = "\\.", batch = TRUE,
    parsers = "openchrom"
  ),
  csv = list(
    ext = "\\.csv$", reader = "read_csv",
    parsers = "chromconverter"
  ),
  masshunter_dad = list(
    ext = "\\.sp$",
    parsers = c("aston", "entab")
  ),
  mdf = list(
    ext = "\\.mdf$", reader = "read_mdf",
    parsers = "chromconverter"
  ),
  msd = list(
    ext = "\\.", batch = TRUE,
    parsers = "openchrom"
  ),
  mzml = list(
    ext = "\\.mzml$", reader = "read_mzml",
    parsers = "chromconverter"
  ),
  mzxml = list(
    ext = "\\.mzxml$", reader = "read_mzml",
    parsers = "chromconverter"
  ),
  other = list(
    ext = "\\.",
    parsers = "entab"
  ),
  shimadzu_ascii = list(
    ext = "\\.txt$", reader = "read_shimadzu",
    parsers = "chromconverter"
  ),
  shimadzu_dad = list(
    ext = "\\.txt$", reader = "read_shimadzu",
    parsers = "chromconverter",
    fixed = list(include = "dad")
  ),
  shimadzu_fid = list(
    ext = "\\.txt$", reader = "read_shimadzu",
    parsers = "chromconverter",
    fixed = list(include = "fid")
  ),
  shimadzu_gcd = list(
    ext = "\\.gcd$", reader = "read_shimadzu_gcd",
    parsers = "chromconverter"
  ),
  shimadzu_lcd = list(
    ext = "\\.lcd$", reader = "read_shimadzu_lcd",
    parsers = "chromconverter"
  ),
  shimadzu_qgd = list(
    ext = "\\.qgd$", reader = "read_shimadzu_qgd",
    parsers = "chromconverter"
  ),
  thermoraw = list(
    ext = "\\.raw$",
    parsers = c("thermoraw", "entab")
  ),
  varian_sms = list(
    ext = "\\.sms$", reader = "read_varian_sms",
    parsers = "chromconverter"
  ),
  waters_arw = list(
    ext = "\\.arw$", reader = "read_waters_arw",
    parsers = "chromconverter"
  ),
  waters_raw = list(
    ext = "\\.raw$", dir = TRUE, reader = "read_waters_raw",
    parsers = c("chromconverter", "rainbow")
  ),
  wsd = list(
    ext = "\\.", batch = TRUE,
    parsers = "openchrom"
  )
)

#' Reader used by each of the external parsers
#'
#' Unlike `chromconverter`, whose reader depends on the format, each binding to
#' an external library reads every format it supports with the same function.
#' @noRd
.parser_readers <- c(entab = "call_entab", rainbow = "call_rainbow",
                     aston = "sp_converter", openchrom = "call_openchrom",
                     thermoraw = "read_thermoraw")

#' Look up a format, returning `NULL` if it is not in the registry
#'
#' Resolves aliases, which are alternative names for the same format.
#' @noRd
format_lookup <- function(format_in){
  entry <- .chrom_formats[[format_in]]
  if (!is.null(entry)) return(entry)
  hit <- vapply(.chrom_formats, function(x) format_in %in% x$aliases, logical(1))
  if (any(hit)) .chrom_formats[[which(hit)[1]]] else NULL
}

#' Resolve a format name to its canonical registry key
#'
#' `supported_formats` offers the aliases to `match.arg` alongside the keys, so
#' a user-supplied `format_in` may be either. Everything that consults the
#' registry resolves aliases itself, but code that compares `format_in` to a
#' format name by hand cannot, so `read_chroms` canonicalizes once up front.
#' Unknown formats are returned unchanged, leaving the error to `format_entry`.
#' @noRd
canonical_format <- function(format_in){
  if (!is.null(.chrom_formats[[format_in]])) return(format_in)
  hit <- vapply(.chrom_formats, function(x) format_in %in% x$aliases, logical(1))
  if (any(hit)) names(.chrom_formats)[which(hit)[1]] else format_in
}

#' Look up a format
#' @noRd
format_entry <- function(format_in){
  entry <- format_lookup(format_in)
  if (is.null(entry)){
    stop(sprintf("The %s format is not supported by chromConverter.",
                 sQuote(format_in)), call. = FALSE)
  }
  entry
}

#' Formats accepted by `read_chroms`
#' @noRd
supported_formats <- function(){
  c(names(.chrom_formats),
    unlist(lapply(.chrom_formats, `[[`, "aliases"), use.names = FALSE))
}

#' Names of a format that are not advertised to the user
#'
#' `read_chroms` accepts every name in the registry, but the version-specific
#' ones come from `get_filetype` rather than from the user, so they are left
#' out of the documentation.
#' @noRd
internal_names <- function(entry){
  if (isTRUE(entry$internal)) c(entry$key, entry$aliases) else
    as.character(entry$internal)
}

#' Document the formats that `read_chroms` accepts
#'
#' The registry is the only place the formats are written down, so the
#' `format_in` documentation is generated from it rather than kept in step by
#' hand. Inserted by the `@eval` tag on `read_chroms`, which is why this
#' returns roxygen lines rather than a formatted string.
#' @noRd
format_in_doc <- function(){
  formats <- vapply(names(.chrom_formats), function(key){
    entry <- c(.chrom_formats[[key]], list(key = key))
    hidden <- internal_names(entry)
    if (key %in% hidden) return(NA_character_)
    aliases <- setdiff(entry$aliases, hidden)
    if (length(aliases) == 0) sprintf("`%s`", key) else
      sprintf("`%s` (or %s)", key, paste0("`", aliases, "`", collapse = ", "))
  }, character(1))
  formats <- sort(formats[!is.na(formats)])
  c("@param format_in Format of the files to be imported or converted. One of:",
    paste0(paste(formats, collapse = ", "), "."),
    "A name in parentheses is an alias, which behaves exactly like the format",
    "it follows. Version-specific names for the 'Agilent ChemStation' formats",
    "(`chemstation_130`, for instance) are accepted as well, but are normally",
    "supplied by chromConverter's own file-type detection rather than being
    provided by the user.")
}

#' Formats each parser can read
#'
#' Inverts the registry into the `parser -> formats` table used by
#' `check_parser`.
#' @noRd
parser_formats <- function(){
  out <- list()
  for (key in names(.chrom_formats)){
    entry <- .chrom_formats[[key]]
    for (parser in entry$parsers){
      out[[parser]] <- c(out[[parser]], key, entry$aliases)
    }
  }
  out
}

#' Format extension
#'
#' Regular expression matching the extension of the specified format.
#' @noRd
format_to_extension <- function(format_in){
  entry <- format_lookup(format_in)
  if (is.null(entry)) "\\." else entry$ext
}

#' Is the format a directory rather than a file?
#' @noRd
format_is_dir <- function(format_in){
  isTRUE(format_lookup(format_in)$dir)
}

#' Does the reader take the whole vector of files at once?
#' @noRd
format_is_batch <- function(format_in){
  isTRUE(format_lookup(format_in)$batch)
}

#' Reader function for a format/parser combination
#' @noRd
reader_for <- function(format_in, parser){
  entry <- format_entry(format_in)
  if (!(parser %in% entry$parsers)){
    stop(sprintf("The %s format cannot be read by the %s parser.",
                 sQuote(format_in), sQuote(parser)), call. = FALSE)
  }
  if (parser == "chromconverter") entry$reader else .parser_readers[[parser]]
}

#' Build a converter for one format/parser combination
#'
#' The readers do not take a uniform set of arguments: `read_thermoraw` has no
#' `data_format`, `read_mzml` no `read_metadata`, `read_varian_sms` no
#' `metadata_format`, and only `read_agilent_rslt` takes `sample_names`. Rather
#' than repeating each reader's argument list in the registry, the standard
#' arguments in `opts` are matched against the reader's own formals.
#'
#' Arguments in `dots` (the `...` of `read_chroms`) are forwarded whole to
#' readers that take `...`, and otherwise matched by name, since splicing an
#' unrecognized argument into a reader without `...` fails at call time.
#'
#' @param format_in Format, as supplied by the user.
#' @param parser Parser, as resolved by `check_parser`.
#' @param opts Named list of standard arguments. Must not contain `parser` or
#' `format_in`, which collide with formals of `read_mzml` and `read_shimadzu`
#' respectively; `format_in` is supplied explicitly to the parsers that need it.
#' @param dots Named list of additional arguments to the reader.
#' @return A function of one argument (a path, or a vector of paths for readers
#' flagged `batch`).
#' @noRd
build_converter <- function(format_in, parser, opts = list(), dots = list()){
  fn_name <- reader_for(format_in, parser)
  fn <- get(fn_name, envir = asNamespace("chromConverter"))
  nms <- names(formals(fn))

  args <- c(opts[intersect(names(opts), nms)], format_entry(format_in)$fixed)
  if ("..." %in% nms){
    extra <- dots
  } else {
    dnames <- names(dots)
    if (is.null(dnames)) dnames <- rep("", length(dots))
    keep <- nzchar(dnames) & dnames %in% nms
    extra <- dots[keep]
    if (any(!keep)){
      unused <- ifelse(nzchar(dnames[!keep]), sQuote(dnames[!keep]), "<unnamed>")
      warning(sprintf(paste0("The following arguments are not accepted by %s ",
                             "and were ignored: %s."), sQuote(fn_name),
                      paste(unique(unused), collapse = ", ")), immediate. = TRUE)
    }
  }
  if (length(extra) > 0) args[names(extra)] <- extra
  function(path) do.call(fn, c(list(path), args))
}
