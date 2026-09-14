# The registry in `R/parser_registry.R` is the single source of truth for
# `read_chroms`' dispatch. These tests pin it against the things that used to
# be written out separately, so the tables cannot drift apart again.

test_that("every registry entry is well formed", {
  reg <- .chrom_formats
  ns <- asNamespace("chromConverter")
  known_parsers <- c("chromconverter",
                     names(.parser_readers))
  for (key in names(reg)){
    entry <- reg[[key]]
    expect_true(is.character(entry$ext) && nzchar(entry$ext),
                label = sprintf("%s has an extension", key))
    expect_gt(length(entry$parsers), 0)
    expect_true(all(entry$parsers %in% known_parsers),
                label = sprintf("%s lists known parsers", key))
    # a format readable by `chromconverter` must name an internal reader, and
    # no other format should
    if ("chromconverter" %in% entry$parsers){
      expect_true(is.character(entry$reader),
                  label = sprintf("%s names a reader", key))
    } else {
      expect_null(entry$reader, label = sprintf("%s has no reader", key))
    }
    for (parser in entry$parsers){
      fn <- reader_for(key, parser)
      expect_true(exists(fn, envir = ns, mode = "function"),
                  label = sprintf("%s reader %s exists", key, fn))
      if (length(entry$fixed)){
        expect_true(all(names(entry$fixed) %in%
                          names(formals(get(fn, envir = ns)))),
                    label = sprintf("%s fixed args are formals of %s", key, fn))
      }
    }
  }
})

test_that("each external parser reads everything with the same function", {
  # this is why an entry needs only the `chromconverter` reader
  reg <- .chrom_formats
  for (parser in names(.parser_readers)){
    formats <- Filter(function(k) parser %in% reg[[k]]$parsers, names(reg))
    fns <- unique(vapply(formats, reader_for, character(1), parser = parser))
    expect_length(fns, 1)
  }
})

test_that("format names are unique and resolve to one entry", {
  reg <- .chrom_formats
  formats <- supported_formats()
  expect_equal(anyDuplicated(formats), 0)
  aliases <- unlist(lapply(reg, `[[`, "aliases"), use.names = FALSE)
  expect_length(intersect(aliases, names(reg)), 0)
  for (f in formats){
    expect_false(is.null(format_lookup(f)),
                 label = sprintf("%s resolves", f))
  }
  expect_null(format_lookup("not_a_format"))
  expect_error(format_entry("not_a_format"), "not supported")
})

test_that("an alias behaves exactly like the format it names", {
  # `chemstation` and `chemstation_fid` are other names for `chemstation_ch`,
  # so they share its extension, reader and parsers. The 'ChemStation'
  # versions are not aliases: they have their own entries, because the
  # external parsers support different subsets of them.
  for (a in c("chemstation", "chemstation_fid")){
    expect_identical(format_lookup(a), format_lookup("chemstation_ch"))
  }
  expect_identical(format_lookup("chemstation_131"),
                   format_lookup("chemstation_uv"))
  expect_false(identical(format_lookup("chemstation_130"),
                         format_lookup("chemstation_ch")))
})

test_that("the derived parser table matches the one it replaced", {
  # `check_parser`'s `allowed_formats` used to be written out by hand. It is
  # now inverted from the registry, so this pins the derived table against the
  # literal it replaced: it decides which parser reads a given file.
  # Formats added deliberately since then go in `added` below, so that the
  # historical table stays intact and every addition is visible.
  expected <- list(
    openchrom = c("msd", "csd", "wsd"),
    chromconverter = c("agilent_d", "agilent_dx", "agilent_rslt", "asm", "cdf",
                       "chemstation", "chemstation_csv", "chemstation_ch",
                       "chemstation_fid", "chemstation_uv", "chromeleon_uv",
                       "chromatotec", "chemstation_2", "chemstation_ms",
                       "chemstation_8", "chemstation_30", "chemstation_31",
                       "chemstation_130", "chemstation_131", "openlab_131",
                       "chemstation_179", "chemstation_81", "chemstation_181",
                       "mzml", "mzxml", "mdf", "shimadzu_ascii", "shimadzu_dad",
                       "shimadzu_fid", "shimadzu_gcd", "shimadzu_qgd",
                       "shimadzu_lcd", "varian_sms", "waters_arw", "waters_raw",
                       "csv"),
    aston = "masshunter_dad",
    entab = c("chemstation", "chemstation_ms", "chemstation_mwd",
              "chemstation_ch", "chemstation_30", "chemstation_31",
              "chemstation_131", "chemstation_fid", "chemstation_uv",
              "masshunter_dad", "thermoraw", "other"),
    rainbow = c("chemstation", "chemstation_ms", "chemstation_ch",
                "chemstation_130", "chemstation_131", "chemstation_fid",
                "chemstation_179", "chemstation_uv", "waters_raw", "agilent_d"),
    thermoraw = "thermoraw")
  added <- list(chromconverter = c("openlab_dx", "rslt", "sirslt", "andi",
                                   "allotrope"))

  derived <- parser_formats()
  expect_setequal(names(derived), names(expected))
  for (parser in names(expected)){
    expect_setequal(derived[[parser]], c(expected[[parser]], added[[parser]]))
  }
})

test_that("format_to_extension returns the expected pattern", {
  expect_equal(format_to_extension("agilent_d"), "\\.d$")
  expect_equal(format_to_extension("agilent_rslt"), "\\.rslt$|\\.sirslt$")
  expect_equal(format_to_extension("chemstation_uv"), "\\.uv$")
  # aliases inherit the extension of their canonical format
  expect_equal(format_to_extension("chemstation_131"), "\\.uv$")
  expect_equal(format_to_extension("openlab_131"), "\\.uv$")
  expect_equal(format_to_extension("chemstation_181"), "\\.ch$")
  expect_equal(format_to_extension("chemstation"), "\\.ch$")
  expect_equal(format_to_extension("chemstation_2"), "\\.ms$")
  expect_equal(format_to_extension("shimadzu_ascii"), "\\.txt$")
  expect_equal(format_to_extension("shimadzu_qgd"), "\\.qgd$")
  expect_equal(format_to_extension("asm"), "\\.json$")
  expect_equal(format_to_extension("csv"), "\\.csv$")
  expect_equal(format_to_extension("waters_raw"), "\\.raw$")
  # formats that can hold any extension keep the catch-all
  expect_equal(format_to_extension("msd"), "\\.")
  expect_equal(format_to_extension("other"), "\\.")
  expect_equal(format_to_extension("not_a_format"), "\\.")
})

test_that("directory-shaped and batch formats are flagged", {
  expect_true(all(vapply(c("agilent_d", "agilent_rslt", "waters_raw"),
                         format_is_dir, logical(1))))
  expect_false(any(vapply(c("chemstation_uv", "mzml", "shimadzu_lcd", "cdf"),
                          format_is_dir, logical(1))))
  expect_true(all(vapply(c("msd", "csd", "wsd"), format_is_batch, logical(1))))
  expect_false(format_is_batch("chemstation_uv"))
})

test_that("build_converter passes each reader only the arguments it takes", {
  opts <- list(format_out = "matrix", data_format = "wide",
               read_metadata = TRUE, metadata_format = "chromconverter",
               path_out = "path_out", sample_names = "basename",
               verbose = FALSE)
  args_for <- function(format_in, parser = "chromconverter", ...){
    names(environment(build_converter(format_in, parser, opts = opts,
                                      dots = list(...)))$args)
  }
  # `read_thermoraw` has no `data_format`; `read_mzml` no `read_metadata`;
  # `read_varian_sms` and `read_mdf` no `metadata_format`
  expect_setequal(args_for("thermoraw", "thermoraw"),
                  c("format_out", "read_metadata", "metadata_format",
                    "path_out", "verbose"))
  expect_false("read_metadata" %in% args_for("mzml"))
  expect_false("metadata_format" %in% args_for("varian_sms"))
  expect_false("metadata_format" %in% args_for("mdf"))
  # `path_out` and `sample_names` only reach the readers that accept them
  expect_true("path_out" %in% args_for("agilent_dx"))
  expect_false("path_out" %in% args_for("chemstation_uv"))
  expect_true("sample_names" %in% args_for("agilent_rslt"))
  expect_false("sample_names" %in% args_for("agilent_dx"))
  # `read_shimadzu` reads several formats, distinguished by `include`
  expect_true("include" %in% args_for("shimadzu_fid"))
  expect_false("include" %in% args_for("shimadzu_ascii"))
  expect_equal(environment(build_converter("shimadzu_fid", "chromconverter",
                                           opts = opts))$args$include, "fid")
  # `format_in` collides with a formal of `read_shimadzu` and must not leak
  expect_false("format_in" %in% args_for("shimadzu_fid"))
  # `parser` collides with a formal of `read_mzml`
  expect_false("parser" %in% args_for("mzml"))
})

test_that("build_converter forwards `...` only where it is accepted", {
  opts <- list(format_out = "matrix", data_format = "wide")
  # `read_chemstation_uv` has no `...`, but does have a `scale` argument
  cv <- build_converter("chemstation_uv", "chromconverter", opts = opts,
                        dots = list(scale = FALSE))
  expect_false(environment(cv)$args$scale)
  # an argument the reader cannot take is dropped with a warning, rather than
  # failing once per file
  expect_warning(cv <- build_converter("chemstation_uv", "chromconverter",
                                       opts = opts, dots = list(nonsense = 1)),
                 "not accepted")
  expect_false("nonsense" %in% names(environment(cv)$args))
  # readers that take `...` receive everything
  cv <- expect_no_warning(build_converter("cdf", "chromconverter", opts = opts,
                                          dots = list(nonsense = 1)))
  expect_true("nonsense" %in% names(environment(cv)$args))
})

test_that("build_converter rejects a format/parser combination it cannot serve", {
  expect_error(build_converter("waters_arw", "rainbow"), "cannot be read")
})

test_that("canonical_format resolves an alias to its registry key", {
  # `supported_formats` offers the aliases to `match.arg`, so `read_chroms`
  # can be handed one; the comparisons against format names in its body only
  # work if it canonicalizes first
  expect_equal(canonical_format("rslt"), "agilent_rslt")
  expect_equal(canonical_format("sirslt"), "agilent_rslt")
  expect_equal(canonical_format("openlab_dx"), "agilent_dx")
  expect_equal(canonical_format("chemstation"), "chemstation_ch")
  expect_equal(canonical_format("chemstation_fid"), "chemstation_ch")
  # a key is already canonical
  expect_equal(canonical_format("agilent_d"), "agilent_d")
  # an unknown format is passed through for `format_entry` to reject
  expect_equal(canonical_format("nonesuch"), "nonesuch")
})

test_that("internal format names are accepted but not documented", {
  # the list of formats, without the prose that follows it (which names
  # `chemstation_130` as an example of a name that is deliberately left out)
  listed <- format_in_doc()[2]
  internal <- c("chemstation_2", "chemstation_8", "chemstation_30",
                "chemstation_31", "chemstation_81", "chemstation_130",
                "chemstation_179", "chemstation_181", "chemstation_mwd",
                "openlab_131", "chemstation_131",
                # a legacy spelling rather than a detected one, but hidden for
                # the same reason: `read_peaklist` reads it differently
                "chemstation")
  for (nm in internal){
    # `get_filetype` returns these, so `read_chroms` has to accept them
    expect_true(nm %in% supported_formats(), label = nm)
    expect_false(grepl(paste0("`", nm, "`"), listed, fixed = TRUE), label = nm)
  }
  # and every other name the registry knows is documented
  for (nm in setdiff(supported_formats(), internal)){
    expect_true(grepl(paste0("`", nm, "`"), listed, fixed = TRUE), label = nm)
  }
})
