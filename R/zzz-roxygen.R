#' Shared params 2D chromatogram
#' @name shared_params
#' @param format_out Class of output. Either `matrix`, `data.frame`, or
#' `data.table`.
#' @param data_format Whether to return data in `wide` (default) or `long` format.
#' @param metadata_format Format to output metadata. Either `chromconverter`
#' or `raw`.
#' @param progress_bar Logical. Whether to show progress bar. Defaults to `TRUE`
#' if `pbapply` is installed.
#' @param cl Argument to [pbapply][pbapply::pbapply] specifying the number
#' of clusters to use or a cluster object created by
#' [makeCluster][parallel::makeCluster]. Defaults to `1`.
#' @param read_metadata Logical. Whether to attach metadata. Defaults to `TRUE`.
#' @param collapse Logical. Whether to collapse lists that only contain a single
#' element. Defaults to `TRUE`.
#' @param scale Whether to scale the data by the scaling factor present in the
#' file. Defaults to `TRUE`.
#' @return A chromatogram in the format specified by the `format_out` and
#' `data_format` arguments.
#' @keywords internal
NULL

#' Generic return (3D)
#' @name generic_return_3D
#' @return A 3D chromatogram in the format specified by `data_format` and
#' `format_out`. If `data_format` is `wide`, the chromatogram will
#' be returned with retention times as rows and wavelengths as columns. If
#' `long` format is requested, three columns will be returned: one for the
#' retention time, one for the wavelength and one for the intensity. The
#' `format_out` argument determines whether the chromatogram is returned as
#' a `matrix`, `data.frame`, or `data.table`. Metadata will be attached to the
#' chromatogram as [attributes] if `read_metadata` is `TRUE`.
#' @keywords internal
NULL

#' Generic return (2D)
#' @name generic_return_2D
#' @return A 2D chromatogram in the format specified by `data_format` and
#' `format_out`. If `data_format` is `wide`, the chromatogram will be returned
#' with retention times as rows and a single column for the intensity. If `long`
#' format is requested, two columns will be returned: one for the
#' retention time and one for the intensity. The `format_out` argument
#' determines whether the chromatogram is returned as a `matrix`, `data.frame`,
#' or `data.table`. Metadata can be attached to the chromatogram as [attributes]
#' if `read_metadata` is `TRUE`.
#' @keywords internal
NULL
