#' @importFrom utils download.file
NULL

#' Define new id
#'
#' This uploads an empty file to Google drive and sets permissions so the file
#' can then be updated by `shoot()` and downloaded with `loot()`.
#' The id should then be set into `options(shootnloot.ids = c(your_label = "your_new_id"))`.
#'
#' See examples in `?shoot`
#'
#' @inheritParams googledrive::drive_share
#' @param name The name of the file on google drive, does not not impact
#'   the use of the package
#'
#' @return An id
#' @export
new_id <- function(
  role = c("reader", "commenter", "writer", "fileOrganizer",
           "owner", "organizer"),
  type = c("user", "group", "domain",  "anyone"),
  ...,
  name = "shoot.loot"
  ) {
  role <- match.arg(role)
  type <- match.arg(type)
  googledrive::local_drive_quiet()
  path <- tempfile(fileext = ".RDS")
  saveRDS(NULL, path)
  file <- googledrive::drive_upload(
    path,
    name
  )
  googledrive::drive_share(file, role, type, ...)
  unlink(path)
  file$drive_resource[[1]]$id
}

#' Shoot and loot
#'
#' Upload or download an object, a script, or a full workspace
#'
#' @param obj An object to upload
#' @param path Path to a file to upload, if left `NULL`, the active file
#'   (including unsaved changes) is uploaded
#' @param where The label of the id to upload (shoot) to or download (loot) from. You'll need the appropriate rights.
#'
#' @return `shoot_object()` and `loot_object()` return the object, `shoot_object()` returns it invisibly since it's
#'   called for side effects. `shoot_file()`, `loot_file()`, `shoot_worspace()` and `loot_workspace()` return `NULL` invisibly
#'   since they are called for side effects.
#' @export
#' @name shootnloot
shoot_object <- function(obj, where = "default") {
  googledrive::local_drive_quiet()
  opts <- getOption("shootnloot.ids")
  if (! where %in% names(opts)) {
    stop(sprintf("You need to define `options(shootnloot.ids = c(%s =))`, see also `?shootnloot::new_id`", where))
  }
  id <- opts[[where]]
  path <- tempfile(fileext = ".RDS")
  saveRDS(obj, path)
  file <- googledrive::drive_update(
    googledrive::as_id(id),
    path
  )
  unlink(path)
  invisible(obj)
}

#' @export
#' @rdname shootnloot
loot_object <- function(where = "default") {
  opts <- getOption("shootnloot.ids")
  if (! where %in% names(opts)) {
    stop(sprintf("You need to define `options(shootnloot.ids = c(%s =))`, see also `?shootnloot::new_id`", where))
  }
  id <- opts[[where]]

  url <- paste0("https://drive.google.com/uc?id=", id)
  path <- tempfile(fileext = ".RDS")

  download.file(url, path)
  #download.file("https://drive.google.com/uc?id=11Fa2GlcsbFM_rAVuaVEqimTD6stcGeRj", path)
  readRDS(path)
}

#' @export
#' @rdname shootnloot
shoot_file <- function(path = NULL, where = "default") {
  googledrive::local_drive_quiet()
  opts <- getOption("shootnloot.ids")
  if (! where %in% names(opts)) {
    stop(sprintf("You need to define `options(shootnloot.ids = c(%s =))`, see also `?shootnloot::new_id`", where))
  }
  id <- opts[[where]]
  if (is.null(path)) {
    path <- tempfile()
    write(rstudioapi::getSourceEditorContext()$content, path)
    on.exit(unlink(path))
  }
  file <- googledrive::drive_update(
    googledrive::as_id(id),
    path
  )
  invisible(NULL)
}

#' @export
#' @rdname shootnloot
loot_file <- function(where = "default") {
  opts <- getOption("shootnloot.ids")
  if (! where %in% names(opts)) {
    stop(sprintf("You need to define `options(shootnloot.ids = c(%s =))`, see also `?shootnloot::new_id`", where))
  }
  id <- opts[[where]]

  url <- paste0("https://drive.google.com/uc?id=", id)
  path <- tempfile()

  download.file(url, path)
  lines <- readLines(path)
  rstudioapi::documentNew(lines)
  unlink(path)
  invisible(NULL)
}


#' @export
#' @rdname shootnloot
shoot_workspace <- function(where = "default") {
  googledrive::local_drive_quiet()
  opts <- getOption("shootnloot.ids")
  if (! where %in% names(opts)) {
    stop(sprintf("You need to define `options(shootnloot.ids = c(%s =))`, see also `?shootnloot::new_id`", where))
  }
  id <- opts[[where]]
  path <- tempfile(fileext = ".Rdata")
  save(list = ls(all.names = TRUE, envir = .GlobalEnv), envir = .GlobalEnv, file = path)

  file <- googledrive::drive_update(
    googledrive::as_id(id),
    path
  )
  unlink(path)
  invisible(NULL)
}


#' @export
#' @rdname shootnloot
loot_workspace <- function(where = "default") {
  opts <- getOption("shootnloot.ids")
  if (! where %in% names(opts)) {
    stop(sprintf("You need to define `options(shootnloot.ids = c(%s =))`, see also `?shootnloot::new_id`", where))
  }
  id <- opts[[where]]
  url <- paste0("https://drive.google.com/uc?id=", id)
  path <- tempfile()
  download.file(url, path)
  load(path, .GlobalEnv)
  invisible(NULL)
}
