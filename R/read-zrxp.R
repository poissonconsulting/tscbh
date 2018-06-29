process_meta <- function(ele, utc_offset) {
  x <- ele$meta
  
  station <- grep("#REXCHANGE.+", x, useBytes = TRUE, value = TRUE)
  station <- sub("#REXCHANGE", "", station, useBytes = TRUE)
  station <- sub("\\|\\*\\|.+", "", station, useBytes = TRUE)
  
  TZ <- grep(".+TZ.+", x, useBytes = TRUE, value = TRUE)
  
  if (length(TZ)) {
    TZ <- grep(".+TZ.+", x, useBytes = TRUE, value = TRUE)
    TZ <- sub(".+TZ", "", TZ, useBytes = TRUE)
    TZ <- sub("\\|\\*\\|", "", TZ, useBytes = TRUE)
    if(utc_offset == 0) {
      if (!identical(TZ, "UTC"))
        stop("TZ is '", TZ, "' not UTC", call. = FALSE)
    } else if(utc_offset < 0) {
      if (!identical(TZ, paste0("UTC", utc_offset)))
        stop("TZ is '", TZ, "' not UTC", utc_offset, call. = FALSE)
    } else if(!identical(TZ, paste0("UTC+", utc_offset)))
        stop("TZ is '", TZ, "' not UTC+", utc_offset, call. = FALSE)
  }
  
  ele$meta <- list(Station = station)
  ele
}

process_data <- function(ele, utc_offset) {
  x <- ele$data
  
  x <- strsplit(x, " ")
  
  ncol <- length(x[[1]])
  nrow <- length(x)
  
  x <- unlist(x)
  x <- matrix(x, nrow = nrow, ncol = ncol, byrow = T)
  x <- as.data.frame(x)
  
  if (identical(ncol(x), 2L)) x$Status_BCH <- NA_integer_
  
  colnames(x) <- c("DateTime", "Recorded", "Status_BCH")
  
  x$DateTime <- as.character(x$DateTime)
  x$DateTime <- as.POSIXct(x$DateTime, tz = ts_utc_offset_to_tz(utc_offset),
                           format = "%Y%m%d%H%M%S")
  x$Recorded <- as.double(as.character(x$Recorded))
  x$Status_BCH <- as.integer(as.character(x$Status_BCH))
  
  ele$data <- x
  
  ele
}

merge_meta_data <- function(ele) {
  ele$data$Station <- ele$meta$Station
  ele$data
}

#' Read zrxp Data File
#' 
#' A utc_offset of -8 is equivalent to Pacific Standard Time.
#'
#' @param file A string specifying the path to the file.
#' @inheritParams tsdbr::ts_create_db
#' @export
ts_read_zrxp <- function(file = "tscbh.zrxp", utc_offset = -8L) {
  check_string(file)
  check_scalar(utc_offset, c(-12L, 14L))

  if(!file.exists(file))
    stop("file '", file, "' does not exist", call. = FALSE)
  
  conn <- file(file, open = "r")
  lines <- readLines(conn)
  close(conn)
  
  meta <- grep("#REXCHANGE", lines, useBytes = TRUE)
  meta2 <- grep("#ZRXPVERSION", lines, useBytes = TRUE)
  
  if (length(meta2)) meta <- meta2
  
  dat <- grep("#LAYOUT\\(.+\\)\\|\\*\\|", lines, useBytes = TRUE) + 1
  
  if (!length(meta) || !identical(length(dat), length(meta))) 
    stop("'", file, "'is not a recognised zrxp format file", call. = FALSE)
  
  bol <- c(diff(meta) < 6, FALSE)
  meta <- c(meta, length(lines) + 1)
  
  ls <- list()
  for (i in 1:length(dat)) {
    ls[[i]] <- list(meta = lines[meta[i]:(dat[i] - 1)], 
                    data = lines[dat[i]:(meta[i + 1] - 1)])
  }
  if (any(bol)) {
    lack <- ls[bol]
    lack <- lapply(lack, process_meta, utc_offset = utc_offset)
    lack <- lapply(lack, function(x) {x$meta$Station})
    lack <- unlist(lack)
    warning("the following stations lack data: ", punctuate(lack), call. = FALSE)
    ls[bol] <- NULL
  }
  
  ls <- lapply(ls, process_meta, utc_offset = utc_offset)
  ls <- lapply(ls, process_data, utc_offset = utc_offset)
  ls <- lapply(ls, merge_meta_data)
  
  data <- do.call("rbind", ls)
  
  data$Status <- ordered("reasonable", c("reasonable", "questionable", "erroneous"))
  data$Status[!is.na(data$Status_BCH) & data$Status_BCH %in% c(55, 200)] <- "questionable"
  data <- data[c("Station", "DateTime", "Recorded", "Status")]
  data <- data[order(data$Station, data$DateTime), ]
  row.names(data) <- NULL  
  as_tibble(data)
}
