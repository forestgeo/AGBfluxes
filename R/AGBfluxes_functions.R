#' CTFS-formated data preparation.
#'
#' Main routine to format, detect major obvious errors, and gap-fill those
#' errors in CTFS-formated data.
#'
#' @param site The full name of your site (in lower case); e.g., 'barro colorado
#'   island'.
#' @param stem `TRUE` or `FALSE`, using the stem data (`stem = TRUE`) rather
#'   than the tree data (i.e. called 'full', `stem = FALSE`).
#' @param dbh_units set the unit ("mm" or "cm") of DBH values, by default dbh_units=="mm"..
#' @param taper_correction `TRUE` or `FALSE`, apply Cushman et al (2014) taper
#'   correction.
#' @param fill_missing `TRUE` or `FALSE`, interpolate missing DBH values.
#' @param use_palm_allometry `TRUE` or `FALSE`, if `TRUE`, compute biomass of
#'   palm trees using a palm-specific allometric model from Goodman et al.
#'   (2013).
#' @param flag_stranglers `TRUE` or `FALSE`, individuals of known strangler fig
#'   species greater than 'dbh_stranglers' are flagged
#' @param dbh_stranglers (optional) Minimal diameter (in mm) of strangler figs,
#'   default = 500.
#' @param maxrel A numeric value: the threshold for flagging major errors in
#'   productivity, applied as `absval(individual-tree-productivity) > maxrel *
#'   (average-productivity-per-hectare)`.
#' @param output_errors `TRUE` or `FALSE`, output all records for trees with
#'   major errors in productivity to a csv file.
#' @param DATA_path The pathname where the data are located.
#' @param exclude_interval `NULL` by default. If needed a vector (e.g. c(1,2))
#'   indicating which census interval(s) must be discarded from computation due,
#'   for instance, to a change in measurement protocol.
#'
#' @return A data.table (data.frame) with all relevant variables.
#'
#' @examples
#' \dontrun{
#' data_preparation(
#'   site = "barro colorado island",
#'   stem = TRUE,
#'   taper_correction = TRUE,
#'   fill_missing = TRUE,
#'   use_palm_allometry = TRUE,
#'   flag_stranglers = TRUE,
#'   dbh_stranglers = 500,
#'   max_prod = 0.2,
#'   output_errors = TRUE,
#'   DATA_path = NULL,
#'   exclude_interval = NULL
#' )
#' }
data_preparation <- function(site,
                             stem,
                              dbh_units="cm",
                             use.CTFS.WD,
                             taper_correction,
                             fill_missing,
                             use_palm_allometry,
                             flag_stranglers,
                             dbh_stranglers,
                             max_prod,
                             output_errors,
                             DATA_path = NULL,
                             exclude_interval = NULL) {
  # TODO: Rename data_preparation() to prepare_data()

  # site <- tolower(site)
  # # TODO: DRY tolower(site)
  # INDEX <- match(tolower(site), site.info$site)
  # # TODO: Add this and many more checks in a new check_data_preparation()
  # # TODO: Shorten message: Invalid `site`. See valid sites in `site.info`.
  # if (is.na(INDEX)) {
  #   stop("Site name should be one of the following: \n", paste(levels(factor(site.info$site)), collapse = " - "))
  # }
  # TODO: Remove this if (): Make `DATA_path` the first argument with no default
  if (is.null(DATA_path)) {
    path_folder <- getwd()
    # TODO: Is the double assignment intentional? (i.e`<<-` instead of `<-`)
    DATA_path <<- paste0(path_folder, "/data/")
  }
  # # TODO: Replace `DATA_path` by `.data`: A list of datasets.
  # # TODO: Write a helper that creates the list of datasets given a path.
  # file_names <- list.files(paste0(getwd(),"/data/"))
  # lapply(file_names,load,function(x) paste0(getwd(),"/data/",x))  # doesn't work
  # for (i in 1:length( file_names))
  # {
  #   load(paste0(getwd(),"/data/",file_names[i]))
  # }
  ## For sake of simplicity, we point toward the data stored in "data" folder (see above for automation)
  site <- tolower(site)
  INDEX <- match(tolower(site), site.info$site)
  if (is.na(INDEX)) {
    stop(
      "Site name should be one of the following: \n",
      paste(levels(factor(site.info$site)), collapse = " - "), call. = FALSE
    )
  }

  DATA_path <- paste0(getwd(), "/data/")
  path_folder <- getwd()
  files <- list.files(DATA_path)
  # TODO: Relying on string matching might be dangerous.
  ifelse(
    stem,
    files <- files[grep("stem", files)],
    files <- files[grep("full", files)]
  )

  # # TODO: Why not output a list of objects instead of files in a directory?
  # ifelse(
  #   # FIXME: path_folder is undefined if user provide DATA_path, so this fails.
  #   #   Define `path_folder` here again, or add argument `output_path`?
  #   !dir.exists(file.path(paste0(path_folder, "/output"))),
  #   dir.create(file.path(paste0(path_folder, "/output"))),
  #   FALSE
  # )

  # Create the receiving data.frame
  nms <- c(
    "treeID", "stemID","tag","StemTag","sp", "quadrat", "gx","gy", "dbh", "hom",
    "ExactDate", "DFstatus","codes","date", "status","CensusID", "year"
  )
  # FIXME: Growing an object can be terribly slow. Instead of creating a dataframe
  # with one row you should create a dataframe with as many rows as you need.
  df <- receiving_df(nms)

  # TODO: Is this code mainly finding the mean date from each census?
  #   If so, you may create `mean_date()` and `lapply()` it to each list-item.
  #   And do it in a helper hide unimportant details.
  # TODO: see seq_along()
  for (i in 1:length(files)) {
    # TODO: Again, this could be avoided if the censues come in a `.data` list
    temp <- data.table::setDT(LOAD(paste(DATA_path, files[i], sep = "/")))
    temp$CensusID <- i

    # TODO: Too-long line. Need a meaningfully-named intermediary variable?
    temp[, year := round(mean(as.numeric(format(as.Date(date, origin = "1960-1-1"), "%Y")), na.rm = T))]

    # TODO: Clarify this condition: Store it in a variable with meaninful name
    # TODO: match() is oftern harder to read than %in% (see ?match())
    if (any(is.na(match(names(df), names(temp))))) {
      ID <- which(is.na(match(names(df), names(temp))))
      stop(
        # TODO: glue::glue() and friends help write more readable error messages
        paste0(
          "The data must have a column named ", names(df)[ID], collapse = " - "
        ),
        call. = FALSE
      )
    }

    # TODO: match() is oftern harder to read than %in% (see ?match())
    temp <- temp[, match(names(df), names(temp)), with = FALSE]
    df <- rbind(df, temp)
  }

  df <- df[-1, ]
  message("Step 1: Data import done.")


  # STEP 1: data consolidation
  df <- consolidate_data(df, dbh_units,taper_correction, fill_missing, stem)

  # STEP 2: Compute AGB
  df <- computeAGB(df, site,DBH=NULL,use.CTFS.WD = T , H = NULL, use_palm_allometry)

  # STEP 3: Format intervals
  DF <- format_interval(df, flag_stranglers, dbh_stranglers)


  DF <- flag_errors(
    DF,
    site,
    flag_stranglers = flag_stranglers,
    max_prod =0.2,
    output_errors = output_errors,
    exclude_interval = exclude_interval
  )


  # TODO: Again, Why not output an object rather than write files?
  save(DF, file = paste0(getwd(), "/data/", site, "_formated_data.Rdata"))

  DF[order(year,treeID)]
}



# TODO: Review from here onwards

# QUestion: do I need to declare and examplify all functions that are called
# internally, such as correctDBH, Check_status, etc??
# ANSWER: You don't need to give examples for internal funcitons. In fact, I
# would avoid the `@examples` tag in internal funcitons. If you want to give
# examples for internal use, you could simply use normal comments, not roxygen
# comments, like this:
# Examples:
# data(df)
# # Correct for change in POM and missing value
# df <- consolidate_data(df, taper_correction=T, fill_missing=T, stem=T)
# df[treeID==212507] # show the result with missing value
# # Correct for missing value only
# df <- consolidate_data(df, taper_correction=F, fill_missing=T, stem=T)
# df[treeID==212507] # show the result with missing value
# # No correction
# df <- consolidate_data(df, taper_correction=F, fill_missing=F, stem=T)
# df[treeID==212507] # show the result with missing value

#' Check for consistency across censuses.
#'
#' This function checks for each stem that (i) its status (i.e. alive or dead)
#' is consistent across all censuses (e.g. can't be dead and alive at next
#' census), (ii) assign a mean census date (when missing) and (iii) interpolate
#' DBHs when missing (fill_missing=T). Additionnaly, unecessary information
#' (i.e. replication of dead trees/recruits) is discarded.
#'
#' @param taper_correction `TRUE` or `FALSE`, are you willing to apply Cushman
#'   et al (2014) taper correction?
#' @param fill_missing `TRUE` or `FALSE`, are you willing to extrapolate missing
#'   DBH from surrounding DBHs?
#' @param stem Is the function applied over stem (stem=TRUE) or tree
#'   (stem=FALSE) data?
#'
#' @return A data.table (data.frame) with 3 new variables: status1 (corrected
#'   status),dbh2 (corrected dbh) and hom2 (corrected height of measure). A
#'   unique stem id ("id") is created if stem = T.
#'
#' @keywords internal
#' @noRd
consolidate_data <- function(df, dbh_units,taper_correction, fill_missing, stem) {
  if (stem) {
    df[, "id" := paste(treeID, stemID, sep = "-")] # creat a unique tree-stem ID
    df <- df[order(id, CensusID)]
  } else {
    df[, "id" := treeID]
  }

  # check that the status is consistent over all censuses (e.g. can't be dead
  # and alive at next census)
  df[, status1 := check_status(.SD), by = id]
  # discard all priors & replicated dead trees
  df <- df[!status1 %in% c("P", "Dr")]


  # Add average date of census when missing for alive trees (mandatory for
  # interpolating DBHs)
  # TODO: Try simplify this for clarity and to fit it in a line (80 characters).
  # e.g., You could write a helper:
  # round_date <- function(x) {
  #   formatted_date <- format(as.Date(x, origin = "1960-01-01"), "%Y")
  #   round(mean(as.numeric(formatted_date), na.rm = TRUE))
  # }
  # Then write:
  # df[ , "year" := round_date(ExactDate), by = CensusID]
  df[, "year" := round(mean(as.numeric(format(as.Date(ExactDate, origin = "1960-1-1"), "%Y")), na.rm = T)), by = CensusID] # Assign 1 year per census

  if ("quadrat" %in% names(df)) {
    # TODO: It's good style to spell out TRUE and FALSE (instead of T and F)
    # TODO: You are using `mean(x, na.rm = TRUE)` a lot. Maybe write a little
    # helpre in utils.R as: mean0 <- function(x) mean(x, na.rm = TRUE)
    DATE <- df[, .(date = mean(date, na.rm = T)), by = .(year, quadrat)][order(year)]
    DATE[, "ID1" := paste(year, quadrat, sep = "-")]
    df$ID <- paste(df$year, df$quadrat, sep = "-")
    #  TODO: Assignment is a very special operation. It's clearer to have only
    # one assignment per line. This will also avoid the code form going
    # beyond the 80 characters that make up a reasonable line.
    df <- within(df, date[is.na(date)] <- DATE$date[match(df[is.na(date), ID], DATE$ID1)])
    df[, ID := NULL]
  } else {
    DATE <- df[, .(date = mean(date, na.rm = T)), by = year][order(year)]
    df <- within(df, date[is.na(date)] <- DATE$date[match(df[is.na(date), "year"]$year, DATE$year)])
  }
  # TODO: What is the meaning of uppercase? If there is no special meaning, then
  #   it's best to stick to lowercase.
  # TODO: It's safest to replace dots "." with underscore "_". Dots are best
  #   reserved for S3 methods.
  # Remove stems without any measurement in any census
  NO.MEASURE <- df[, all(is.na(dbh)), by = id]
  df <- df[!id %in% NO.MEASURE$treeID[NO.MEASURE$V1]]

  # Taper correction or missing values: -> fill gaps for missing values
  # might be time consuming (25 minutes for BCI)
  df[, c("dbh2", "hom2") := correctDBH(.SD, taper_correction = taper_correction, fill_missing = fill_missing), by = id]
  NO.MEASURE <- df[, all(is.na(dbh2)), by = treeID]
  # remove trees without any measurement
  df <- df[!treeID %in% NO.MEASURE$treeID[NO.MEASURE$V1]]

  # Check that DBHs are in mm
  if (dbh_units=="cm") {
  df[, c("dbh", "dbh2") := list(dbh*10,dbh2*10)]
    }
  message("Step 2: Data consolidation done.")

  df
}



# Examples
# data(df)
# correctDBH(df[treeID==240]) # DOESN'T work. Need to pass 'check_status' first

#' Taper correction and linear interpolaiton.
#'
#' Perform two mains tasks: (a) apply a taper correction when POM is > 130 cm,
#' and (b) linear interpolation values when missing DBHs. Interpolation of
#' missing values is done by averaging surrounding available DBH values.
#'
#' @param DF A data.table.
#' @param taper_correction `TRUE` or `FALSE`, are you willing to apply Cushman
#'   et al (2014) taper correction?
#' @param fill_missing `TRUE` or `FALSE`, are you willing to extrapolate missing
#'   DBH from surrounding DBH?
#'
#' @return A data.table (data.frame) with all relevant variables.
#'
#' @keywords internal
#' @noRd
correctDBH <- function(DT, taper_correction, fill_missing) {
  dbh2 <- DT[status1 == "A", "dbh"][[1]]
  # round hom to be at 1.3 meter (avoiding odd rounding)
  hom2 <- DT[status1 == "A", round(hom * 100) / 100]
  loc <- which(is.na(dbh2))
  DATE <- DT[status1 == "A", "date"][[1]]
  hom2[is.na(hom2) & !is.na(dbh2)] <- 1.3

  # TODO: Extract this in its own function. It'll make for clearer code.
  # 1. Apply Cushman's correction to trees with POM changed
  if (taper_correction & any(hom2 != 1.3, na.rm = T)) {
    hom2[is.na(hom2)] <- 1.3
    if (taper_correction & any(hom2 != 1.3, na.rm = T)) {
      loc1 <- hom2 != 1.3 & !is.na(dbh2)
      if (!any(is.na(loc1))) {
        dbh2[loc1] <- round(dbh2[loc1] * exp(0.0247 * (hom2[loc1] - 1.3)), 1)
        hom2[loc1] <- 1.3
      }
    }
  }

  # TODO: Extract this in its own function. It'll make for clearer code.
  # 2. Fill gaps
  if (fill_missing & any(is.na(dbh2))) {

    # dead stem
    if (any(grep("D", DT$status1))) {
      # if last value is NA, last valid DBH measure is replicated (RULE=2)
      ifelse(length(dbh2) %in% loc, RULE <- 2, RULE <- 1)

      tryCatch(
        stats::approx(DATE, dbh2, rule = RULE, xout = DT$date[loc])$y,
        error = function(e) {
          # TODO: I replaced print() with message(). Are you sure you want to
          # only give some information? Consider using warning() or stop().
          message(unique(paste("Stem id", DT$id, "generates a problem.")))
        }
      )

      if (any(is.na(stats::approx(DATE, dbh2, rule = RULE, xout = DATE[loc])$y))) {
        message(DT$id)
      }
      dbh2[loc] <- stats::approx(DATE, dbh2, rule = RULE, xout = DATE[loc])$y
      hom2[loc] <- stats::approx(DATE, hom2, rule = RULE, xout = DATE[loc])$y

      dbh2 <- c(dbh2, NA) # add NA at year of death
      hom2 <- c(hom2, NA)
      # end of dead stems
    } else {
      # alive stems
      # if last value is NA, last valid BDBH measure is replicated (RULE=2)
      ifelse(length(dbh2) %in% loc, RULE <- 2, RULE <- 1)
      tryCatch(
        stats::approx(DT$date, dbh2, rule = RULE, xout = DT$date[loc])$y,
        error = function(e) {
          message(paste("Stem id", DT$id, "generates a problem."))
        }
      )

      dbh2[loc] <- stats::approx(DT$date, dbh2, rule = RULE, xout = DT$date[loc])$y
      hom2[loc] <- stats::approx(DT$date, hom2, rule = RULE, xout = DT$date[loc])$y
    } # end of alive stems

    # Resprouts
    RESP <- grep("\\bR\\b", DT$codes[is.na(DT$dbh)])
    if (length(RESP) > 0) {
      dbh2[RESP] <- NA
      hom2[RESP] <- 1.30
    }
    # TODO: Try to simplify the code so it's not so nested. Too much logic is
    # difficult to follow, test, and debug. See https://rstd.io/code-smells
  } else { # end of fill_missing

    # Replicate dbh & hom if no NA values or fill_missing = `FALSE`
    dbh2 <- DT[, "dbh"][[1]]
    hom2 <- DT[, round(hom * 100) / 100]
    hom2[is.na(hom2) & !is.na(dbh2)] <- 1.3
    if (taper_correction & any(hom2 != 1.3, na.rm = T)) {
      loc1 <- hom2 != 1.3 & !is.na(dbh2)
      if (!any(is.na(loc1))) {
        dbh2[loc1] <- round(dbh2[loc1] * exp(0.0247 * (hom2[loc1] - 1.3)), 1)
        hom2[loc1] <- 1.3
      }
    }
  } # end if fill_missing = F or no missing dbh

  list(dbh2, hom2)
}

#' Biomass computation.
#' Allocate wood density and compute above-ground biomass using the updated
#' model of Chave et al. (2014), given in Rejou-Mechain et al. (2017). Palm
#' trees (palm=T) are computed using a different allometric model (Goodman et
#' al. 2013).
#'
#' @param df The data.frame on which AGB shall be computed.
#' @param DBH Optional, specify the variable to be used (e.g. "dbh" or
#'   "dbh_corrected").
#' @param WD Optional, provide an external data.frame of wood densities by
#'   species.
#' @param H Optional, specify the column with height measurments.
#' @param use_palm_allometry `TRUE` or `FALSE`, if `TRUE`  biomass of palm trees
#'   is computed through a specific allometric model (Goodman et al. 2013). Only
#'   valid for South America.
#'
#' @return A data.table (data.frame) with all relevant variables.
#' @keywords internal
#' @noRd
computeAGB <- function(df,
                       site,
                       DBH = NULL,
                       use.CTFS.WD = T ,
                       H = NULL,
                       use_palm_allometry) {
  if (!exists("DATA_path")) {
    # TODO: `<<-` is dangerous. Are you sure you need it?
    DATA_path <<- paste0(path_folder, "/data/")
  }
  # Allocate wood density
  df <- assignWD(df, site=site, use.CTFS.WD = use.CTFS.WD)


  # Compute biomass
  df <- assignAGB(df, site=site, DBH = DBH, H = H,use_palm_allometry=use_palm_allometry)


   message("Step 3: AGB calculation done.")
   df
}

#' Assign wood density.
#'
#' Assign wood density using CTFS wood density data base (WSG).
#'
#' @param df A data.table.
#' @param site Provide the full name of your site (in lower case) i.e. 'barro
#'   colorado island'.
#' @param wsgdata A list of tree species (mnenomic) and corresponding wood
#'   density.
#' @param denscol The variable to be return ("wsg" by default). No other option
#'   is implemented.
#'
#' @return A data.table (data.frame) with all relevant variables.
#' @keywords internal
#' @noRd
assignWD <- function(DAT, site, use.CTFS.WD = T) {
  if (is.null(DATA_path)) {
    DATA_path <<- paste0(path_folder, "/data/")
  }
  # if("wsg"%in%names(DAT)) {
  #   A <- utils::menu(
  #     c("Y", "N"),
  #     title = "WD already present. Do you want to update those?"
  #     )
  # }
  # if (A==T) {
  # Add genus & species to data
  SP <- LOAD(paste(DATA_path, list.files(DATA_path)[grep("spptable", list.files(DATA_path))], sep = "/"))
  # FIXME: Replace subset() with `[`. See ?subset():
  # > Warning: This is a convenience function intended for use interactively.
  # For programming it is better to use the standard subsetting functions like
  # [, and in particular the non-standard evaluation of argument subset can have
  # unanticipated consequences.
  SP <- subset(SP, , c("sp", "Genus", "Species", "Family"))
  # TODO: Remove sep. paste() defaults to `sep = " "`.
  SP$name <- paste(SP$Genus, SP$Species, sep = " ")

  if (use.CTFS.WD) {
    # Import & format CTFS wood data base
    # TODO: This approach makes code clearer. E.g.:
    # meaningful_name <- site.info$wsg.site.name[site.info$site == tolower(site)]
    # wsgdatamatch <- which(WSG %in% meaningful_name)
    wsgdatamatch <- which(
      WSG$site %in% site.info$wsg.site.name[site.info$site == tolower(site)]
    )

    if (length(wsgdatamatch) == 0) stop("Site name doesn't match!")

    wsg <- unique(WSG[wsgdatamatch, c("genus", "species", "spwood") ])
    names(wsg) <- c("genus", "species", "wd")
    wsg <- wsg[!is.na(wsg$wd), ]
    # TODO: Sure you need invisible? What are you trying to accomplish?
    # Maybe you mean to use suppressMessages() or suppressWarnings()?
    # TODO: Replace "A" by a more informative name.
    A <- invisible(BIOMASS::getWoodDensity(
        SP$Genus,
        SP$Species,
        stand = rep(site, nrow(SP)),
        family = NULL,
        region = "World",
        addWoodDensityData = wsg)
    )
  } else {
    A <- invisible(
      BIOMASS::getWoodDensity(
        SP$Genus,
        SP$Species,
        stand = rep(site, nrow(SP)),
        family = NULL,
        region = "World"
      )
    )
  }

  # Assign WD by taxon
  A$name <- paste(A$genus, A$species, sep = " ")

  SP <- unique(merge(SP, A[, c("name", "meanWD")], by = "name", all.x = T))
  names(SP) <- c("name", "sp", "genus", "species", "Family", "wsg")
  if (any(grep("name", names(DAT)))) {
    DT <- merge(DAT[,-c("sp","wsg")], SP, by = "name", all.x = T)
  } else {
    DT <- merge(DAT[,-c("wsg")], SP, by = "sp", all.x = T)
  }

  # Allocate mean WD to species not in the list
  # TODO: Extract the message into inform_something() to clarify intention. The
  # long message obscures what this code does:
  # if (any(is.na(DT$wsg))) {
  #  iform_something()
  #   DT <- within(DT, wsg[is.na(wsg)] <- mean(wsg, na.rm = T))
  # }
  if (any(is.na(DT$wsg))) {
    message(paste0(
        "There are ", nrow(DT[is.na(wsg)]),
        " individuals without WD values. Plot-average value (",
        round(mean(DT$wsg, na.rm = T), 2), ") was assigned."
      ))

    DT <- within(DT, wsg[is.na(wsg)] <- mean(wsg, na.rm = T))
  }

  DT
  # } else {
  #   DAT
  # }
}


#' Assign AGB.
#'
#' Compute above-ground biomass using the updated model of Chave et al. (2014),
#' given in Rejou-Mechain et al. (2017).
#'
#' @param site Provide the full name of your site (in lower case) i.e. 'barro
#'   colorado island'.
#' @param D A column with tree diameters (in mm).
#' @param WD A column name (e.g. "wsg") or data.set with wood densities
#'   (optional).
#' @param H A column name (e.g. "height") or data.set with tree heights
#'   (optional).
#'
#' @return A vector with AGB values (in Mg).
#' @keywords internal
#' @noRd
assignAGB <- function(DAT, site, DBH = NULL, H = NULL,use_palm_allometry) {
  if (!is.null(DBH)) {
    D <- DAT[, get(DBH)]
  } else {
    D <- DAT$dbh2
  }
  if (!any(grepl("wsg", names(DAT)))) {
    stop("you must assign WD first through assignWD()", call. = FALSE)
  }
  if (!is.null(H)) {
    H <- df[, ..height]

    if (length(D) != length(H)) {
      stop(
        "H and D have different length, or column 'height' not provided",
        call. = FALSE
      )
    }

    if (any(is.na(D))) {
      warning("NA values in D", call. = FALSE)
    }

    DAT$agb <- (0.0673 * (DAT$wsg * H * (D / 10)^2)^0.976) / 1000
  } else {
    INDEX <- match(tolower(site), site.info$site)
    if (is.na(INDEX)) {
      # TODO: You are using `paste(levels(factor(site.info$site))` in three
      # places. You may wrap it in a helper: `collapse_levels("site")`
      stop(
        "Site name should be one of the following: \n",
        paste(levels(factor(site.info$site)), collapse = " - "), call. = FALSE
      )
    }
    E <- site.info$E[INDEX]
    # TODO: Clarify the meaning of these numbers by storing them in a
    # meaningfully named variable or function:
    # DAT$agb <- well_named_funciton(DAT$wsg, E, D)
    DAT$agb <- exp(-2.023977 - 0.89563505 * E + 0.92023559 *
        log(DAT$wsg) + 2.79495823 * log(D / 10) - 0.04606298 *
        (log(D / 10)^2)) / 1000
  }

  # Compute biomass for palms
  if (use_palm_allometry) {
    agbPalm <- function(D) {
      exp(-3.3488 + 2.7483 * log(D / 10) + ((0.588)^2) / 2) / 1000
    }

    # Assign medium DBH by stem by species
    MED_DBH <- DAT[Family=="Arecaceae",median(dbh2,na.rm=T),by=name]
    DAT[!is.na(dbh2),"dbh2":=ifelse(is.na(MED_DBH$V1[match(name,MED_DBH$name)]),dbh2,MED_DBH$V1[match(name,MED_DBH$name)])]

  if (!is.na(match("family", tolower(names(DAT))))) {
      DAT[Family == "Arecaceae", "agb" := agbPalm(dbh2)]
    } else {
      SP <- LOAD(paste0(DATA_path, list.files(DATA_path)[grep("spptable", list.files(DATA_path))]))
      trim <- function(x) gsub("^\\s+|\\s+$", "", x)
      SP$genus <- trim(substr(SP$Latin, 1, regexpr(" ", SP$Latin)))
      SP$species <- trim(substr(SP$Latin, regexpr(" ", SP$Latin), 50))
      SP <- SP[, c("sp", "genus", "species", "Family")]
      SP$name <- paste(SP$genus, SP$species, sep = " ")
      names(SP) <- c("sp", "genus", "species", "Family", "name")
      DAT <- merge(DAT, SP, by = "sp", all.x = T)
      DAT[Family == "Arecaceae", "agb" := agbPalm(dbh2)]
    }
  }

  DAT
}
#' Format census intervals.
#'
#' Create census intervals (i.e. put consecutive census side by side), assign
#' status by tree (i.e. alive (A),dead (D), recruited (R) or resprout (Rsp)).
#'
#' @param df A data.table.
#' @param flag_stranglers `TRUE` or `FALSE`, individuals of known strangler fig
#'   species greater than 'dbh_stranglers' are flagged.
#' @param dbh_stranglers (optional) Minimal diameter (in mm) of strangler figs,
#'   default = 500.
#'
#' @return A formated data.table.
#' @keywords internal
#' @noRd
format_interval <- function(df,
                            flag_stranglers,
                            dbh_stranglers,
                            code.broken = NULL) {

  YEAR <- sort(unique(df$year))

  # Receiveing data set
  nms <- c(
    "treeID", "dbh1", "dbhc1",  "status1", "code1",  "hom1",  "sp", "wsg",
    "agb1", "date1", "Nstem1", "dbh2",  "dbhc2", "status2",  "code2", "hom2", "agb2",
    "date2", "Nstem2", "broken",  "agbl",  "agb1.surv", "interval", "year"
  )
  # FIXME: Growing an object can be terribly slow. Instead of creating a
  # dataframe with one row you should create a dataframe with as many rows as
  # you need.
  DF2 <- data.table::setDT(receiving_df(nms))

   # Avoid missing HOM (30/11/2018)
  MISS_HOM <- df[, .I[is.na(hom) & status1!="D"],]
  HOM <- df[MISS_HOM-1,hom]
  HOM[is.na(HOM)] <- 1.3  # assign 1.3 to missing values (if no previous value)
  df <- within(df, hom[MISS_HOM] <- HOM) # replace missing HOM by previous one

  # 4 minutes to run
  for (j in 1:(length(YEAR) - 1)) {
    # keep only information for the biggest alive stem per treeID
    i1 <- df[year == YEAR[j] & status1 != "D", .I[which.max(dbh2)], by = treeID]
    # TODO: Clarify the meaning of these strings by storing them in a
    # meaningfully named variable.
    A1 <- df[i1$V1, c("treeID", "dbh", "dbh2", "status1", "codes", "hom", "agb", "id", "sp", "wsg")]
    names(A1) <- c("treeID", "dbh1", "dbhc1", "status1", "code1", "hom1", "agb", "id1", "sp", "wsg")
    B1 <- df[year == YEAR[j] & status1 != "D", list("agb1" = sum(agb, na.rm = T), "date1" = mean(date, na.rm = T),"Nstem1"=length(agb)), by = treeID]
    BB <- merge(B1, A1, by = "treeID", all.x = T)
    cens1 <- BB[, c("treeID", "dbh1", "dbhc1", "status1", "code1", "hom1", "agb", "id1", "sp", "wsg", "agb1", "date1","Nstem1")]

    i2 <- df[year == YEAR[j + 1] & status1 != "D", .I[which.max(dbh2)], by = treeID]
    A2 <- df[i2$V1, c("treeID", "dbh", "dbh2", "hom", "status1", "codes", "id")]
    names(A2) <- c("treeID", "dbh2", "dbhc2", "hom2", "status2", "code2", "id2")
    B2 <- df[year == YEAR[j + 1], list("agb2" = sum(agb[status1 != "D"], na.rm = T), "date2" = mean(date, na.rm = T),"Nstem2"=length(agb[status1 != "D"])), by = treeID]
    BB2 <- merge(B2, A2, by = "treeID", all.x = T)
    cens2 <- BB2[, c("treeID", "dbh2", "dbhc2", "status2", "code2", "hom2", "agb2", "date2", "Nstem2","id2")]
    cens2 <- within(cens2, status2[is.na(status2)] <- "D")

    ID <- data.table::setDT(data.frame(treeID = unique(c(cens1$treeID, cens2$treeID))))
    ID <- merge(ID, cens1, by = "treeID", all.x = T)
    ID <- merge(ID, cens2, by = "treeID", all.x = T)

    # Process broken stems
    ID[, "broken" := ifelse(dbhc2 / dbhc1 < 0.8 & dbhc1 > 100 & hom2 <= hom1, 1, 0)] # flag large broken main stems to be added to losses
    ID[broken == 1, "agbl" := agb]
    ID[broken == 1, "agb1.surv" := agb1 - agbl]

    ID[, c("agb", "id1", "id2") := NULL]
    ID$interval <- j
    ID$year <- YEAR[j + 1]
    DF2 <- rbind(DF2, ID)
  }
  DF2 <- DF2[-1, ]

  # Add coordinates & other mandatory information
  COORD <- df[, .(gx = round(10 * gx[!is.na(gx)]) / 10, gy = round(10 * gy[!is.na(gx)]) / 10, quadrat = quadrat, name = name), by = treeID]
  COORD <- unique(COORD[!duplicated(COORD$treeID)])
  DF2 <- merge(DF2, COORD, by = "treeID", all.x = T)

  # Add average date of census when missing
  DATE <- df[, .(date = mean(date, na.rm = T)), by = .(year, quadrat)][order(year)]
  DATE[, "year2" := c(year[2:length(year)], NA), by = quadrat] # year of previous census
  DATE[, "ID1" := paste(year, quadrat, sep = "-")]
  DATE[, "ID2" := paste(year2, quadrat, sep = "-")]
  DF2[, "ID" := paste(DF2$year, DF2$quadrat, sep = "-")]
  DF2 <- within(DF2, date1[is.na(date1)] <- DATE$date[match(DF2[is.na(date1), ID], DATE$ID2)])
  DF2 <- within(DF2, date2[is.na(date2)] <- DATE$date[match(DF2[is.na(date2), ID], DATE$ID1)])

  if (any(is.na(DF2$date1))) {
  DATE2 <- df[, .(date = mean(date, na.rm = T)), by = .(year)][order(year)]
  DATE2[, "year2" := c(year[2:length(year)], NA)] # year of previous census
  DF2 <- within(DF2, date1[is.na(date1)] <- DATE2$date[match(DF2[is.na(date1), year], DATE2$year2)])
  }
  DF2$int <- (DF2$date2 - DF2$date1) / 365.5 # census interval in days

  # Update status for recruited trees
  DF2[, nrow := seq_len(.N), by = treeID]
  DF2 <- within(DF2, status1[is.na(status1) & !is.na(dbh2) & nrow == 1] <- "P") # recruited trees or Z code (=?)
  DF2[, nrow := NULL]

  # Assign status
  DF2[, c("agbl", "code", "dHOM") := assign_status(.SD), by = treeID]

  # Remove unnecessary rows
  DF2[code == "D", nrow := seq_len(.N), by = treeID]
  DF2 <- within(DF2, nrow[is.na(nrow)] <- 0)
  DF2 <- DF2[nrow < 2, ] # keeps only 1 line for dead trees
  DF2[, "nrow" := NULL]

  # Compute annualized fluxes
  # annual prod for alive trees & multi-stems
  DF2[code %in% c("A", "AC"), prod.g := (agb2 - agb1) / int, by = treeID]
  # annual prod for alive trees & multi-stems
  DF2[code == "B", prod.g := ifelse(agb1.surv!=0,(agb2 - agb1.surv) / int,0), by = treeID]
  # annual prod for resprouts and recruits
  DF2[code %in% c("R", "Rsp"), prod.r := agb2 / int, by = treeID]
  DF2[code == "D", agbl := agb1, by = treeID]
  DF2[,loss := agbl / int, by = treeID] # annualized loss for dead trees

  # Process multiple stems trees:
  DF2[,"dN":=Nstem2 -Nstem1]
  # allocate agb to losses if nstem2 - nstem1 <0
  DF2 <- within(DF2,loss[dN<0 & Nstem1>1 & code%in% c("A", "AC")] <- abs(prod.g[dN<0 & Nstem1>1 & code%in% c("A", "AC")]))
  DF2 <- within(DF2,prod.g[dN<0 & Nstem1>1 & code%in% c("A", "AC")] <- 0 )
  # allocate agb to resprout if nstem2 - nstem1 > 0
  DF2 <- within(DF2,prod.r[dN>0 & Nstem1>1 & code%in% c("A", "AC")] <- prod.g[dN>0 & Nstem1>1 & code%in% c("A", "AC")])
  DF2 <- within(DF2,prod.g[dN>0 & Nstem1>1 & code%in% c("A", "AC")] <- 0 )

  # Flag large strangler figs
  if (flag_stranglers) {
    DF2$ficus <- 0
    ficus$name <- paste(ficus$Genus, ficus$Species, sep = " ")
    FIC <- match(DF2$name, ficus$name[ficus$Strangler == "Yes"])
    if (!exists("dbh_stranglers")) {
      assign("dbh_stranglers", 500)
    }
    DF2 <- within(DF2, ficus[!is.na(FIC) & dbhc1 > dbh_stranglers] <- 1)
  }
  DF2[, c("agbl", "agb1.surv","broken") := NULL]
  DF2
}

#' Flag major errors.
#'
#' Identify trees with major errors in DBH measurments. A major error correspond
#' to a relative individal productivity (or growth) is above a given percentage
#' (set by 'maxrel') of the mean productivity computed at a site. Additionnaly,
#' flagged trees that died at next census interval are also flagged. Option to
#' see DBH measurement (=draw.graph) of flagged trees or print a csv
#' (output_errors) are given.
#'
#' @param DF A data.table.
#' @param site Provide the full name of your site (in lower case) i.e. 'barro
#'   colorado island'.
#' @param flag_stranglers, `TRUE` or `FALSE`, if `TRUE`  strangler figs tree are
#'   flagged (upon a list to published soon).
#' @param max_prod A value (between 0 and 1) corresponding the percentage over which relative
#'   productivity is assumed to be too high. Relative productivity correspond to the ratio between tree productivity and the mean productivity across quadrats whit consistent measure over time (i.e. quadrats with changes in POM are excluded).
#' @param output_errors `TRUE` or `FALSE`, output all records for trees with
#'   major errors in productivity to a csv file.
#' @param exclude_interval A vector (i.e. c(1,2)) indicating if a set of census
#'   intervals must be discarded from computation due for instance to a change
#'   in  protocol of measurment.
#'
#' @return A data.table (data.frame) with all relevant variables.
#' @keywords internal
#' @noRd
flag_errors <- function(DF,
                        site,
                        flag_stranglers,
                        max_prod,
                        output_errors,
                        exclude_interval) {
  if(is.null(max_prod)){
    stop("You should specify a maximum relative productivity (max_prod)")
  }
  mean.prod <- determine_mean_prod(DF, flag_stranglers, exclude_interval)
  DF[, prod.rel := as.numeric(NA), ]
  # relative contribution to average total productivity
  DF[, prod.rel := prod.g/ mean.prod]
  DF[, error := 0]
  DF <- within(DF, error[prod.rel > max_prod & dHOM == 0 & code != "D"] <- 1)
  DF <- within(DF, error[prod.rel < (-max_prod) & dHOM == 0 & code != "D"] <- -1)

  # Error with recruited or resprout:
  DF <- within(DF, error[prod.r > 0.5 & dHOM == 0 & code=="R"] <- 1)

  # Flag census after a major error
  POSI <- DF[, .I[error != 0] + 1, ]
  POSI2 <- DF[POSI, .I[error == 0 & dHOM == 0 & code == "D"], ]
  DF[, error.loss := 0]
  DF <- within(DF, error.loss[POSI[POSI2]] <- 1) # flag consecutive census

  if (flag_stranglers) {
    # flag dead strangler figs
    DF <- within(DF, error.loss[ficus == 1 & code == "D"] <- 1)
  }
  # ID <- DF[error!=0 & !code%in%c("D","R"),nrow(.SD)>=1,by=treeID]
  # ID <- ID[V1==T,treeID]
  ID <- unique(DF[error != 0, treeID])
  A <- utils::menu(
    c("Y", "N"),
    title = paste(
      "There are",length(ID),
      "trees with errors. Do you want to print",
      round(length(ID)/15),"pages?"
    )
  )

  ifelse(A == 1, graph_problem_trees <- T, graph_problem_trees <- F)

  if (graph_problem_trees) { # Plot trees with large major error
    YEAR <- levels(factor(DF$year))
    CX <- 2
    a <- 0
    i <- 0

    # TODO: Growing an object can be terribly slow. Instead you should create
    # an object with the structure that you need (rows, columns, list elements,
    # etc.)
    GRAPH <- list()

    for (n in 1:length(ID)) {
      i <- i + 1
      DF[, year := as.numeric(year)]
      DF$dbh1 <- round(DF$dbh1, 1)
      DF[, "y1" := year - round(int)]
      aa <- ID[n]
      X <- DF[treeID == aa & !code %in% c("R")][order(year)]
      X$point <- 0
      X$point[X$error != 0] <- 1
      Y <- DF[treeID == aa & !code %in% c("D", "R")][order(year)]
      YY <- Y[, .(year = max(year), name = unique(name), d2 = round(dbhc2[year == max(year)], 1), d02 = round(dbh2[year == max(year)], 1), hom2 = round(hom2[year == max(year)], 2), y1 = unique(y1)), by = treeID]
      Y$line <- 0
      Y$line[Y$dHOM == 0] <- 1
      Y$point <- 0
      Y$point[Y$error != 0] <- 1

      GRAPH[[i]] <- ggplot2::ggplot(X, aes(x = y1, y = dbhc1)) +
        geom_point(size = 2) +
        geom_segment(data = Y,
          aes(x = y1, y = dbhc1, xend = year, yend = dbhc2, linetype = as.factor(line))
        ) +
        geom_point(data = X[point == 1], aes(x = year, y = dbhc2), col = 2) +
        labs(title = paste0(unique(X$name), " (", ID[n], ")"), x = " ", y = "dbh (mm)") +
        geom_text(data = Y,
          aes(x = y1, y = dbh1 - (0.05 * dbh1)), label = round(Y$hom1, 2), cex = CX
        ) +
        geom_text(data = YY,
          aes(x = year, y = d02 - (0.05 * d02)), label = round(YY$hom2, 2), cex = CX) +
        geom_text(data = Y,
          aes(x = year, y = 0.3 * max(dbhc2)), label = Y$dbh1, cex = CX,
          angle = 90, vjust = 1
        ) +
        geom_text(data = YY,
          aes(x = year, y = 0.3 * max(d2)),
          label = YY$d02, cex = CX, angle = 90, vjust = 1
        ) +
        theme(
          plot.title = element_text(size = 5 * CX, face = "bold"),
          axis.title.y = element_text(size = 5 * CX, , face = "bold"),
          axis.text.y = element_text(size = 4 * CX),
          axis.text.x = element_text(size = 4 * CX, vjust = 0, angle = 30),
          panel.background = element_blank(),
          strip.text = element_text(size = 4 * CX, face = "bold"),
          strip.background = element_rect("lightgrey"),
          panel.spacing = unit(0.1, "lines")
        ) +
        scale_linetype_manual(values = c("0" = "dashed", "1" = "solid")) +
        guides(linetype = F, colour = F) +
        scale_x_continuous(
          limits = c(min(as.numeric(YEAR)) - 3, max(as.numeric(YEAR)) + 3),
          breaks = as.numeric(YEAR)
        ) +
        scale_y_continuous(limits = c(0.2 * max(YY$d2), max(X$dbhc2, X$dbhc1)))

      if (i %% 15 == 0) { ## print 15 plots per page
        a <- a + 1
        ggplot2::ggsave(
          do.call(gridExtra::grid.arrange, GRAPH),
          file = paste0(getwd(), "/output/trees_with_major_errors_", a, ".pdf"),
          width = 29.7, height = 20.1, units = "cm"
        )

        GRAPH <- list() # reset plot
        i <- 0 # reset index
      }
    }
  } # end of graph

  if (length(ID) == 0) {
    message(
      paste0(
        "No tree productivity above", maxrel, "% or below", -maxrel,
        "% of mean productivity at your plot. You may eventually want to try a lower threshold."
      )
    )
  }

  if (output_errors & length(ID) > 0) {
    utils::write.csv(
      DF[treeID %in% ID],
      file = paste0(getwd(), "/output/trees_with_major_errors.csv")
    )
  }

  # TODO: Functions should generally either return a plot or an object (e.g. DF)
  # You may need to brake down this function in two pieces, e.g. flag_errors(),
  # which computes something and returns DF, and plot_flags(), which plots DF.
  # You can create a class "flags" and write an S3 method for plot() -- i.e.
  # plot.flags() -- so you get the plot with `plot(flag_errors())`.
  DF[, c("prod.rel","ID") := NULL]

  message("Step 5: Errors flagged. Saving formated data into 'data' folder.")
  DF
} # end of major.error

#' Set mean productivity.
#'
#' Set mean productivity (Mg/ha/yr) across all census intervals for a given
#' site.
#'
#' @param DF A data.table.
#' @param site Provide the full name of your site (in lower case) i.e. 'barro
#'   colorado island'.
#' @param strangler `TRUE` or `FALSE`, if `TRUE`  strangler figs tree are
#'   flagged (upon a list to published soon).
#' @param exclude_interval A vector (i.e. c(1,2)) indicating if a set of census
#'   intervals must be discarded from computation due for instance to a change
#'   in  protocol of measurment.
#'
#' @return Mean productivity in Mg/ha/yr.
#' @keywords internal
#' @noRd
determine_mean_prod <- function(DF, flag_stranglers, exclude_interval) {
  if (!"quad"%in%names(DF)){
  grid.size=10
  area =(grid.size^2)/(10000)
  DF <- create_quad(DF,grid.size,x="gx",y="gy",fit.in.plot=T)
  }
  if (missing(exclude_interval)) {
    # TODO: Clarify what's going on here. You may need to extract functions and
    # write variables with meaningful names.
    ifelse(flag_stranglers,
    PRODA  <- DF[ficus != 1,.(prod=sum(prod.g,na.rm=T)/area,dHOM=ifelse(any(abs(dHOM)>0.1,na.rm=T),1,0)),by=c("quad","year")][order(year)],
    PRODA  <- DF[,.(prod=sum(prod.g,na.rm=T)/area,dHOM=ifelse(any(abs(dHOM)>0.1,na.rm=T),1,0)),by=c("quad","year")][order(year)])
    } else {
    ifelse(flag_stranglers,
    PRODA  <- DF[ficus != 1 & !interval%in%c(exclude_interval),.(prod=sum(prod.g,na.rm=T)/area,dHOM=ifelse(any(abs(dHOM)>0.1,na.rm=T),1,0)),by=c("quad","year")][order(year)],
    PRODA  <- DF[!interval%in%c(exclude_interval),.(prod=sum(prod.g,na.rm=T)/area,dHOM=ifelse(any(abs(dHOM)>0.1,na.rm=T),1,0)),by=c("quad","year")][order(year)])
    }

  mPROD <- PRODA[dHOM==0,mean(prod)]

  mPROD
}

#' Loess.
#'
#' A wrapper to get smoothed predictions of AGB fluxes using a loess function
#' (library 'locfit').
#'
#' @param x A data.table.
#' @param var The name of the variable to be smoothed again intial AGB.
#' @param range The range of initial AGB to be used for prediction (i.e. 5th and
#'   95th percentiles of the whole distribution).
#'
#' @return A smoothed prediction of the variable of interest.
#' @keywords internal
#' @noRd
loess.fun <- function(x, var, range, weights = NULL) {
  if (is.null(weights)) {
    fit <- locfit(var ~ lAGB, data = x)
  } else {
    fit <- locfit(var ~ lAGB, data = x, weights = weights)
  }

  pred <- stats::predict(fit, newdata = list(lAGB = range))
  data.frame(lAGB = range, y = as.numeric(pred))
}

# loess.fun <- function(x,var)  {
# 	fit <- locfit(var ~ lAGB, data=x)
# 	Xrange = as.numeric(quantile(x,seq(0.01,1,.01),na.rm=T)) # 100 values to estimate the smooth spline
# 	pred <- stats::predict(fit,newdata=list(lAGB=Xrange))
# 	return(data.frame(lAGB=Xrange,y=as.numeric(pred)))
# }

#' Normalized tree status.
#'
#' Check the consistency of stem/tree status over time (i.e. a tree that is
#' 'alive' at last census can not be 'dead' inbetween).
#'
#' @param x A data.table.
#'
#' @return A data.table (data.frame) with a new colum "status1" where values can
#'   be "P"(prior),"A"(alive),"D"(dead) and "Dr"(dead replicated).
#' @keywords internal
#' @noRd
check_status <- function(DT) {
  # TODO: These is too deeply nested and prone to error. Consider using
  # switch(), dplyr::case_when, extracting functions, etc.
  # See https://rstd.io/code-smells
  if (!"status" %in% names(DT)) {
    DT$status <- NA
    DT$status[DT$DFstatus == "alive"] <- "A"
    DT$status[DT$DFstatus == "dead"] <- "D"
  }

  STAT <- rep("A", nrow(DT))

  if (all(is.na(DT$dbh))) {
    STAT <- rep("Dr", nrow(DT))
  } else if (any(is.na(DT$dbh)) | any(grep("\\bD\\b", DT$status))) { # look for dbh=NA or dead (D) status
    locA <- which(DT$status == "A" & !is.na(DT$dbh))
    locD <- which(DT$status != "A" & is.na(DT$dbh))

    if (length(locA) != 0) {
      if (min(locA) > 1) {
        STAT[is.na(DT$dbh)][1:min(locA) - 1] <- "P"
      } # Prior (to be discarded)
      if (max(locA) < nrow(DT)) {
        STAT[(max(locA) + 1)] <- "D"
      }
      if (max(locA) + 2 <= nrow(DT)) {
        STAT[(max(locA) + 2):nrow(DT)] <- "Dr" # Dead replicated (to be discarded)
      }
      # # Broken stem/resprout
      # if (length(locD)>0 & locD<nrow(DT)) {
      # STAT[locD] <- "B"
      # STAT[locD+1] <- "Rsp"
      # }
    } # end of loca>0
  }

  STAT
}


# FIXME: LOAD() does not load objects in the global environment. It loads them
# into an environment, `env`, created inside your funciton. In any case, I think
# you should not need to load anything -- instead your fuctions should get the
# data they need via arguments. The user can pass datasests in a list. And you
# could write a helper to load from a directory into a list. The outcome is the
# same but your functions become more focused, easier to build and to
# understand. These are all different environments:
# * Global environment.
# * Functions environment.
# * Package environment (or namespace).
# You don't need to get into this trouble.

#' Load object.
#'
#' A wrapper to softly load R objects in the Global environment.
#'
#' @param saveFile The path to the object to be loaded.
#'
#' @return Import the object.
#' @keywords internal
#' @noRd
LOAD <- function(saveFile) {
  env <- new.env()
  load(saveFile, envir = env)
  loadedObjects <- objects(env, all = TRUE)
  stopifnot(length(loadedObjects) == 1)
  env[[loadedObjects]]
}



# # FIXME: Can we remove this?
# # # Debug
# DT <- DF2[treeID==391]
# DT <- DF2[treeID== 230867 ][order(year)]
# DT <- DF2[treeID==5637]
# assign_status(DT)

#' Assign status.
#'
#' Assign status alive ("A"), alive with POM changed ("AC"), dead ("D"),
#' recruited ("R") or resprout ("Rsp") to trees, and check for consistency over
#' time (i.e. avoid resurrection).
#'
#' @param DF A data.table.
#'
#' @return Update the column 'status1' with consistent information.
#' @keywords internal
#' @noRd
assign_status <- function(DT) {
  code <- rep("A", nrow(DT))
  agbl <- DT$agbl
  code[code == "A" & is.na(DT$dbhc1) & DT$status1 == "P"] <- "R"
  code[code == "A" & DT$broken == 1] <- "B" # Broken
  code[code == "A" & DT$code2 == "R"] <- "Rsp" #
  # resprouted trees poses a problem only the first year, are alive/dead after
  code[code == "A" & is.na(DT$status1) & !is.na(DT$status2)] <- "Rsp"
  # resprouted trees poses a problem only the first year, are alive/dead after
  code[code == "A" & grep("MAIN", DT$code1) & grep("SPROUT", DT$code2)] <- "Rsp"
  if(is.na(utils::tail(DT$status2, 1))) {
    DT <- within(DT,status2[nrow(DT)] <- "D") }
  if (utils::tail(DT$status2, 1) == "D") {
    code[length(code)] <- "D"
  }
  if (any(code == "A")) {
    loc <- max(which(code %in% c("A", "Rsp")))
    if (any(code[1:loc] == "D")) {
      loc2 <- which(code[1:loc] == "D")
      code[loc2] <- "B"
      agbl[loc2] <- DT$agb1[loc2]
    }
  }
  dHOM <- DT$hom2 - DT$hom1
  dHOM[is.na(dHOM)] <- 0
  # trees with POM changed are not accounted for in productivity
  code[code == "A" & dHOM != 0] <- "AC"

  list(agbl, code, dHOM)
}



#' Create quadrats.
#'
#' Create a grid where all trees are allocated to a given quadrat of size (=
#' grid size).
#'
#' @param census A data.frame where trees have relative X-Y coordinates.
#' @param grid_size The size of the grid (in meter).
#' @param x The identifier of X coordinates (i.e. 'gx').
#' @param y The identifier of Y coordinates (i.e. 'gy').
#' @param make.squared Does the coordinates needs to.
#'
#' @return Add three columns to the data.frame: quadrat's number, centroids X
#'   and Y.
#' @keywords internal
#' @noRd
create_quad <- function(census, grid_size, x = "gx", y = "gy", fit.in.plot) {
  X <- census[, grep(x, names(census)), with = F][[1]]
  Y <- census[, grep(y, names(census)), with = F][[1]]

  if (any(is.na(X)) | any(is.na(Y))) {
    warning(
      paste(length(X[is.na(X)]), " trees without coordinates were discarded."),
      call. = FALSE
    )
    census <- census[!is.na(X) & !is.na(Y)]
    X <- census[, grep(x, names(census)), with = F][[1]]
    Y <- census[, grep(y, names(census)), with = F][[1]]
  }
  minx <- 0
  miny <- 0
  maxx <- max(X, na.rm = T)
  maxy <- max(Y, na.rm = T)

  if (exists("fit.in.plot")) {
    maxx <- round(max(X) / 10) * 10
    maxy <- round(max(Y) / 10) * 10
  }
  x1 <- X
  x1[X <= 0] <- 0.1
  x1[X >= maxx] <- maxx - 0.1
  y1 <- Y
  y1[Y <= 0] <- 0.1
  y1[Y >= maxy] <- maxy - 0.1

  # specify grid size for division into quadrats
  w_grid <- ceiling(maxx) / grid_size
  h_grid <- ceiling(maxy/10)*10 / grid_size
  n_quadrat <- w_grid * h_grid

  # for now, only allow grid sizes that fit neatly
  if (max(x1, na.rm = T) > maxx | min(x1, na.rm = T) < 0 | max(y1, na.rm = T) > maxy | min(y1, na.rm = T) < 0) {
    stop(
      "Some trees are outside the plot boundaries. Consider fit.in.plot=T",
      call. = FALSE
    )
  }
  if (round(w_grid) != w_grid | round(h_grid) != h_grid) {
    stop("Plot width and height must be divisible by grid_size", call. = FALSE)
  }

  census$quad <- (ceiling((maxy - miny) / grid_size)) * (floor((x1 - minx) / grid_size)) + (floor((y1 - miny) / grid_size) + 1) ## identifiant de cellule unique 1->100 en partant de 0,0
  if (max(census$quad, na.rm = T) != n_quadrat) {
    # TODO: stop() pastes stuff. So you can avoid paste(). Also, see paste0()
    # which defaults to `sep = ""`.
    stop(
        "Quadrat numbering error: expected ", n_quadrat, " quadrats; got ",
        max(census$quad, na.rm = T), sep = "")

  }
  census$centroX <- (floor(x1 / grid_size) * grid_size) + (grid_size / 2)
  census$centroY <- (floor(y1 / grid_size) * grid_size) + (grid_size / 2)
  # census[,.(max(gx)-min(gx),max(gy)-min(gy)),by=quad]

  census
}

#' Compute unbiased recruitment flux and loss flux.
#'
#' `rec_flux()` and `loss_flux()` compute unbiased recruitment flux and loss
#' flux, respectively.
#'
#' @param A0 Biomass at initial census.
#' @param A1 Biomass of alive trees at final census.
#' @param S1 Initial biomass of individuals that survived to time t.
#' @param time Cenusus interval in year.
#'
#' @return Absolute recruitment flux in % per year.
#' @keywords internal
#' @noRd
rec_flux <- function(A0, A1, S1, area, time) {
  rec <- log(A1 / S1) * (A1 - A0) / (area * time * log(A1 / A0))
  ifelse(rec == Inf, 0, rec)

  rec
}

#' @rdname rec_flux
#' @keywords internal
#' @noRd
loss_flux <- function(A0, A1, S1, area, time) {
  LO <- (log(A0 / S1) / log(A1 / A0)) * ((A1 - A0) / (area * time))
  # FIXME: This line achieves nothing unless you assign it to `LO`. It sholud be:
  # LO <- ifelse(LO == Inf, 0, LO)
  ifelse(LO == Inf, 0, LO)

  LO
}

trim_growth <- function(cens,
                        slope = 0.006214,
                        intercept = .9036,
                        err.limit = 4,
                        maxgrow = 75,
                        dbhunit = "mm") {
  if (dbhunit == "cm") intercept <- intercept / 10
  stdev.dbh1 <- slope * cens$dbhc1 + intercept

  growth <- (cens$dbhc2 - cens$dbhc1) / cens$int

  bad.neggrow <- which(cens$dbhc2 <= (cens$dbhc1 - err.limit * stdev.dbh1))
  bad.posgrow <- which(growth > maxgrow)

  accept <- rep(TRUE, length(cens$dbhc1))
  accept[bad.neggrow] <- FALSE
  accept[bad.posgrow] <- FALSE
  accept[is.na(growth)] <- FALSE

  accept[is.na(cens$dbhc1) | is.na(cens$dbhc2) | cens$dbhc2 <= 0] <- FALSE
  growth[!accept] <- NA

  growth
}

#' Create a dataframe with one row full of missing values.
#'
#' @param .names String giving the names of the dataframe to create.
#'
#' @return A dataframe.
#' @keywords internal
#' @noRd
receiving_df <- function(.names) {
  na <- as.list(rep(NA, length(.names)))
  stats::setNames(data.frame(na), .names)
}
