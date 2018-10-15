#' Borrowed from BIOMASS to do little changes that allow using this function.
#'
#' For example, the original funciton has this code: `data(<dataset>, envir =
#' environment())` which really makes our life unnecesarily diffucult. Why not
#' make `<dataset>` internal data and use it directly?
#'
#' @keywords internal
#' @noRd
getTaxonomy <- function(genus, findOrder = FALSE)
{
  ### Find the family (and the order) of a vector of genus

  ################## 1. Retrieve the Family
  # Create ids
  inputGenus <- data.frame(id = 1:length(genus), inputGenus = as.character(genus),
                           stringsAsFactors = FALSE)

  # Merge the input genera with the genus family table
  # `genusFamily` is internal data.
  genusFam <- unique(merge(inputGenus, genusFamily, by.x = "inputGenus", by.y = "genus", all.x = TRUE))

  # Sort data by id
  genusFam <- genusFam[order(genusFam$id),]

  ################## 2. Retrieve the Order

  if(findOrder == TRUE)
  {
    tmp <- unique(genusFam[, c("inputGenus", "family")])
    # `apgFamilies` is internal data
    tmpOrder <- unique(merge(tmp, apgFamilies, by.x = "family", by.y = "famAPG", all.x = TRUE))

    for(f in unique(tmpOrder$family))
      genusFam$order[genusFam$family %in% f] <- unique(tmpOrder$order[tmpOrder$family %in% f])
  }

  genusFam <- genusFam[order(genusFam$id), ]
  genusFam$id <- NULL
  return(genusFam)
}
