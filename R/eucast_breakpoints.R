#' Determine EUCAST susceptibility
#'
#' This function given a data.frame with MIC will add a column to this table
#' giving the corresponding EUCAST susceptibility (Sensitive, Intermediate or Resistant)
#'
#' @param MIC.table data.frame having MIC in one column.
#' @param species.column ID of the column containing the Species name recognized names are "A. baumannii", "P. aeruginosa", 
#' "E. coli" and "K. pneumoniae"
#' @param MIC.column ID of the column containing the MIC values in MIC.table. Defaults to 2
#' @param ATB.column ID of the column containing the ATB names in MIC.table. Defaults to 1
#' @keywords EUCAST sensitivity, breakpoints
#' @export
#' @examples
#' eucast_breakpoints(MIC.table)
eucast_breakpoints <-
  function(MIC.table,
           species.column = 1,
           MIC.column = 4,
           ATB.column = 3) {
    EUCAST.susceptibility <- NULL
    result <- MIC.table
    #By default the species is unrecognized
    result[, (length(MIC.table) + 1)] <- "Unrecognized species"
    
    for (i in 1:nrow(MIC.table))
    {
      if (MIC.table[i, species.column] == "A. baumannii")
      {
        if (sum(eucast.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column]) == 0)
        {
          result[i, (length(MIC.table) + 1)] <-
            "Antibiotic not found in EUCAST tables"
        }
        else if (is.na(eucast.breakpoints.acinetobacter[eucast.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column], 2]))
        {
          result[i, (length(MIC.table) + 1)] <- "No cutoff set by EUCAST"
        }
        else
        {
          MIC.value <-
            unlist(MIC.table[i, MIC.column], use.names = F)
          for (j in 1:length(MIC.value))
          {
            if (sum(
              grepl("<=", MIC.value[j]),
              grepl(">", MIC.value[j]),
              grepl("Positive control problem", MIC.value[j]),
              grepl("Negative control problem", MIC.value[j])
            ) > 0)
            {
              EUCAST.susceptibility[j] <- "No MIC value"
            }
            
            if (sum(
              grepl("<=", MIC.value[j]),
              grepl(">", MIC.value[j]),
              grepl("Positive control problem", MIC.value[j]),
              grepl("Negative control problem", MIC.value[j])
            ) == 0)
            {
              temp.MIC <- 0
              temp.MIC <-
                as.numeric(MIC.value[j])
              if (temp.MIC <= eucast.breakpoints.acinetobacter[(eucast.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column]), 2])
              {
                EUCAST.susceptibility[j] <- "Susceptible"
                
              }
              else if (temp.MIC > eucast.breakpoints.acinetobacter[eucast.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column], 3])
              {
                EUCAST.susceptibility[j] <- "Resistant"
              }
              else if (temp.MIC <= eucast.breakpoints.acinetobacter[eucast.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column], 3] &
                       temp.MIC > eucast.breakpoints.acinetobacter[eucast.breakpoints.acinetobacter[, 1] == MIC.table[i, ATB.column], 2])
              {
                EUCAST.susceptibility[j] <- "Intermediate"
              }
            }
          }
          result[i, (length(MIC.table) + 1)] <-
            paste0(EUCAST.susceptibility)
        }
      }
      if (MIC.table[i, species.column] == "P. aeruginosa")
      {
        if (sum(eucast.breakpoints.pseudomonas[, 1] == MIC.table[i, ATB.column]) == 0)
        {
          result[i, (length(MIC.table) + 1)] <-
            "Antibiotic not found in EUCAST tables"
        }
        else if (is.na(eucast.breakpoints.pseudomonas[eucast.breakpoints.pseudomonas[, 1] == MIC.table[i, ATB.column], 2]))
        {
          result[i, (length(MIC.table) + 1)] <- "No cutoff set by EUCAST"
        }
        else
        {
          MIC.value <-
            unlist(MIC.table[i, MIC.column], use.names = F)
          for (j in 1:length(MIC.value))
          {
            if (sum(
              grepl("<=", MIC.value[j]),
              grepl(">", MIC.value[j]),
              grepl("Positive control problem", MIC.value[j]),
              grepl("Negative control problem", MIC.value[j])
            ) > 0)
            {
              EUCAST.susceptibility[j] <- "No MIC value"
            }
            
            if (sum(
              grepl("<=", MIC.value[j]),
              grepl(">", MIC.value[j]),
              grepl("Positive control problem", MIC.value[j]),
              grepl("Negative control problem", MIC.value[j])
            ) == 0)
            {
              temp.MIC <- 0
              temp.MIC <-
                as.numeric(MIC.value[j])
              if (temp.MIC <= eucast.breakpoints.pseudomonas[(eucast.breakpoints.pseudomonas[, 1] == MIC.table[i, ATB.column]), 2])
              {
                EUCAST.susceptibility[j] <- "Susceptible"
                
              }
              else if (temp.MIC > eucast.breakpoints.pseudomonas[eucast.breakpoints.pseudomonas[, 1] == MIC.table[i, ATB.column], 3])
              {
                EUCAST.susceptibility[j] <- "Resistant"
              }
              else if (temp.MIC <= eucast.breakpoints.pseudomonas[eucast.breakpoints.pseudomonas[, 1] == MIC.table[i, ATB.column], 3] &
                       temp.MIC > eucast.breakpoints.pseudomonas[eucast.breakpoints.pseudomonas[, 1] == MIC.table[i, ATB.column], 2])
              {
                EUCAST.susceptibility[j] <- "Intermediate"
              }
            }
          }
          result[i, (length(MIC.table) + 1)] <-
            paste0(EUCAST.susceptibility)
        }
      }
      enterobacteriaceae <- c("E. coli", "K. pneumoniae")
      if (MIC.table[i, species.column] %in% enterobacteriaceae)
      {
        if (sum(eucast.breakpoints.enterobacteriaceae[, 1] == MIC.table[i, ATB.column]) == 0)
        {
          result[i, (length(MIC.table) + 1)] <-
            "Antibiotic not found in EUCAST tables"
        }
        else if (is.na(eucast.breakpoints.enterobacteriaceae[eucast.breakpoints.enterobacteriaceae[, 1] == MIC.table[i, ATB.column], 2]))
        {
          result[i, (length(MIC.table) + 1)] <- "No cutoff set by EUCAST"
        }
        else
        {
          MIC.value <-
            unlist(MIC.table[i, MIC.column], use.names = F)
          for (j in 1:length(MIC.value))
          {
            if (sum(
              grepl("<=", MIC.value[j]),
              grepl(">", MIC.value[j]),
              grepl("Positive control problem", MIC.value[j]),
              grepl("Negative control problem", MIC.value[j])
            ) > 0)
            {
              EUCAST.susceptibility[j] <- "No MIC value"
            }
            
            if (sum(
              grepl("<=", MIC.value[j]),
              grepl(">", MIC.value[j]),
              grepl("Positive control problem", MIC.value[j]),
              grepl("Negative control problem", MIC.value[j])
            ) == 0)
            {
              temp.MIC <- 0
              temp.MIC <-
                as.numeric(MIC.value[j])
              if (temp.MIC <= eucast.breakpoints.enterobacteriaceae[(eucast.breakpoints.enterobacteriaceae[, 1] == MIC.table[i, ATB.column]), 2])
              {
                EUCAST.susceptibility[j] <- "Susceptible"
                
              }
              else if (temp.MIC > eucast.breakpoints.enterobacteriaceae[eucast.breakpoints.enterobacteriaceae[, 1] == MIC.table[i, ATB.column], 3])
              {
                EUCAST.susceptibility[j] <- "Resistant"
              }
              else if (temp.MIC <= eucast.breakpoints.enterobacteriaceae[eucast.breakpoints.enterobacteriaceae[, 1] == MIC.table[i, ATB.column], 3] &
                       temp.MIC > eucast.breakpoints.enterobacteriaceae[eucast.breakpoints.enterobacteriaceae[, 1] == MIC.table[i, ATB.column], 2])
              {
                EUCAST.susceptibility[j] <- "Intermediate"
              }
            }
          }
          result[i, (length(MIC.table) + 1)] <-
            paste0(EUCAST.susceptibility)
        }
      }
      
    }
    colnames(result)[(length(MIC.table) + 1)] <-
      "EUCAST susceptibility"
    return(result)
  }
