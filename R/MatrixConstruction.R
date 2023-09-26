#' Matrix Construction
#'
#' This function creates a Matrix ID using the 2023 Matrix
#' Assignment Logic utilizing the dominate condition for matrix labels.
#'
#' MatrixCreation Function
#'
#' This function converts stand data from the SILC format into a matrix for use in forest growth and yield models.
#'
#' This function creates a Matrix ID using the 2023 Matrix
#' Assignment Logic utilizing the dominate condition for matrix labels
#'
#' This includes OS strata for RY, AW, and AE, the elimination Of CS and CH strata
#' replaced by a unified C strata, and the elimination of C strata in RY where they
#' are lumped with the other OS species.
#'
#' @param District The SILC District where the stand is located.
#' @param StandID A unique identifier for the stand.
#' @param OSIZE The overstory stem size class (1 = regenerating, 2 = pre-commercial, 3 = pole, 4 = sawlog).
#' @param ODEN The overstory density. (A, B, C, or D)
#' @param OSPP1 The overstory species 1. Valid species codes are in details
#' @param OSPP2 The overstory species 2 (if present). Valid species codes are the same as for OSPP1.
#' @param USIZE The understory stem size class (1 = regenerating, 2 = pre-commercial, 3 = pole, 4 = sawlog).
#' @param UDEN The understory density (A, B, C, or D)
#' @param USPP1 The understory species 1. Valid species codes are the same as for OSPP1.
#' @param USPP2 The understory species 2 (if present). Valid species codes are the same as for OSPP1.
#' @param PCT The year the stand received pre-commercial thinning treatment.
#' @param HighElevation A logical indicating whether the stand is at a high elevation.
#' @param ACRES The number of acres in the stand.
#'
#' @details The Size classes can be 1 (regenerating), 2 (pre-commercial), 3 (pole), or 4 (sawlog). The valid species codes for OSPP1 and OSPP2 (overstory species) and USPP1 and USPP2 (understory species) are as follows:
#' - "BA": Black Ash
#' - "BC": Black Cherry
#' - "BE": Beech
#' - "BI": Bigtooth aspen
#' - "BP": Black spruce
#' - "CE": Cedar
#' - "BF": Balsam fir
#' - "BS": Black spruce
#' - "HE": Hemlock
#' - "IH": Intolerant Hardwoods
#' - "MM": Mountain maple
#' - "NS": Norway Spruce
#' - "OH": Other Hardwoods
#' - "OS": Other softwoods
#' - "PO": Aspen
#' - "RP": Red pine
#' - "RM": Red maple
#' - "RO": Red Oak
#' - "RS": Red Spruce
#' - "SF": Spruce-Fir
#' - "SM": Sugar maple
#' - "TA": Tamarack
#' - "TH": Tolerant Hardwood
#' - "WA": White ash
#' - "WB": White birch
#' - "WP": White pine
#' - "WS": White spruce
#' - "YB": Yellow birch
#'
#' @return A matrix containing the stand data in a format suitable for use in forest growth and yield models.
#'
#' @examples
#' MatrixCreation("RY", "Stand1", 4, "B", "BF", "OS", 2, "C", "BA", "BS", 1999, TRUE, 29)
#'
#'@returns Matrix Assignment for Stands Using Overstory and Understory Observations
#'@family SILC Functions
#'@export


# Matrix Creation Function ------------------------------------------------

MatrixCreation <- function(District, StandID,
                                  OSIZE, ODEN, OSPP1, OSPP2,
                                  USIZE, UDEN, USPP1, USPP2,
                                  PCT,
                                  HighElevation = FALSE, ACRES = NA){


  # Establish Overstory Broadtype -------------------------------------------


  data <- data.frame(District, StandID,
                     OSIZE, ODEN, OSPP1, OSPP2,
                     USIZE, UDEN, USPP1, USPP2, PCT,
                     HighElevation, ACRES)

  # Lookup table
  lookup_table <- tibble(
    species = c("BC", "BE", "BI", "BP", "IH", "MM", "OH", "PO", "RM", "RO", "SM", "TH", "WA", "WB", "YB",
                "CE",
                "BF", "BS", "NS", "RS", "SF", "WS",
                "HE", "OS", "RP", "TA", "WP"),
    group = c(rep("H", 15),
              "C",
              rep("S", 6),
              rep("OS", 5))
  )

  species <- lookup_table$species
  group <- lookup_table$group

  apply_lookup <- function(column) {
    if (any(is.na(column))) {
      return(NA)
    } else {
      return(group[match(column, species)])
    }
  }

  data <- data %>%
    mutate(
      o_broad1 = apply_lookup(OSPP1),
      o_broad2 = apply_lookup(OSPP2),
      u_broad1 = apply_lookup(USPP1),
      u_broad2 = apply_lookup(USPP2)
    )

  data <- data %>% mutate(
    o_broad = case_when(
      o_broad1 == "S" & o_broad2 == "S" ~ "S",
      o_broad1 == "S" & o_broad2 == "OS" ~ "S",
      o_broad1 == "S" & o_broad2 == "C" ~ "S",
      o_broad1 == "S" & o_broad2 == "H" ~ "SH",
      o_broad1 == "H" & o_broad2 == "H" ~ "H",
      o_broad1 == "H" & o_broad2 == "S" ~ "HS",
      o_broad1 == "H" & o_broad2 == "C" ~ "HS",
      o_broad1 == "H" & o_broad2 == "OS" ~"HS",
      o_broad1 == "OS" ~ "OS",
      o_broad1 == "C" ~ "C",
      o_broad1 == "S" & is.na(o_broad2) == TRUE ~ "S",
      o_broad1 == "H" & is.na(o_broad2) == TRUE ~ "H",
      o_broad1 == "OS" & is.na(o_broad2) == TRUE ~ "OS",
      o_broad1 == "C" & is.na(o_broad2) == TRUE ~ "C",
      TRUE ~ NA_character_
    )
  )

  data <- data %>% mutate(
    u_broad = case_when(
      u_broad1 == "S" & u_broad2 == "S"  ~ "S",
      u_broad1 == "S" & u_broad2 == "OS" ~ "S",
      u_broad1 == "S" & u_broad2 == "C"  ~ "S",
      u_broad1 == "S" & u_broad2 == "H"  ~ "SH",
      u_broad1 == "H" & u_broad2 == "H"  ~ "H",
      u_broad1 == "H" & u_broad2 == "S"  ~ "HS",
      u_broad1 == "H" & u_broad2 == "C"  ~ "HS",
      u_broad1 == "H" & u_broad2 == "OS" ~ "HS",
      u_broad1 == "OS" ~ "OS",
      u_broad1 == "C" ~ "C",
      u_broad1 == "S" & is.na(u_broad2) == TRUE ~ "S",
      u_broad1 == "H" & is.na(u_broad2) == TRUE ~ "H",
      u_broad1 == "OS" & is.na(u_broad2) == TRUE ~ "OS",
      u_broad1 == "C" & is.na(u_broad2) == TRUE ~ "C",
      TRUE ~ NA_character_
    )
  )

  data$OSD <- paste(data$OSIZE, data$ODEN, sep = "")
  data$USD <- paste(data$USIZE, data$UDEN, sep = "")

  data <- data %>% mutate(
    MSD = case_when(
      OSD == "4B" & USD == "3A" ~ "3A",
      OSD == "4C" & USD == "3A" ~ "4A",
      OSD == "4C" & USD == "3B" ~ "3B",
      OSD == "4D" & USD == "3A" ~ "3A",
      OSD == "4D" & USD == "3B" ~ "3B",
      OSD == "4D" & USD == "3C" ~ "3C",
      OSD == "4D" & USD == "2A" ~ "2A",
      OSD == "4D" & USD == "1A" ~ "1A",
      OSD == "3D" & USD == "2A" ~ "2A",
      OSD == "3D" & USD == "1A" ~ "1A",
      OSD == "2D" & USD == "1A" ~ "1A",
      TRUE ~ OSD
    )
  )

  data <- data %>% mutate(
    broadtype = case_when(
      OSD == "4B" & USD == "3A" ~ u_broad,
      OSD == "4C" & USD == "3A" ~ u_broad,
      OSD == "4C" & USD == "3B" ~ u_broad,
      OSD == "4D" & USD == "3A" ~ u_broad,
      OSD == "4D" & USD == "3B" ~ u_broad,
      OSD == "4D" & USD == "3C" ~ u_broad,
      OSD == "4D" & USD == "2A" ~ u_broad,
      OSD == "4D" & USD == "1A" ~ u_broad,
      OSD == "3D" & USD == "2A" ~ u_broad,
      OSD == "3D" & USD == "1A" ~ u_broad,
      OSD == "2D" & USD == "1A" ~ u_broad,
      TRUE ~ o_broad
    )
  )


  data <- data %>% mutate(
    broadtype = case_when(
      District == "RY" & broadtype == "C" ~ "OS",
      District == "SP" & broadtype == "OS" ~ "S",
      TRUE ~ broadtype
    )
  )


  data$Matrix <- paste0(data$broadtype, data$MSD, sep = "")

  data <- data %>% mutate(
    Matrix = case_when(
      Matrix == "OS4D" & USPP1 =="RS" & USD == "1B" ~ "S1B",
      Matrix == "OS4D" & USPP1 =="SF" & USD == "1B" ~ "S1B",
      Matrix == "OS4D" & USPP1 =="BF" & USD == "1B" ~ "S1B",
      Matrix == "OS4D" & USPP1 =="RS" & USD == "2B" ~ "S2B",
      Matrix == "OS4D" & USPP1 =="SF" & USD == "2B" ~ "S2B",
      Matrix == "OS4D" & USPP1 =="BF" & USD == "2B" ~ "S2B",
      TRUE ~ Matrix
    )
  )

  data <- data %>%
    mutate(Matrix = ifelse(broadtype %in% c("S", "OS", "HS", "H", "SH") &
                             (is.numeric(PCT) & PCT > 0 | is.logical(PCT) & PCT),
                           paste(Matrix, "-P", sep = ""),
                           paste(Matrix, "-N", sep = ""))) %>%
    mutate(PCT = ifelse(PCT > 0, TRUE, FALSE))

  data$Matrix <- ifelse(data$HighElevation == TRUE & data$District == "RY",
                        paste(data$Matrix, "-H", sep = ""),
                        data$Matrix)

  if (any(is.na(data$Matrix))) {
    warning("There are NAs in the Matrix column.")
  }
  if (any(is.na(data$broadtype))) {
    warning("There are NAs in the broadtype column.")
  }
  if (any(is.na(data$MSD))) {
    warning("There are NAs in the MSD column.")
  }

  data <- data %>%
    select(District, StandID, MSD, broadtype, Matrix, ACRES) %>%
    rename(SizeDensity = "MSD", NewBroad = "broadtype",
           NewMatrix = "Matrix")

  return(data)

}











