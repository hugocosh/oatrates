
crudeRate <- function(inputDataFrame, ratePer = 1000, confLevel = 95) {

  #=============================================================================================
  # Calculates crude rates and their confidence intervals for Numerators and Denominators in a dataframe
  # using the 'new' crude rate CI guidance. Where:
  # Numerator >=3 to <10 events use Chi squared exact method
  # Numerator >=10 events use Byar's method
  # Crude rates and confidence intervals are suppressed if the number of events <3 or the Numerator or Denominator contain NA values.
  #
  # Inputs:
  # inputDataFrame    Data frame to process. The data frame must contain column names 'Numerator' and 'Denominator' for the function to work.
  # ratePer           Rate per 100, 1000, 10000, 100000 or 1000000 (1000 is default)
  # confeLvel         Confidence level - 90, 95 or 99 (95 is default)
  #
  # Outputs:
  # outputDataFrame  Data frame with crude rates
  #=============================================================================================
  # library(magrittr) # Have tested loading in this package so that pipes ( %>% ) work.

  usethis::use_pipe()
  # Before doing analysis, need to do a series of checks on whether the data is in the correct format.
  # This includes whether the columns are named correctly

  # Make a second brief function to test whether columns are 'not in' the dataset
  # Code taken from https://stackoverflow.com/questions/5831794/opposite-of-in
  '%!in%' <- function(x,y)!('%in%'(x,y))

  # Check column names
  if("Numerator" %!in% colnames(inputDataFrame)) stop("Dataframe does not contain the column 'Numerator'. Please check your spelling")
  if("Denominator" %!in% colnames(inputDataFrame)) stop("Dataframe does not contain the column 'Denominator'. Please check your spelling")

  # Check format of Numerator and Denominator columns
  if(!is.numeric(inputDataFrame$Numerator)) stop("Numerator column is not in a numeric format")
  if(!is.numeric(inputDataFrame$Denominator)) stop("Denominator column is not in a numeric format")

  # Also need to add in stops for incorrect values for ratePer and confLevel
  if(confLevel %!in% c(90, 95, 99)) stop("confLevel has not been set to a valid value. Allowed confLevel values are 90, 95 and 99.")
  if(ratePer %!in% c(100, 1000, 10000, 100000, 1000000)) stop("ratePer has not been set to a valid value. Allowed ratePer values are 100, 1000, 10000, 100000 and 1000000.")

  # Warning messages for invalid values, NAs in Numerator or denomoniator, 0s in Denominator, negative values
  if(any(is.na(inputDataFrame$Numerator))) warning("Numerator contains one or more rows with a value of NA")
  if(any(is.na(inputDataFrame$Denominator) | inputDataFrame$Denominator == 0)) warning("Denominator contains one or more rows with a value of 0 or NA")

  # The following warning messages don't seem to work
  #if(any(inputDataFrame$Numerator) < 0) warning("Numerator contains one or more rows with a negative value")
  #if(any(inputDataFrame$Denominator < 0)) warning("Denominator contains one or more rows with a negative value")

  # Helpful messages to inform user of ratePer and confLevel values used
  message("The crude rate has been calculated as a rate per ", ratePer, ". The default value is set to 1000.")
  message("The confidence intervals have been calculated for the ", confLevel, "% level. The default value is set to 95.")

  # Calculate crude rate
  test <- dplyr::mutate(inputDataFrame, crudeRate = Numerator/Denominator * ratePer)

  # Calculate a (alpha) and z based on confLevel

  a <- 1 - (confLevel/100)
  z <- qnorm(1 -(a/2))

  # Byar's method

  test <- dplyr::mutate(test, XlByar = Numerator * (1 - 1/(9 * Numerator) - z/(3 * sqrt(Numerator))) ^ 3
                        , XuByar = (Numerator + 1) * (1 - 1/(9 * (Numerator + 1)) + z/(3 * sqrt(Numerator + 1))) ^ 3)

  test <- dplyr::mutate(test, LCLByar = (XlByar/Denominator) * ratePer
                  ,UCLByar = (XuByar/Denominator) * ratePer)

  # Chi squared exact method

  #chi <- qchisq(p = (1 - (0.05/2)), df = (2 * inputDataFrame$Numerator), lower.tail = FALSE)

  test <- dplyr::mutate(test, XlChi = qchisq(p = (1 - (a/2)), df = (2 * Numerator), lower.tail = FALSE) / 2
                        , XuChi = qchisq(p = (1 - (a/2)), df = (2 * Numerator) + 2) / 2)

  test <- dplyr::mutate(test, LCLChi = (XlChi/Denominator) * ratePer
                        ,UCLChi = (XuChi/Denominator) * ratePer)

  # Now select method depending on Numerator
  test <- dplyr::mutate(test, LCL = dplyr::case_when(dplyr::between(Numerator, 3,9) ~ LCLChi
                                        , Numerator >= 10 ~ LCLByar
                                        , TRUE ~ 9999)
                        , UCL = dplyr::case_when(dplyr::between(Numerator, 3,9) ~ UCLChi
                                          , Numerator >= 10 ~ UCLByar
                                          , TRUE ~ 9999)
                        , method = dplyr::case_when(is.na(Numerator) | is.na(Denominator) ~ 'Suppress'
                                                    , Numerator < 3 ~ 'Suppress'
                                             , dplyr::between(Numerator, 3,9) ~ 'Chi'
                                             , Numerator >= 10 ~ 'Byar'
                                             , TRUE ~ 'Suppress'))

  # And drop any columns that have been used in the creation of the CIs
  outputDataFrame <- dplyr::select(test, -c(XlByar, XuByar, LCLByar, UCLByar, XlChi, XuChi, LCLChi, UCLChi))

  # Update suppressed data with NAs as required
  outputDataFrame <- dplyr::mutate(outputDataFrame, crudeRate = dplyr::case_when(LCL == 9999 ~ 9999
                                                                                 , TRUE ~ crudeRate)
                                   , LCL = dplyr::na_if(LCL, 9999)
                                   , UCL = dplyr::na_if(UCL, 9999)) %>%
    dplyr::mutate(crudeRate = dplyr::na_if(crudeRate, 9999))

  return(outputDataFrame)
}


# Now test the function out on a sample data frame

Test <- data.frame(Numerator = -1:14, Denominator = 1000:1015)

Test[14,2] <- NA
Test[13,1] <- 2


crudeRate(Test, 10000, 95)




















