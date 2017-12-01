# -------------------------------------------------------------------------------------------
# Purpose: This script contains the constants that are used by functions or are commonly used
# by the exec scripts assoicated with this package.
#
# Created by: Dorheim, Kalyn
# Created on: December 1 2017
# Modified:   xxx
#
# Notes:
#
# Graphing Constants -----------------------------------------------------------------------

# Define my fav figure settings, often used inside graphing functions and in exec code
MY_SETTINGS <- ggplot2::theme(text = ggplot2::element_text(size = 18)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))


# Month Name -------------------------------------------------------------------------------

# A dataframe containing the month number and month name, often used to add the month_name
# column to a data frame.
MONTH_NAME <- data.frame(month = 1:12, month_name = c("Jan", "Feb", "Mar", "Apr", "May",
                                                      "Jun", "Jul", "Aug", "Sep", "Oct",
                                                      "Nov", "Dec"))
