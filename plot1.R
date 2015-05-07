## This function draws a frequency histogram of the Global Active Power
## between the 1st and 2nd of February 2007.

plot1 <- function(){
    ## Check that data.table package is installed. If not, install it
    if (!require("data.table")) {
        install.packages("data.table")
    }
    ## Load the libraries
    library(data.table)
    library(graphics)
    library(grDevices)
    ## Read the household power consumption file only from 1/2/2007 to 2/2/2007
    input <- "household_power_consumption.txt"
    items_vector <- grep("1/2/2007", readLines(input)) ## find 1/2/2007 in the file
    items_vector <- items_vector - 1 ## N - 1 to include first item
    first_item <- min (items_vector)
    n_items <- 60 * 24 * 2 ## 60 minutes 24 hours 2 days
    output <- fread(input, sep =";", header = TRUE, skip = first_item -1, na.strings = "?", nrows = n_items)
    cnames <- as.vector(unlist(fread(input, nrows = 1, header = FALSE)))
    suppressWarnings(colnames(output) <- cnames)
    ## Draw the plot in plot1.png file
    png("plot1.png")
    hist(output$Global_active_power, xlab = "Global Active Power (kilowatts)", 
         col = "red", main = "Global Active Power")
    dev.off()
}