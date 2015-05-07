## This function draws a line chart of the 3 levels of sub metering
## between the 1st and 2nd of February 2007.

plot3 <- function(){
    ## Check that data.table and chron packages are installed. If not, install them
    if (!require("data.table")) {
        install.packages("data.table")
    }
    if (!require("chron")) {
        install.packages("chron")
    }
    ## Load the libraries
    library(data.table)
    library(chron)
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
    ## Generate a time vector combining Date and Time
    Time <- chron(data$Date,data$Time, format = c(dates = "d/m/y", 
                                                  times = "h:m:s"))
    ## Determine where the labels "Thu" "Fri" and "Sat" should be
    Lab1 <- min(Time)
    Lab2 <- Lab1 + 1
    Lab3 <- max(Time)
    ## Draw the plot in plot3.png file
    png("plot3.png")
    plot(Time,data$Sub_metering_1, type = "l", 
         ylab = "Energy sub metering", xlab = "", xaxt = 'n')
    axis(1,c(Lab1,Lab2,Lab3),labels = c("Thu", "Fri", "Sat"))
    lines(Time,data$Sub_metering_2, type = "l", col = "red")
    lines(Time,data$Sub_metering_3, type = "l", col = "blue")
    legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
           col = c("black","red","blue"), lwd = 1)
    dev.off()
}