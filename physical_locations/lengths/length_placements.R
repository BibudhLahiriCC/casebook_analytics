customHistogram <- function(histogram, mainTitle, xLabel,
                            yLabel, fiveNumberSummary)
{
  #If there are n bars in the histogram, then 
  #histogram$breaks is an array of (n+1) points, including
  #the start-point of first bucket and last point of last bucket.
  #histogram$counts is an array of n numbers.
  nBars <- length(histogram$counts);
  totalFreq <- sum(histogram$counts);
  heights <- histogram$counts/totalFreq;
  widths <- c();
  barLabels <- c();
  width <- histogram$mids[2] - histogram$mids[1];
  #cat(paste("nBars = ", nBars, "\n"));
  for (i in 1:nBars)
  {
    widths[i] <- width;
    barLabels[i] <- as.character(histogram$breaks[i+1]);
  }
   subTitle <- paste("Min = ", fiveNumberSummary[1],
                    ", Q1 = ", fiveNumberSummary[2],
                    ", Median = ", fiveNumberSummary[3],
                    ", Q3 = ", fiveNumberSummary[4],
                    ", Max = ", fiveNumberSummary[5], sep = "");
  barplot(height = heights, width = widths, xlim = c(0, max(histogram$breaks)),
          beside = TRUE, horiz = FALSE, main = mainTitle, xlab = xLabel,
          ylab = yLabel, space = 0, axisnames = TRUE, names.arg = barLabels, 
          sub = subTitle
          );
}

length_placements <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="indcs", host="sandbox.in.mycasebook.org", port="5432",dbname="casebook2_sandbox");
  png("length_placements.png");

  statement <- paste("select ceiling((date(end_date) - ",
              " date(start_date))::real/(30::real))",
      " from people, physical_location_records, physical_location_placements",
      " where people.id = physical_location_records.person_id",
      " and physical_location_records.physical_location_id = physical_location_placements.id",
      " and physical_location_records.physical_location_type = 'PhysicalLocation::Placement'",
      " and physical_location_placements.start_date is not NULL",
      " and physical_location_placements.end_date is not NULL",
      " and physical_location_placements.end_date > physical_location_placements.start_date",
      " and ceiling((date(end_date) - date(start_date))::real/(30::real)) <= 60",
                    sep = "");
    res <- dbSendQuery(con, statement);
    data <- fetch(res, n = -1);
    edges <- c(0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60);
    histogram <- hist(data[,1], breaks = edges, plot = FALSE);
    customHistogram(histogram = histogram, 
         mainTitle = "Histogram for length of stay at placements",
         xLabel = "Length of stay", yLabel = "Fraction of children",
         fivenum(data[,1]));
  dbDisconnect(con);
  dev.off();
}

