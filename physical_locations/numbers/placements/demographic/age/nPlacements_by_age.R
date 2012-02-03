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

nPlacements_by_age <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="indcs", host="sandbox.in.mycasebook.org", port="5432",dbname="casebook2_sandbox");
  png("nPlacements_by_age.png");
  max_age <- 18;
  bucket_size_age <- 3;
  n_age_buckets <-  ceiling(max_age/bucket_size_age);
  plots_per_row <- 2;
  n_rows_of_plots <- ceiling(n_age_buckets/plots_per_row);
  par(mfrow=c(n_rows_of_plots, plots_per_row));

  for (i in 1:n_age_buckets)
  {
    start_age <- (i-1)*bucket_size_age;
    end_age <- min(i*bucket_size_age - 1, max_age);
    statement <- paste("select people.id, ",
    " count(physical_location_records.id) as nPlacements", 
                     " from people, physical_location_records",
                     " where people.id = physical_location_records.person_id",
                     " and physical_location_records.physical_location_type = 'PhysicalLocation::Placement'",
                     " and fn_get_date_first_assessment(people.id) is not NULL",
                     " and people.date_of_birth is not NULL",
    " and extract(year from age(fn_get_date_first_assessment(people.id), ", 
                                 "people.date_of_birth)) between ",
                      start_age, " and ", end_age,
                     " group by people.id " ,
                    sep = "");
    res <- dbSendQuery(con, statement);
    data <- fetch(res, n = -1);
    edges <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50);
    histogram <- hist(data[,2], breaks = edges, plot = FALSE);
    customHistogram(histogram = histogram, 
         mainTitle = paste("Age [", start_age, "-", end_age, "], ", nrow(data), "children", sep = " "),
         xLabel = "Number of placements", yLabel = "Fraction of children",
         fivenum(data[,2]));
  }
  dbDisconnect(con);
  dev.off();
}

