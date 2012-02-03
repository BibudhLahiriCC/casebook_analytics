customHistogram <- function(histogram, mainTitle, xLabel,
                            yLabel)
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
  barplot(height = heights, width = widths, xlim = c(0, max(histogram$breaks)),
          beside = TRUE, horiz = FALSE, main = mainTitle, xlab = xLabel,
          ylab = yLabel, space = 0, axisnames = TRUE, names.arg = barLabels, 
          );
}


nTHVs <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="indcs", host="sandbox.in.mycasebook.org", port="5432",dbname="casebook2_sandbox");
  ethnicities <- c("american_indian", "asian", "black", "white");
  #ethnicities <- c("white");
  plots_per_row <- 2;
  n_ethnic_groups <- length(ethnicities);
  n_rows_of_plots <- ceiling(n_ethnic_groups/plots_per_row);
  png("nTHVs_by_ethnicity.png");
  par(mfrow=c(n_rows_of_plots, plots_per_row));
  for (i in 1:n_rows_of_plots)
  {
    if ((i == n_rows_of_plots) &  (n_ethnic_groups%%plots_per_row > 0))
      end_column_index = min(plots_per_row, n_ethnic_groups%%plots_per_row)
    else
      end_column_index = plots_per_row;
    for (j in 1:end_column_index)
    {
     ethnicity <- ethnicities[(i-1)*plots_per_row + j];
     statement <- paste("SELECT person_id, count(physical_location_id) as nTrialHomeVisits", 
                       " from physical_location_records, people",
                       " where physical_location_records.person_id = people.id",
                       " and physical_location_records.physical_location_type = 'PhysicalLocation::TrialHomeVisit'",
                       " and people.", ethnicity, " = true",
                       " group by person_id", 
                       sep = "");
     res <- dbSendQuery(con, statement);
     data <- fetch(res, n = -1);
     edges <- c(0, 2, 4, 6, 8, 10, 12);
     histogram <- hist(data[,2], breaks = edges, plot = FALSE);
     #require(utils);
     #str(histogram);
     customHistogram(histogram = histogram, 
         mainTitle = paste(nrow(data), ethnicity, "children", sep = " "),
         xLabel = "Number of THVs", yLabel = "Fraction of children");
    }
  }
  dbDisconnect(con);
  dev.off();
}

