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


nRunaways <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="indcs", host="sandbox.in.mycasebook.org", port="5432",dbname="casebook2_sandbox");
  genders <- c("Female", "Male");
  png("nRunaways_by_gender.png");
  par(mfrow=c(2, 1));
  for (i in 1:2)
  {
     gender <- genders[i];
     statement <- paste("SELECT person_id, count(physical_location_id) as nRunaways", 
                       " from physical_location_records, people",
                       " where physical_location_records.person_id = people.id",
                       " and physical_location_records.physical_location_type = 'PhysicalLocation::Runaway'",
                       " and people.gender = '", gender, "'",
                       " group by person_id", 
                       sep = "");
     res <- dbSendQuery(con, statement);
     data <- fetch(res, n = -1);
     edges <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50);
     histogram <- hist(data[,2], breaks = edges, plot = FALSE);
      customHistogram(histogram, 
            mainTitle = paste(nrow(data), gender, "children", sep = " "),
           xLabel = "Number of runaways", 
           yLabel = "Fraction of children"
         );
  }
  dbDisconnect(con);
  dev.off();
}

