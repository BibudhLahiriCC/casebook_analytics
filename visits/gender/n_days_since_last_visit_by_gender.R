customHistogram <- function(histogram, mainTitle, xLabel,
                            yLabel, fiveNumberSummary, queryPoint)
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
  xAxisRightEnd <- max(histogram$breaks);
  width <- ceiling(xAxisRightEnd/nBars);
  for (i in 1:nBars)
  {
    widths[i] <- width;
    #barLabels[i] <- as.character(histogram$breaks[i+1]);
    leftPoint <- 0;
    if (i > 1)
    {
      leftPoint <- histogram$breaks[i] + 1; 
    }
    barLabels[i] <- paste(as.character(leftPoint),
                          "-",
                          as.character(histogram$breaks[i+1]));
  }
  subTitle <- paste("As of ", queryPoint, ", min = ", fiveNumberSummary[1],
                    ", Q1 = ", fiveNumberSummary[2],
                    ", Median = ", fiveNumberSummary[3],
                    ", Q3 = ", fiveNumberSummary[4],
                    ", Max = ", fiveNumberSummary[5], sep = "");
  barplot(height = heights, width = widths, xlim = c(0, xAxisRightEnd),
          beside = TRUE, horiz = FALSE, main = mainTitle, xlab = xLabel,
          ylab = yLabel, space = 0, axisnames = TRUE, cex.names = 0.8, 
          names.arg = barLabels, 
          sub = subTitle
          );
}


n_days_since_last_visit_by_gender <- function(queryPoint)
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="indcs", host="sandbox.in.mycasebook.org", 
                   port="5432",dbname="casebook2_sandbox");
  png("n_days_since_last_visit_by_gender.png");
  par(mar=c(2.0, 2.0, 1.0, 1.0));
  #par(cin=c(8.5,11));
  par(mfrow=c(2, 2));
  gender_conditions <- c("Male",
                      "Female");
  for (i in 1:2){
   statement <- paste(
   "select b.person_id, '", queryPoint, "' - max(date(contacts.occurred_at))", 
   " from physical_location_records, physical_location_placements, ",
   " contact_people b, contacts, people", 
   " where physical_location_records.person_id = b.person_id",
   " and physical_location_records.physical_location_id = physical_location_placements.id",
   " and physical_location_records.physical_location_type = 'PhysicalLocation::Placement'",
   " and date(physical_location_placements.start_date) <= '", queryPoint, "'",
   " and (date(physical_location_placements.end_date) is NULL ",
   " or date(physical_location_placements.end_date) > '", queryPoint, "')",
   " and date(contacts.occurred_at) <= '", queryPoint, "'",
   " and contacts.mode like 'Face to Face%'",
   " and contacts.id = b.contact_id",
   " and b.person_id = people.id",
   " and people.gender = '", gender_conditions[i], "'",
   " group by b.person_id",                     
              sep = "");
   #str(statement);
   res <- dbSendQuery(con, statement);
   data <- fetch(res, n = -1);
   cat(paste(" gender ", gender_conditions[i], 
             ", #rows = ", nrow(data), "\n", sep = ""));
   edges <- c(0, 6, 12, 18, 24, 30, 6000);
   histogram <- hist(data[,2], breaks = edges, plot = FALSE);
   #str(histogram);
   customHistogram(histogram = histogram, 
         mainTitle = paste("#days since last F2F for", 
                           nrow(data),  gender_conditions[i], "children", sep = " "),
         xLabel = "#days since last visit", yLabel = "Fraction of children",
         fivenum(data[,2]),
         queryPoint);
   boxplot(x = data[,2], outline = FALSE, horizontal = TRUE);
  }
  dbDisconnect(con);
  dev.off();
}

