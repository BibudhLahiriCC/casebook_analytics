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
  subTitle <- paste("Min = ", fiveNumberSummary[1],
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

n_visits_distribution_15_day_bucket <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="indcs", host="sandbox.in.mycasebook.org", port="5432",dbname="casebook2_sandbox");
  png("n_visits_distribution_15_day_bucket.png");
  par(mfrow=c(2, 1));
  statement <- paste("select b.person_id, current_date - max(date(contacts.occurred_at))", 
   " from physical_location_records, physical_location_placements, contact_people a,", 
   " contact_people b, relationships, relationship_types, contacts",
   " where physical_location_records.person_id = b.person_id",
   " and physical_location_records.physical_location_id = physical_location_placements.id",
   " and physical_location_records.physical_location_type = 'PhysicalLocation::Placement'",
   " and date(physical_location_placements.start_date) <= current_date",
   " and (date(physical_location_placements.end_date) is NULL or date(physical_location_placements.end_date) > current_date)",
   " and a.contact_id = b.contact_id and a.person_id <> b.person_id",
   " and a.person_id = relationships.strong_side_person_id and b.person_id = relationships.weak_side_person_id",
   " and relationships.type_id = relationship_types.id and relationship_types.strong_name = 'parent' and relationship_types.weak_name = 'child'",
   " and a.contact_id = contacts.id",
   " group by b.person_id",
   " having (current_date - max(date(contacts.occurred_at))) <= 200",
              sep = "");
  cat(statement);
  res <- dbSendQuery(con, statement);
  data <- fetch(res, n = -1);
  edges <- c(0, 15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180, 195, 210);
  histogram <- hist(data[,2], breaks = edges, plot = FALSE);
  #str(histogram);
  customHistogram(histogram = histogram, 
         mainTitle = paste("#days since last visit with parent for", 
                           nrow(data), "children", sep = " "),
         xLabel = "#days since last visit", yLabel = "Fraction of children",
         fivenum(data[,2]));
  boxplot(x = data[,2], outline = FALSE);
  dbDisconnect(con);
  dev.off();
}

