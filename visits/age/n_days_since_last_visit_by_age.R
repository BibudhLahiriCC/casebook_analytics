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
          beside = TRUE, horiz = FALSE, space = 0, axisnames = TRUE, cex.names = 1.5,
          cex.axis = 1.5,
          names.arg = barLabels);
  title(main = mainTitle, cex.main = 1.5, font.main = 9, col.main= "blue",
         sub = subTitle, cex.sub = 1.5, font.sub = 9, col.sub = "red",
         xlab = xLabel,
         ylab = yLabel, cex.lab = 1.5);
  #axis(1, cex.axis = 2);
  #axis(2, cex.axis = 2);
}


n_days_since_last_visit_by_age <- function(queryPoint)
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="indcs", host="sandbox.in.mycasebook.org", 
                   port="5432",dbname="casebook2_sandbox");
  png(filename = "n_days_since_last_visit_by_age.png",
      width = 840, height = 960);
  par(mar=c(5, 4, 4, 2) + 0.1);
  par(oma = c(3,3,3,3));
  par(mfrow=c(5, 2));
  age_conditions <- c(" between 0 and 1",
                      " between 2 and 5",
                      " between 6 and 12",
                      " between 13 and 17",
                      " >= 18");
  for (i in 1:5){
   statement <- paste(
   "select b.person_id, '", queryPoint, "' - max(date(contacts.occurred_at))", 
   " from physical_location_records, out_of_home_locations, ",
   " contact_people b, contacts, people", 
   " where physical_location_records.person_id = b.person_id",
   " and physical_location_records.physical_location_id = out_of_home_locations.id",
   " and physical_location_records.physical_location_type = out_of_home_locations.type",
   " and physical_location_records.physical_location_type = 'PhysicalLocation::Placement'",
   " and date(out_of_home_locations.start_date) <= '", queryPoint, "'",
   " and (date(out_of_home_locations.end_date) is NULL ",
   " or date(out_of_home_locations.end_date) > '", queryPoint, "')",
   " and date(contacts.occurred_at) <= '", queryPoint, "'",
   " and contacts.mode like 'Face to Face%'",
   " and contacts.id = b.contact_id",
   " and b.person_id = people.id",
   " and people.date_of_birth is not null",
   " and extract(year from age(current_date, people.date_of_birth))",
     age_conditions[i], 
   " group by b.person_id",                     
              sep = "");
   #str(statement);
   res <- dbSendQuery(con, statement);
   data <- fetch(res, n = -1);
   cat(paste(" age ", age_conditions[i], 
             ", #rows = ", nrow(data), "\n", sep = ""));
   edges <- c(0, 6, 12, 18, 24, 30, 6000);
   histogram <- hist(data[,2], breaks = edges, plot = FALSE);
   #str(histogram);
   customHistogram(histogram = histogram, 
        mainTitle = paste("for age", 
                           age_conditions[i], ",",
                           nrow(data), "children, as on", queryPoint, sep = " "),
         xLabel = "#days since last visit", yLabel = "Fraction of children",
         fivenum(data[,2]),
         queryPoint);
   boxplot(x = data[,2], outline = FALSE, horizontal = TRUE, cex.names = 1.5,
          cex.axis = 1.5);
  }
  dbDisconnect(con);
  dev.off();
}

