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

age_distribution <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="indcs", host="sandbox.in.mycasebook.org", port="5432",dbname="casebook2_sandbox");
  png("age_distribution.png");
  statement <- paste("select people.id,", 
    "extract(year from age(fn_get_date_first_assessment(people.id),", 
              "people.date_of_birth))", 
                     " from people, allegations",
                     " where allegations.victim_id = people.id",
                     " and people.date_of_birth is not NULL",
                     " and fn_get_date_first_assessment(people.id) is not NULL",
  " and extract(year from age(fn_get_date_first_assessment(people.id), ",
  " people.date_of_birth)) <= 18",
                       sep = "");
  #str(statement);
  res <- dbSendQuery(con, statement);
  data <- fetch(res, n = -1);
  #edges <- c(0, 3, 6, 9, 12, 15, 18);
  edges <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18);
  histogram <- hist(data[,2], breaks = edges, plot = FALSE);
  #require(utils);
  #str(histogram);
  customHistogram(histogram = histogram, 
         mainTitle = paste("Age distribution of", 
                           nrow(data), "children", sep = " "),
         xLabel = "Age (in years)", yLabel = "Fraction of children",
         fivenum(data[,2]));
  dbDisconnect(con);
  dev.off();
}

