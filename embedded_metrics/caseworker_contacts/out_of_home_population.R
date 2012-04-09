customHistogram <- function(histogram, mainTitle, xLabel,
                            yLabel, fiveNumberSummary, 
                            queryPoint = as.character(Sys.Date()))
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
  subTitle <- paste("min = ", fiveNumberSummary[1],
                    ", Q1 = ", fiveNumberSummary[2],
                    ", Median = ", fiveNumberSummary[3],
                    ", Q3 = ", fiveNumberSummary[4],
                    ", Max = ", fiveNumberSummary[5], sep = "");
  barplot(height = heights, width = widths, xlim = c(0, xAxisRightEnd),
          beside = TRUE, horiz = FALSE, main = mainTitle, xlab = xLabel,
          ylab = yLabel, cex.lab = 1.5, space = 0, axisnames = TRUE, cex.names = 1.5,
          cex.axis = 1.5, cex.main = 1.5, font.main = 9, col.main= "blue",
          names.arg = barLabels, 
          sub = subTitle, cex.sub = 1.5, font.sub = 9, col.sub = "red"
          );
}

resolve_race <- function(american_indian, asian, black, pacific_islander, white,
                         multi_racial)
{
  race <- "";
  if (isTRUE(multi_racial))
  {
   race <- "multi_racial";
  }
  else if (isTRUE(american_indian))
  {  
   race <- "american_indian";
  }
  else if (isTRUE(asian))
  {  
   race <- "asian";
  }
  else if (isTRUE(black))
  {  
   race <- "black";
  }
  else if (isTRUE(white))
  {  
   race <- "white";
  }
  else 
  {  
   race <- "unknown";
  }
  return(race);
}

out_of_home_population <- function(queryPoint)
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", host="mirror.in.mycasebook.org", 
                   port="5432", dbname="casebook2_mirror");
  statement <- paste("select p.id person_id, r.type,",
                     "to_char(p.date_of_birth, 'YYYY-MM-DD') date_of_birth,",
                     "p.gender, p.american_indian, p.asian, p.black,",
                     "p.pacific_islander, p.white, p.multi_racial, p.hispanic_or_latino_origin, cn.name,",
                      "to_char(c.occurred_at, 'YYYY-MM-DD') last_visit_date,", 
                      "current_date - date(c.occurred_at) days_since_last_visit,",
                      "cn.name county_for_case, r.provider_type",
                    "from removal_locations r, people p, contacts c, contact_people cp,",
                    "case_plan_focus_children cpfc, case_plans cl, cases cs, counties cn",
                    "where r.type not in ('RemovalLocation::TrialHomeVisit', 'PhysicalLocation::Runaway')",
                    "and r.person_id = p.id",
                    "and r.start_date is not null",
                    "and (r.end_date is null or date(r.end_date) > current_date)",
                    "and (cp.person_id = p.id)",
                    "and (cp.contact_id = c.id)",
                    "and c.mode like 'Face to Face%'",
                    "and cpfc.person_id = p.id",
                    "and cpfc.case_plan_id = cl.id",
                    "and cl.case_id = cs.id",
                    "and cs.county_id = cn.id",
                    "order by p.id, last_visit_date desc",
                    sep = " ");
  #cat(statement);
  res <- dbSendQuery(con, statement);
  data <- fetch(res, n = -1);
  #Aliases in SQL become column names in R data frame

  #For each child, keep the last visit date only, and the number of days since 
  #that date.
  child_id <- 0;
  rows_fetched <- nrow(data);
  output <- data.frame();
  today <- Sys.Date();

  for (i in 1:rows_fetched)
  {
    if (data[i, "person_id"] != child_id)
    {
      #New child begins
      child_id <- data[i, "person_id"];
      row <- data[i, ];
      output <- rbind(output, row);
    }
  }
  output["age_rounded_in_years"] <- 
    round(as.numeric(difftime(today, as.POSIXlt(output[, "date_of_birth"], 
                                     format="%Y-%m-%d"), units = c("days")))/365);
                         
  output$race <- apply(output, 1, 
        function(row) resolve_race(as.logical(row["american_indian"]),
                                   as.logical(row["asian"]),
                                   as.logical(row["black"]),
                                   as.logical(row["pacific_islander"]),
                                   as.logical(row["white"]),
                                   as.logical(row["multi_racial"])));
  edges <- c(0, 6, 12, 18, 24, 30, 6000);
  genders = unique(output[, "gender"]);
  n_genders = length(genders);

  counties = unique(output[, "county_for_case"]);
  n_counties = length(counties);
  cat(paste("n_counties = ", n_counties, "\n", sep = ""));

  races = unique(output[, "race"]);
  n_races = length(races);
  cat(paste("n_races = ", n_races, "\n", sep = ""));

  #Age buckets are 0-1, 2-5, 6-12, 13-16, 17, 18+
  n_age_buckets = 6;

  n_rows_of_plots = n_genders + n_counties + n_races 
  #+ n_age_buckets
  ;
  
  png("out_of_home_population.png", 
      width = 840, height = 960, units = "px"
      );
  par(mfrow=c(n_rows_of_plots, 2));

  for (i in 1:n_genders)
  {
    cat(paste("gender = ", genders[i], "\n", sep = " "));
    filtered_output <- subset(output, (gender == genders[i]));
    cat(paste("size of filtered_output = ", nrow(filtered_output), 
              "\n", sep = " "));
    histogram <- hist(filtered_output[,"days_since_last_visit"], 
                      breaks = edges, plot = FALSE);
    customHistogram(histogram = histogram,
        mainTitle = paste("for gender",
                           genders[i], ",",
                           nrow(filtered_output), "children", sep = " "),
         xLabel = "#days since last visit", yLabel = "Fraction of children",
         fiveNumberSummary = fivenum(filtered_output[,"days_since_last_visit"]));
    boxplot(x = filtered_output[,"days_since_last_visit"], 
           outline = FALSE, horizontal = TRUE, cex.names = 1.5,
          cex.axis = 1.5);
  }


  for (i in 1:n_counties)
  {
    cat(paste("county = ", counties[i], "\n", sep = " "));
    filtered_output <- subset(output, (county_for_case == counties[i]));
    cat(paste("size of filtered_output = ", nrow(filtered_output), "\n", 
              sep = " "));
    histogram <- hist(filtered_output[,"days_since_last_visit"], 
                      breaks = edges, plot = FALSE);
    customHistogram(histogram = histogram,
        mainTitle = paste("for county",
                           counties[i], ",",
                           nrow(filtered_output), "children", sep = " "),
         xLabel = "#days since last visit", yLabel = "Fraction of children",
         fiveNumberSummary = fivenum(filtered_output[,"days_since_last_visit"]));
    boxplot(x = filtered_output[,"days_since_last_visit"], 
           outline = FALSE, horizontal = TRUE, cex.names = 1.5,
          cex.axis = 1.5);
  }

  for (i in 1:n_races)
  {
    cat(paste("race = ", races[i], "\n", sep = " "));
    filtered_output <- subset(output, (race == races[i]));
    cat(paste("size of filtered_output = ", nrow(filtered_output), "\n", 
              sep = " "));
    histogram <- hist(filtered_output[,"days_since_last_visit"], 
                      breaks = edges, plot = FALSE);
    customHistogram(histogram = histogram,
        mainTitle = paste("for race",
                           races[i], ",",
                           nrow(filtered_output), "children", sep = " "),
         xLabel = "#days since last visit", yLabel = "Fraction of children",
         fiveNumberSummary = fivenum(filtered_output[,"days_since_last_visit"]));
    boxplot(x = filtered_output[,"days_since_last_visit"], 
           outline = FALSE, horizontal = TRUE, cex.names = 1.5,
          cex.axis = 1.5);
  }

  #for (i in 1:n_age_buckets)
  #{
  #  if (i == 1)
  #  {
  #    filtered_output <- subset(output, (age_rounded_in_years >= 0 & 
  #                                       age_rounded_in_years <= 1));
  #    title_part = "for age 0-1";
  #  }
  #  else if (i == 2)
  #  {
  #    filtered_output <- subset(output, (age_rounded_in_years >= 2 & 
  #                                       age_rounded_in_years <= 5));
  #    title_part = "for age 2-5";
  #  }
  #  else if (i == 3)
  #  {
  #    filtered_output <- subset(output, (age_rounded_in_years >= 6 & 
  #                                       age_rounded_in_years <= 12));
  #    title_part = "for age 6-12";
  #  }
  #  else if (i == 4)
  #  {
  #    filtered_output <- subset(output, (age_rounded_in_years >= 13 & 
  #                                       age_rounded_in_years <= 16));
  #    title_part = "for age 13-16";
  #  }
  #  else if (i == 5)
  #  {
  #    filtered_output <- subset(output, (age_rounded_in_years == 17));
  #    title_part = "for age 17+";
  #  }
  #  else if (i == 6)
  #  {
  #    filtered_output <- subset(output, (age_rounded_in_years >= 18));
  #    title_part = "for age 18+";
  #  }

  #  histogram <- hist(filtered_output[,"days_since_last_visit"], 
  #                    breaks = edges, plot = FALSE);
  #  customHistogram(histogram = histogram,
  #      mainTitle = paste(title_part, ",",
  #                         nrow(filtered_output), "children", sep = " "),
  #       xLabel = "#days since last visit", yLabel = "Fraction of children",
  #       fiveNumberSummary = fivenum(filtered_output[,"days_since_last_visit"]));
  #  boxplot(x = filtered_output[,"days_since_last_visit"], 
  #         outline = FALSE, horizontal = TRUE, cex.names = 1.5,
  #        cex.axis = 1.5);
  #}


  dbDisconnect(con);
  dev.off();
  #output;
}

