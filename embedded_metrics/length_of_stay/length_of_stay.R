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

get_demographic <- function(con, person_id)
{
  statement <- paste("select p.gender || ',' || p.american_indian || ',' || p.asian || ',' || p.black || ',' ||",
                     "p.pacific_islander || ',' || p.white || ',' || p.multi_racial || ',' ||",
                     "p.hispanic_or_latino_origin || ',' || to_char(p.date_of_birth, 'YYYY-MM-DD') ",
                     "from people p where p.id = ", person_id, sep = "");
  res <- dbGetQuery(con, statement);
  demographic <- as.character(res);
  return(demographic);
}

get_county_for_case <- function(con, person_id)
{
  #TODO: With same child, same case, there may be two different case plans.
  #Will the county be always same for those case plans?
  statement <- paste("select cn.name county_for_case ", 
                     "from case_plan_focus_children cpfc, case_plans cl, ",
                     "cases cs, counties cn ",
                     "where cpfc.person_id = ", person_id, 
                     " and cpfc.case_plan_id = cl.id ",
                     "and cl.case_id = cs.id ",
                     "and cs.county_id = cn.id ",
                     "and cs.county_id is not null ",
                     "limit 1", sep = "");
  res <- dbGetQuery(con, statement);
  county_for_case <- as.character(res);
  return(county_for_case);
}

#This method, given a baseline date and an array of numbers,
#adds the numbers to that date and returns the list of dates.
#The result is a data frame with the name of the column given by
#colname.
get_dates <- function(baseline_date, numbers, colname)
{
  n_numbers <- length(numbers);
  dates <- mat.or.vec(n_numbers, 1);
  baseline <- as.POSIXlt(baseline_date, format="%Y-%m-%d");
  for (i in 1:n_numbers)
  {
    dates[i] <- format(as.POSIXlt(baseline + numbers[i]*24*3600, 
                                        tz = "", origin = baseline),
                       format = "%Y-%m-%d");
  }
  #Setting stringsAsFactors = F makes sure the entries in the data frame
  #are not factors, but character strings
  dates_df <- as.data.frame(as.vector(dates), stringsAsFactors=F);
  colnames(dates_df) <- c(colname);
  return(dates_df);
}

#This method gets the removal episode start and end dates for a given
#child. We use "locations" to start removal episodes, but court outcomes
#to end removal episodes.

get_removal_episodes <- function(con, person_id = 1)
{
  #cat(paste("person_id = ", person_id, "\n", sep = ""));
  #Get the start dates (and times) of the locations
  statement <- paste("select to_char(started_at, 'YYYY-MM-DD') ",
                     "location_start_date, ",
                     "to_char(started_at, 'YYYY-MM-DD HH24:MI:SS') ",
                     "location_start_date_with_time ",
                     " from removal_locations ", 
                     "where person_id = ", person_id, 
                     " order by started_at", sep = "");
  #res <- dbSendQuery(con, statement);
  #location_start_dates <- fetch(res, n = -1);
 
  #Get the dates of the court outcomes
  statement <- paste("select to_char(ch.date, 'YYYY-MM-DD') ",
                     "court_hearing_date, chot.name ",
                     "from court_hearings ch, court_hearing_outcomes cho, ",
                     "court_hearing_outcome_types chot ",
                     "where ch.person_id = ", person_id, " and ",
                     "cho.court_hearing_id = ch.id ",
                     "and cho.outcome_type_id = chot.id ",
                     "and chot.name in ('Adoption Finalized without Subsidy',",
                                      "'Adoption Finalized with Subsidy',",
                                      "'Guardianship Finalized without Subsidy',",
                                      "'Guardianship Finalized with Subsidy',",
                                      "'Detention Denied - Dismissal',",
                                      "'Court Case Closed/Child has been in foster care',",
                    "'Dismissal of CHINS Petition Ordered/Child has been in foster care',",
                    "'End Collaborative Care Program') ",
                    "order by court_hearing_date", sep = "");
  #res <- dbSendQuery(con, statement);
  #court_outcome_dates <- fetch(res, n = -1);
  location_start_dates <- get_dates("2012-02-28", 
         c(10), "location_start_date");

  court_outcome_dates <- get_dates("2012-02-28", 
         c(1, 15), "court_hearing_date");
  #Set two pointers, one in location_start_dates, other in court_outcome_dates
  i <- 1; j <- 1;
  n_location_start_dates <- nrow(location_start_dates);
  n_court_outcome_dates <- nrow(court_outcome_dates);

  if (n_location_start_dates > 0)
  {
    print(location_start_dates); 
  }
  if (n_court_outcome_dates > 0)
  {
    print(court_outcome_dates);
  }
  removal_episode_number <- 0;
  removal_episodes <- data.frame(matrix(nrow= nrow(location_start_dates), 
                                        ncol=3));
  colnames(removal_episodes) <- c("episode_number", "start_date",
                                  "end_date");

  while (i <= n_location_start_dates & j <= n_court_outcome_dates)
  {
    #In general, the start-date of a removal episode, 
    #that is the outcome of a court hearing,
    #is the first location_start_date
    #that follows the court_hearing_date by at least 2 days.
    #The end date of this removal episode is the next court_hearing_date,
    #and so on.

    while ((i <= n_location_start_dates) & 
           (as.POSIXlt(location_start_dates[i, "location_start_date"], 
                      format="%Y-%m-%d") < 
            as.POSIXlt(court_outcome_dates[j, "court_hearing_date"], 
                      format="%Y-%m-%d") + 2*24*3600))
    {
      i <- i + 1;
    }
    if (i <= n_location_start_dates)
    {
      removal_episode_number <- removal_episode_number + 1;
      removal_episodes[removal_episode_number, 1] <- removal_episode_number;
      removal_episodes[removal_episode_number, "start_date"] <- 
         location_start_dates[i, "location_start_date"];
      if (j < n_court_outcome_dates)
      {
        j <- j + 1;
         removal_episodes[removal_episode_number, "end_date"] <- 
         court_outcome_dates[j, "court_hearing_date"]; 
      }
    }
  }
  removal_episodes <- removal_episodes[1:removal_episode_number, ];
  if (removal_episode_number > 0)
  {
    print(removal_episodes);
  }
  return(removal_episodes);
}

length_of_stay <- function(queryPoint)
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", host="mirror.in.mycasebook.org", 
                   port="5432", dbname="casebook2_mirror");

  #First, select all children who ever had a location
  statement <- paste("select distinct(person_id) from removal_locations");
  res <- dbSendQuery(con, statement);
  children_with_locations <- fetch(res, n = -1);
  n_children_with_locations <- length(children_with_locations[,1]);
  for (i in 1:n_children_with_locations)
  {
    #Get the removal episode start and end dates for each child, if possible.
    removal_episodes <- get_removal_episodes(con, children_with_locations[i,1]);
  }
  dbDisconnect(con);
}
