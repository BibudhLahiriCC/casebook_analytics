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

get_last_visit_date <- function(con, person_id)
{
   #cp for child, cp1 for caseworker. Both have to be present.
   #statement <- paste(
   #"select COALESCE(max(to_char(c.occurred_at, 'YYYY-MM-DD')), '1900-01-01') last_visit_date ",
   #                   "from contacts c, contact_people cp, contact_people cp1 ",
   #                   "where (cp.contact_id = c.id) ",
   #                   "and c.mode like 'Face to Face%' ",
   #                   "and c.successful = 't' ",
   #                   "and cp.present = 't' ",
   #                   "and cp1.contact_id = c.id ",
   #                   "and cp1.person_id <> cp.person_id ",
   #                   "and cp1.present = 't' ",
   #                   "and cp.person_id = ", person_id, sep = "");
   #TODO: To take care of the fact that the caseworker be present 
   statement <- paste("select COALESCE(max(to_char(c.occurred_at, 'YYYY-MM-DD')), ",
                      "'1900-01-01') last_visit_date ",
                      "from contacts c, contact_people cp ",
                      "where (cp.contact_id = c.id) ",
                      "and c.mode like 'Face to Face%' ",
                      "and c.successful = 't' ",
                      "and cp.present = 't' ",
                      "and cp.person_id = ", person_id, sep = "");
  cat(paste("Query started: ", Sys.time(), "\n", sep = ""));
  res <- dbGetQuery(con, statement);
  cat(paste("Query ended: ", Sys.time(), "\n", sep = ""));
  last_visit_date <- as.character(res);
  return(last_visit_date);
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

#Returns true if a child has any open location,
#unless the open location is a THV or with a non-custodial 
#parent

has_open_location <- function(con, person_id)
{
  statement <- paste("select type, ",
                     "case when type = 'RemovalLocation::Placement' then 1 ",
                     "     when type = 'RemovalLocation::TrialHomeVisit' then 2 ",
                     "     when type = 'RemovalLocation::TemporaryAbsence' then 3 ",
                     "     when type = 'RemovalLocation::Runaway' then 4 ", 
                     "end ",
                     "as type_code ",
                     "from removal_locations ",
                     "where started_at is not null ",
                     "and ended_at is null ",
                     "and person_id = ", person_id, 
                     " order by type_code", sep = "");
  res <- dbSendQuery(con, statement);
  open_locations <- fetch(res, n = -1);
  n_open_locations <- nrow(open_locations);
  if (n_open_locations == 0)
  {
    return(FALSE);
  }
  #Will copy to a data frame the open location that remains after 
  #breaking ties
  filtered_location <- data.frame(matrix(nrow= nrow(open_locations), 
                                     ncol=ncol(open_locations))); 
  colnames(filtered_location) <- colnames(open_locations);

  k <- 0;

  for (i in 1:n_open_locations)
  {
    to_copy_row <- TRUE;
    #If there are multiple open locations for the same child, apply the following 
    #rules to break ties. 
    #1) Do not copy placement or THV if there is a temp absence
    #2) Do not copy temp absence if there is a runaway

    location_type <- open_locations[i, "type"];
    if (location_type == "RemovalLocation::Placement" | 
        location_type == "RemovalLocation::TrialHomeVisit")
    {
      j <- i + 1;
      #The way the data is ordered ensures that higher-priority types will be
      #located below lower-priority ones
      while (j <= n_open_locations & isTRUE(to_copy_row))
      {
        if (open_locations[j, "type"] == "RemovalLocation::TemporaryAbsence")
        {
          to_copy_row <- FALSE;
        }
        j <- j + 1;
      }
    }

    if (location_type == "RemovalLocation::TemporaryAbsence")
    {
      j <- i + 1;
      #The way the data is ordered ensures that higher-priority types will be
      #located below lower-priority ones
      while (j <= n_open_locations & isTRUE(to_copy_row)) 
      {
        if (open_locations[j, "type"] == "RemovalLocation::Runaway")
        {
          to_copy_row <- FALSE;
        }
        j <- j + 1;
      }
    }
    if (isTRUE(to_copy_row))
    {
      k <- k + 1;
      filtered_location[k, ] <- open_locations[i, ]; 
    }
  } # for (i in 1:n_open_locations)

  #Cut off trailing rows that are not need 
  filtered_location <- filtered_location[1:k, ];
  child_has_open_location <- FALSE;
  if (k > 1)
  {
    #There should not be more than one open location at this point for a child
    child_has_open_location <- TRUE;   
  }
  else if (k == 1)
  {
    #Check if this is a THV or with a non-custodial parent
    if (!((filtered_location[1, "type"] == "RemovalLocation::TrialHomeVisit") 
        | (isTRUE(with_non_custodial_parent(con, person_id)))))
    {
      child_has_open_location <- TRUE;
    }
  }
  return(child_has_open_location);
}

with_non_custodial_parent <- function(con, person_id)
{
   statement <- paste("select q.relocating_to_non_custodial_parent ",
                       "from removal_locations r, initial_placement_quizzes q ",
                       "where r.initial_placement_quiz_id = q.id ", 
                       "and r.person_id = ", person_id, 
                       sep = "");
   res <- dbGetQuery(con, statement);
   relocating_to_non_custodial_parent <- as.logical(res);
   return(relocating_to_non_custodial_parent);
}


in_home_population <- function(queryPoint)
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", host="mirror.in.mycasebook.org", 
                   port="5432", dbname="casebook2_mirror");

  #First, select all children who are the focus of some case
  statement <- paste("select cfc.person_id ",
                     "from case_focus_child_involvements cfci, ",
                     "involvement_types it, case_focus_children cfc ",
                     "where cfci.involvement_type_id = it.id ",
                     "and it.name <> 'Closed' ",
                     "and cfci.case_focus_child_id = cfc.id ",
                     "limit 100",
                     sep = "");
  res <- dbSendQuery(con, statement);
  case_focus_children <- fetch(res, n = -1);

  #Remove all children with any open location, unless the location is a THV
  #or with a non-custodial parent
  #Copy data to a new data frame, applying some conditions in between
  n_case_focus_children <- nrow(case_focus_children);
  children_without_open_locations <- data.frame(matrix(nrow= n_case_focus_children, 
                                     ncol=ncol(case_focus_children)));
  colnames(children_without_open_locations) <- 
     colnames(case_focus_children);

  k <- 0;
  cat(paste("Starting to check open locations:", Sys.time(), "\n", sep = ""));
  for (i in 1:n_case_focus_children)
  {
    has_open_loc = has_open_location(con, case_focus_children[i, "person_id"]);
    if (!isTRUE(has_open_loc))
    {
      k <- k + 1;
      children_without_open_locations[k, ] <-
        case_focus_children[i, ];
    }
  }
  cat(paste("Finished to check open locations:", Sys.time(), "\n", sep = ""));
  further_filtered <- as.data.frame(children_without_open_locations[1:k, ]);
  colnames(further_filtered) <- colnames(children_without_open_locations);

  cat(paste("Started to get last dates of visit:", Sys.time(), "\n", sep = ""));

  #Call get_last_visit_date for every row
  further_filtered$last_visit_date <- apply(further_filtered, 1, 
        function(row) get_last_visit_date(con, as.numeric(row["person_id"])));

  cat(paste("Ended to get last dates of visit:", Sys.time(), "\n", sep = ""));
 
  today <- Sys.Date();
  further_filtered$days_since_last_visit <- 
     round(as.numeric(difftime(today,as.POSIXlt(further_filtered$last_visit_date, 
                                           format="%Y-%m-%d"), 
                         units = c("days"))));

  #Get demographic data about the children
  further_filtered$demographic <- apply(further_filtered, 1, 
        function(row) get_demographic(con, as.numeric(row["person_id"])));
  
  cat(paste("Started to get demog info:", Sys.time(), "\n", sep = ""));

  for (i in 1:k)
  {
    tokenized_demographic <- unlist(strsplit(further_filtered[i, "demographic"], ","));
    further_filtered[i, "gender"] <- tokenized_demographic[1];
    further_filtered[i, "american_indian"] <- tokenized_demographic[2];
    further_filtered[i, "asian"] <- tokenized_demographic[3];
    further_filtered[i, "black"] <- tokenized_demographic[4];
    further_filtered[i, "pacific_islander"] <- tokenized_demographic[5];
    further_filtered[i, "white"] <- tokenized_demographic[6];
    further_filtered[i, "multi_racial"] <- tokenized_demographic[7];
    further_filtered[i, "hispanic_or_latino_origin"] <- tokenized_demographic[8];
    further_filtered[i, "date_of_birth"] <- tokenized_demographic[9];
  }

  cat(paste("Ended to get demog info:", Sys.time(), "\n", sep = ""));

  further_filtered$age_rounded_in_years <- 
    round(as.numeric(difftime(today, as.POSIXlt(further_filtered[, "date_of_birth"], 
                                     format="%Y-%m-%d"), units = c("days")))/365);

  cat(paste("Started to get race info:", Sys.time(), "\n", sep = ""));

  further_filtered$race <- apply(further_filtered, 1, 
        function(row) resolve_race(as.logical(row["american_indian"]),
                                   as.logical(row["asian"]),
                                   as.logical(row["black"]),
                                   as.logical(row["pacific_islander"]),
                                   as.logical(row["white"]),
                                   as.logical(row["multi_racial"])));
  cat(paste("Ended to get race info:", Sys.time(), "\n", sep = ""));

  cat(paste("Started to get county info:", Sys.time(), "\n", sep = ""));

  further_filtered$county_for_case <- apply(further_filtered, 1, 
        function(row) get_county_for_case(con, as.numeric(row["person_id"])));

  cat(paste("Ended to get county info:", Sys.time(), "\n", sep = ""));

  #TODO: Region of state county falls in

  #genders = unique(further_filtered[, "gender"]);
  genders = c("Male", "Female");
  n_genders = length(genders);

  counties = unique(further_filtered[, "county_for_case"]);
  n_counties = length(counties);

  races = unique(further_filtered[, "race"]);
  n_races = length(races);

  #Age buckets are 0-1, 2-5, 6-12, 13-16, 17, 18+
  n_age_buckets = 6;

  n_rows_of_plots = n_genders 
  #+ n_counties + n_races 
  #+ n_age_buckets
  ;
  
  png("in_home_population.png", 
      width = 840, height = 960, units = "px");
  #par(mar=c(5, 4, 4, 2) + 0.1);
  #par(oma = c(3,3,3,3));
  par(mfrow=c(n_rows_of_plots, 2));

  for (i in 1:n_genders)
  {
    filtered_output <- subset(further_filtered, (gender == genders[i]));
    histogram <- hist(filtered_output[,"days_since_last_visit"], 
                      plot = FALSE);
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


  #for (i in 1:n_counties)
  #{
  #  cat(paste("county = ", counties[i], "\n", sep = " "));
  #  filtered_output <- subset(further_filtered, (county_for_case == counties[i]));
  #  cat(paste("size of filtered_output = ", nrow(filtered_output), "\n", 
  #            sep = " "));
  #  histogram <- hist(filtered_output[,"days_since_last_visit"], 
  #                    breaks = edges, plot = FALSE);
  #  customHistogram(histogram = histogram,
  #      mainTitle = paste("for county",
  #                         counties[i], ",",
  #                         nrow(filtered_output), "children", sep = " "),
  #       xLabel = "#days since last visit", yLabel = "Fraction of children",
  #       fiveNumberSummary = fivenum(filtered_output[,"days_since_last_visit"]));
  #  boxplot(x = filtered_output[,"days_since_last_visit"], 
  #         outline = FALSE, horizontal = TRUE, cex.names = 1.5,
  #        cex.axis = 1.5);
  #}

  #for (i in 1:n_races)
  #{
  # cat(paste("race = ", races[i], "\n", sep = " "));
  #  filtered_output <- subset(further_filtered, (race == races[i]));
  #  cat(paste("size of filtered_output = ", nrow(filtered_output), "\n", 
  #            sep = " "));
  #  histogram <- hist(filtered_output[,"days_since_last_visit"], 
  #                    breaks = edges, plot = FALSE);
  #  customHistogram(histogram = histogram,
  #      mainTitle = paste("for race",
  #                         races[i], ",",
  #                         nrow(filtered_output), "children", sep = " "),
  #       xLabel = "#days since last visit", yLabel = "Fraction of children",
  #       fiveNumberSummary = fivenum(filtered_output[,"days_since_last_visit"]));
  #  boxplot(x = filtered_output[,"days_since_last_visit"], 
  #         outline = FALSE, horizontal = TRUE, cex.names = 1.5,
  #        cex.axis = 1.5);
  #} 
  dbDisconnect(con);
  dev.off();
}
