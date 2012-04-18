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
  cat(paste("nBars = ", nBars, ", totalFreq = ", totalFreq,
            ", xAxisRightEnd = ", xAxisRightEnd, ", width = ", 
            width, "\n", sep = ""));
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
  print(heights);
  print(widths);
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
  res <- dbGetQuery(con, statement);
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

get_supervising_agency <- function(con, resource_id)
{
  #TODO: With same child, same case, there may be two different case plans.
  #Will the county be always same for those case plans?
  statement <- paste("select supervising_agency_id ",
                     "from resources s ", 
                     "where s.id = ", resource_id, sep = "");
  res <- dbGetQuery(con, statement);
  supervising_agency <- as.numeric(res);
  return(supervising_agency);
}

get_parent_resource <- function(con, resource_id)
{
  statement <- paste("select parent_resource_id from resources where ",
                     "id = ", resource_id, sep = "");
  res <- dbGetQuery(con, statement);
  parent_resource <- as.numeric(res);
  return(parent_resource);
}

out_of_home_population2 <- function(queryPoint)
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", host="mirror.in.mycasebook.org", 
                   port="5432", dbname="casebook2_mirror");
  #Select all children with open location
  statement <- paste("select person_id, provider_id, provider_type, type, started_at, ",
                     "case when type = 'RemovalLocation::Placement' then 1 ",
                     "     when type = 'RemovalLocation::TrialHomeVisit' then 2 ",
                     "     when type = 'RemovalLocation::TemporaryAbsence' then 3 ",
                     "     when type = 'RemovalLocation::Runaway' then 4 ", 
                     "end ",
                     "as type_code ",
                     "from removal_locations ",
                     "where started_at is not null ",
                     "and ended_at is null ",
                     "order by person_id, type_code", sep = "");
  res <- dbSendQuery(con, statement);
  children_with_open_location <- fetch(res, n = -1);
  #Copy data to a new data frame, applying some conditions in between
  n_children_with_open_location <- nrow(children_with_open_location);
  duplicate_deleted_data <- data.frame();
  for (i in 1:n_children_with_open_location)
  {
    to_copy_row <- TRUE;
    child_id <- children_with_open_location[i, "person_id"];
    #If there are multiple open locations for the same child, apply the following 
    #rules to break ties. 
    #1) Do not copy placement or THV if there is a temp absence
    #2) Do not copy temp absence if there is a runaway

    location_type <- children_with_open_location[i, "type"];
    if (location_type == "RemovalLocation::Placement" | 
        location_type == "RemovalLocation::TrialHomeVisit")
    {
      j <- i + 1;
      #The way the data is ordered ensures that higher-priority types will be
      #located below lower-priority ones
      while (j <= n_children_with_open_location & 
              isTRUE(to_copy_row) & 
              children_with_open_location[j, "person_id"] == child_id)
      {
        if (children_with_open_location[j, "type"] == "RemovalLocation::TemporaryAbsence")
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
      while (j <= n_children_with_open_location & 
              isTRUE(to_copy_row) & 
              children_with_open_location[j, "person_id"] == child_id)
      {
        if (children_with_open_location[j, "type"] == "RemovalLocation::Runaway")
        {
          to_copy_row <- FALSE;
        }
        j <- j + 1;
      }
    }
    if (isTRUE(to_copy_row))
    {
      duplicate_deleted_data <- rbind(duplicate_deleted_data, 
                                      children_with_open_location[i, ]); 
    }
  } #end for (i in 1:n_children_with_open_location)
  n_duplicate_deleted_records <- nrow(duplicate_deleted_data);
  #All children should have at most one open location now. Discard if 
  #1) that location is THV or runaway
  #2) the open location is with non-custodial parent
  #3) the open location is out-of-state
  further_filtered <- data.frame();
  for (i in 1:n_duplicate_deleted_records)
  {
    to_copy_row <- TRUE;
    if (duplicate_deleted_data[i, "type"] == "RemovalLocation::TrialHomeVisit"
        & duplicate_deleted_data[i, "type"] == "RemovalLocation::Runaway")
    {
      to_copy_row <- FALSE;
    }
    statement <- paste("select q.relocating_to_non_custodial_parent ",
                       "from removal_locations r, initial_placement_quizzes q ",
                       "where r.initial_placement_quiz_id = q.id ", 
                       "and r.person_id = ", duplicate_deleted_data[i, "person_id"], 
                       sep = "");
    res <- dbGetQuery(con, statement);
    relocating_to_non_custodial_parent <- as.logical(res);
    if (isTRUE(relocating_to_non_custodial_parent))
    {
      to_copy_row <- FALSE;
    }
    statement <- paste("select s.type ",
                       "from removal_locations r, resources s ",
                       "where r.provider_id = s.id ",
                       "and r.person_id = ", duplicate_deleted_data[i, "person_id"], 
                       sep = "");
    res <- dbGetQuery(con, statement);
    resource_type <- as.character(res);
    if (resource_type == "OutOfStateResource")
    {
      to_copy_row <- FALSE;
    }
    if (isTRUE(to_copy_row))
    {
      further_filtered <- rbind(further_filtered, 
                                   duplicate_deleted_data[i, ]); 
    }
  } #end for (i in 1:n_duplicate_deleted_records)
  
  #Call get_last_visit_date for every row
  further_filtered$last_visit_date <- apply(further_filtered, 1, 
        function(row) get_last_visit_date(con, as.numeric(row["person_id"])));
 
  today <- Sys.Date();
  further_filtered$days_since_last_visit <- 
     round(as.numeric(difftime(today,as.POSIXlt(further_filtered$last_visit_date, 
                                           format="%Y-%m-%d"), 
                         units = c("days"))));

  #Get demographic data about the children
  further_filtered$demographic <- apply(further_filtered, 1, 
        function(row) get_demographic(con, as.numeric(row["person_id"])));
  
  n_further_filtered <- nrow(further_filtered);
  for (i in 1:n_further_filtered)
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
  further_filtered$age_rounded_in_years <- 
    round(as.numeric(difftime(today, as.POSIXlt(further_filtered[, "date_of_birth"], 
                                     format="%Y-%m-%d"), units = c("days")))/365);
                         
  further_filtered$race <- apply(further_filtered, 1, 
        function(row) resolve_race(as.logical(row["american_indian"]),
                                   as.logical(row["asian"]),
                                   as.logical(row["black"]),
                                   as.logical(row["pacific_islander"]),
                                   as.logical(row["white"]),
                                   as.logical(row["multi_racial"])));

  further_filtered$county_for_case <- apply(further_filtered, 1, 
        function(row) get_county_for_case(con, as.numeric(row["person_id"])));
  #TODO: Region of state county falls in

  for (i in 1:n_further_filtered)
  {
    if (further_filtered[i, "provider_type"] == "FosterFamily")
    {
     further_filtered[i, "supervising_agency"] <- 
        get_supervising_agency(con, further_filtered[i, "provider_id"]);
    }
    else
    {
      further_filtered[i, "supervising_agency"] <- NA_integer_;
    }
    if (further_filtered[i, "provider_type"] == "ResidentialResource")
    {
     further_filtered[i, "parent_resource"] <- 
        get_parent_resource(con, further_filtered[i, "provider_id"]);
    }
    else
    {
      further_filtered[i, "parent_resource"] <- NA_integer_;
    }
  }
  print(further_filtered);
  edges <- c(0, 6, 12, 18, 24, 30, 42000);
  #genders = unique(further_filtered[, "gender"]);
  genders = c("Male", "Female");
  n_genders = length(genders);

  counties = unique(further_filtered[, "county_for_case"]);
  n_counties = length(counties);
  cat(paste("n_counties = ", n_counties, "\n", sep = ""));

  races = unique(further_filtered[, "race"]);
  n_races = length(races);
  cat(paste("n_races = ", n_races, "\n", sep = ""));

  #Age buckets are 0-1, 2-5, 6-12, 13-16, 17, 18+
  n_age_buckets = 6;

  n_rows_of_plots = n_genders 
  #+ n_counties + n_races 
  #+ n_age_buckets
  ;
  
  png("out_of_home_population.png", 
      width = 840, height = 960, units = "px"
      );
  #par(mar=c(5, 4, 4, 2) + 0.1);
  #par(oma = c(3,3,3,3));
  par(mfrow=c(n_rows_of_plots, 2));

  for (i in 1:n_genders)
  {
    cat(paste("gender = ", genders[i], "\n", sep = " "));
    filtered_output <- subset(further_filtered, (gender == genders[i]));
    cat(paste("size of filtered_output = ", nrow(filtered_output), 
              "\n", sep = " "));
    histogram <- hist(filtered_output[,"days_since_last_visit"], 
                      #breaks = edges, 
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
  edges <- c(0, 6, 12, 18, 24, 30, 42000);
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
      width = 300, height = 400, units = "px"
      );
  par(mar=c(5, 4, 4, 2) + 0.1);
  par(oma = c(3,3,3,3));
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

