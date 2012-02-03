#Gives the number of children in placement on any given date t
n_children_in_placement <- function(queryPoint)
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="indcs", host="sandbox.in.mycasebook.org", port="5432",dbname="casebook2_sandbox");
  statement <- paste(
   "select count(person_id)", 
   " from physical_location_records, physical_location_placements",
   " where  physical_location_records.physical_location_id = physical_location_placements.id",
   " and physical_location_records.physical_location_type = 'PhysicalLocation::Placement'",
   " and date(physical_location_placements.start_date) <= '", queryPoint, "'",
   " and (date(physical_location_placements.end_date) is NULL ",
       "or date(physical_location_placements.end_date) > '", queryPoint, "')",
              sep = "");
  #str(statement);
  res <- dbSendQuery(con, statement);
  data <- fetch(res, n = -1);
  dbDisconnect(con);
  data$count;
}


#Gives the number of children who were in placement on any given date t
#and did not have a face to face visit with any caseworker in the last
#n days, counting from t, i.e., in the window [t-n+1, t]
no_visit_in_n_days <- function(windowLength, queryPoint)
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="indcs", host="sandbox.in.mycasebook.org", port="5432",dbname="casebook2_sandbox");
  statement <- paste(
   "select b.person_id, '", queryPoint, "' - max(date(contacts.occurred_at))",
   " from physical_location_records, physical_location_placements,", 
   " contact_people b, contacts",
   " where physical_location_records.person_id = b.person_id",
   " and physical_location_records.physical_location_id = physical_location_placements.id",
   " and physical_location_records.physical_location_type = 'PhysicalLocation::Placement'",
   " and date(physical_location_placements.start_date) <= '", queryPoint, "'",
   " and (date(physical_location_placements.end_date) is NULL",
   " or date(physical_location_placements.end_date) > '", queryPoint, "')",
   " and date(contacts.occurred_at) <= '", queryPoint, "'",
   " and contacts.mode like 'Face to Face%'",
   " and contacts.id = b.contact_id",
   " group by b.person_id",
   " having ('", queryPoint, "' - max(date(contacts.occurred_at))) >= ", 
   windowLength,
              sep = "");
  #str(statement);
  res <- dbSendQuery(con, statement);
  data <- fetch(res, n = -1);
  dbDisconnect(con);
  nrow(data);
}

fraction_children_no_visit <- function(queryPoint)
{
  png("fraction_children_no_visit.png");
  #currentDate <- format(Sys.time(), "%Y-%m-%d");
  n_children_in_placement <- n_children_in_placement(queryPoint); 
  windowLengths <- c(6, 12, 18, 24, 30, 36);
  nPoints <- length(windowLengths);
  fractions <- c();
  labels <- c();
  for (i in 1:nPoints)
  {
    no_visit_in_n_days <- no_visit_in_n_days(windowLengths[i], queryPoint);
    fraction <- round(no_visit_in_n_days/n_children_in_placement, 2);
    fractions[i] <- fraction; 
    labels[i] <- paste("(", no_visit_in_n_days, ", ", fraction, ")", sep = "");
  }
  plot(x = windowLengths, y = fractions, type = "b", 
       main = paste("f(n,t) for varying n and t = ", queryPoint, sep = ""),
       sub = paste(n_children_in_placement, " children in placement", sep = ""),
       xlab = "n", ylab = "f(n,t)", pch=18, col="blue", 
       xlim = c(floor(0.8*min(windowLengths)), ceiling(1.2*max(windowLengths))));
  text(windowLengths, fractions, labels, cex=0.8, pos=4, col="red");
  dev.off();
}

#Returns f(n,t).
fraction_children_no_visit <- function(windowLength, queryPoint)
{
  n_children_in_placement <- n_children_in_placement(queryPoint); 
  no_visit_in_n_days <- no_visit_in_n_days(windowLength, queryPoint);
  fraction <- 0;
  if (n_children_in_placement > 0)
  {
   fraction <- round(no_visit_in_n_days/n_children_in_placement, 2);
  }
  cat(paste("queryPoint = ", queryPoint, 
            ", n_children_in_placement = ", n_children_in_placement,
            ", no_visit_in_n_days = ", no_visit_in_n_days,
            ", fraction = ", fraction, "\n", sep = ""));
  c(n_children_in_placement, no_visit_in_n_days, fraction);
}

# endPoint is the maximum value of t for which we want f(n,t). 
# startPoint is the minimum value of t for which we want f(n,t). 
# interval is the gap between two consecutive points in time.

variation_fraction_children_no_visit_with_time <- function(startPoint, 
                                                  endPoint, interval, 
                                                  windowLength)
{
  fileName <- paste("variation_fnt_winLen_", windowLength, ".png", sep = "");
  png(fileName);
  queryPoint = startPoint;
  fractions <- c();
  queryPoints <- c();
  labels <- c();
  i <- 1;
  while (queryPoint <= endPoint)
  { 
    queryPoints[i] <- queryPoint;
    fcnv <- fraction_children_no_visit(windowLength, queryPoint);
    fractions[i] <- fcnv[3];
    queryPoint <- as.character(as.Date(queryPoint) + interval);
    labels[i] <- paste("(", fcnv[1], ", ", fcnv[2], ", ", fcnv[3], ")");
    i <- i + 1;
  }
  plot(x = as.Date(queryPoints), y = fractions, type = "b", 
       main = paste("f(n,t) for n = ", windowLength, sep = ""),
       xlab = "Query points", ylab = "f(n,t)", pch=18, col="blue");
  text(queryPoints, fractions, labels, cex=0.8, pos=4, col="red");
  dev.off();
}
