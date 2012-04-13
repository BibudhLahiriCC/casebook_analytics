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

length_of_states <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", 
          host="staging-db-slave.c45223.blueboxgrid.com", 
          port="5432",dbname="casebook2_staging");
  resource_types <- c("FosterFamily", "OutOfStateResource", 
                      "ResidentialResource");
  n_resource_types <- length(resource_types); 
  #Length of time resources spend in different license states
  for (i in 1:n_resource_types)
  {
   statement <- paste("select resource_id, paper_licenses.state state, ",
                   "date(paper_licenses.created_at) created_at ", 
                   "from paper_licenses, resources ",
                   "where paper_licenses.license_type = 'License' ",
                   "and paper_licenses.resource_id = resources.id ",
                   "and resources.type = '", resource_types[i], "' ",
                   "order by resource_id, date(paper_licenses.created_at) ",
                   "limit 100",
                   sep = "");
   cat(paste(statement, "\n", sep = ""));
   res <- dbSendQuery(con, statement);
   raw_data <- fetch(res, n = -1);
   rows_fetched <- nrow(raw_data);
   
   if (rows_fetched > 0)
   {
    cat(paste("rows_fetched = ", rows_fetched, "\n", sep = ""));
    
    data <- data.frame();
    #There may be multiple paper licenses for the same resource
    #in a period when the resource is in the same state. A row gets
    #added from raw_data to data if
    # 1) we encounter a new resource, or
    # 2) we encounter a new state for the same resource
    resource_id <- 0
    state <- "";
    for (j in 1:rows_fetched)
    {
     if (raw_data[j, "resource_id"] != resource_id) 
     {
       #new resource
       #cat(paste("new resource for j = ", j, ", resource_id = ", 
       #           resource_id, ", raw_data[j, resource_id] = ",
       #           raw_data[j, "resource_id"], "\n", sep = ""));
       resource_id <- raw_data[j, "resource_id"];
       state <- raw_data[j, "state"];
       row <- raw_data[j, ]
       data <- rbind(data, row);
     }
     else if (raw_data[j, "state"] != state)
     {
       #change of state for same resource
       #cat(paste("new state for j = ", j, ", state = ", 
       #           state, ", raw_data[j, state] = ",
       #           raw_data[j, "state"], "\n", sep = ""));
       state <- raw_data[j, "state"];
       row <- raw_data[j, ]
       data <- rbind(data, row);
     }
     #else do nothing, skip row
    } #end for (j in 1:rows_fetched)
    n_rows_processed_data <- nrow(data);
    today <- Sys.Date();

    for (k in 1:n_rows_processed_data)
    {
      #Compute the length the resource spends in each state
      if ((k < n_rows_processed_data) & 
          (data[k, "resource_id"] == data[k+1, "resource_id"]))
      {
        data[k, "length_of_state"] <-  
                 as.numeric(difftime(as.POSIXlt(data[k+1, "created_at"], format="%Y-%m-%d"),
                                     as.POSIXlt(data[k, "created_at"], 
                                     format="%Y-%m-%d"), units = c("days")));
      }
      else
      {
        data[k, "length_of_state"] <- as.numeric(difftime(today, 
                                                          as.POSIXlt(data[k, "created_at"], 
                                                                     format="%Y-%m-%d"), 
                                                         units = c("days")));
      }
    }
    #str(data);
    #For each possible state value (for each resource type),
    #find the number of days different resources spend in that state,
    #and then report distribution of that data
    states <- unique(data$state);
    n_states <- length(states);
    filename <- paste("length_of_states_", resource_types[i], ".png", sep = "");
    png(filename,  width = 840, height = 960, units = "px");
    par(mfrow=c(n_states, 1));
    n_resources <- length(unique(data$resource_id));

    for (l in 1:n_states)
    {
      filtered_by_state <- subset(data, (state == states[l]));
      #edges <- c(0, 2, 4, 6, 8, 10, 12, 16);
      histogram <- hist(filtered_by_state$length_of_state, 
                        #breaks = edges, 
                        plot = FALSE);
      #We want number of resources of this type who spent at 
      #least one day in this state.
      customHistogram(histogram = histogram, 
         mainTitle = paste("#length in days, ", 
                           nrow(filtered_by_state), resource_types[i], 
                          "resources", "state =", states[l], sep = " "),
         xLabel = "length in days", yLabel = "Fraction of resources",
         fivenum(filtered_by_state$length_of_state));
    }
    dev.off();
   }  #end if (rows_fetched > 0)
  } #end for (i in 1:n_resource_types)
  dbDisconnect(con);
}

