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
          ylab = yLabel, cex.lab = 1.5, space = 0, axisnames = TRUE, cex.names = 1.5,
          cex.axis = 1.5, cex.main = 1.5, font.main = 9, col.main= "blue",
          names.arg = barLabels, 
          sub = subTitle, cex.sub = 1.5, font.sub = 9, col.sub = "red"
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
  resource_types_for_caption <- c("foster families", "out of state resources", 
                      "residential resources");

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
                   #"limit 100",
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
       resource_id <- raw_data[j, "resource_id"];
       state <- raw_data[j, "state"];
       row <- raw_data[j, ]
       data <- rbind(data, row);
     }
     else if (raw_data[j, "state"] != state)
     {
       #change of state for same resource
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
    
    #Report for states which only have more than a threshold 
    #number of resources
    threshold <- 10;
    #For each of the states, get the number of resources of this type that 
    #have been in that state
    n_resources_grouped_by_states <- tapply(data$resource_id, data$state, length);
    n_states_to_plot_for <- 0;
    states_to_plot_for <- c();
    all_states <- names(n_resources_grouped_by_states);
    #n_resources_grouped_by_states and all_states will have same number of rows

    for (l in 1:length(n_resources_grouped_by_states))
    {
      if (n_resources_grouped_by_states[l] >= threshold)
      {
        n_states_to_plot_for <- n_states_to_plot_for + 1;
        states_to_plot_for <- append(states_to_plot_for, all_states[l]); 
      }
    }

    filename <- paste("length_of_states_", resource_types[i], ".png", sep = "");
    png(filename,  width = 920, height = 960, units = "px");

    ncols <- 2;
    nrows <- ceiling(n_states_to_plot_for/ncols);
    par(mfrow=c(nrows, ncols));
    n_resources <- length(unique(data$resource_id));

    for (l in 1:n_states_to_plot_for)
    {
      filtered_by_state <- subset(data, (state == states_to_plot_for[l]));
      
      #The same resource can be in the same state multiple times, e.g.,
      #a resource can go through the cycle licensed 
      #-> relicensure_application_pending -> licensed
      #-> relicensure_application_pending -> licensed -> revoked -> licensed.
      #If same resource is in the same state multiple times, 
      #Since the data is already ordered by resource_id and 
      #now it is filtered by state, the rows for same resource
      #in same state will be consecutive rows.
      summed_length_of_state <- c();
      nrows_filtered_by_state <- nrow(filtered_by_state);
      resource_id <- 0
      sum <- 0
      #str(filtered_by_state);
      for (m in 1:nrows_filtered_by_state)
      {
        if (filtered_by_state[m, "resource_id"] != resource_id)
        {
          #Encountered new resource
          if (resource_id != 0)
          {
            #There is a previous resource
            summed_length_of_state <- append(summed_length_of_state, sum);
            sum <- filtered_by_state[m, "length_of_state"];
            resource_id <- filtered_by_state[m, "resource_id"];
          }
          else
          {
            #This is the first resource
            sum <- filtered_by_state[m, "length_of_state"];
            resource_id <- filtered_by_state[m, "resource_id"];
          }
        }
        else
        {
          #Continuing with existing resource
          sum <- sum + filtered_by_state[m, "length_of_state"];
        }
      }
      #edges <- c(0, 2, 4, 6, 8, 10, 12, 16);
      histogram <- hist(summed_length_of_state, 
                        #breaks = edges, 
                        plot = FALSE);
      #We want number of resources of this type who spent at 
      #least one day in this state.
      customHistogram(histogram = histogram, 
         mainTitle = paste(length(summed_length_of_state), 
                           resource_types_for_caption[i], 
                           ", state:", states_to_plot_for[l], sep = " "),
         xLabel = "length in days", yLabel = "Fraction of resources",
         fivenum(summed_length_of_state));
    }
    dev.off();
   }  #end if (rows_fetched > 0)
  } #end for (i in 1:n_resource_types)
  dbDisconnect(con);
}

