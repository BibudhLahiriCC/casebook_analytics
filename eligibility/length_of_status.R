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
                    ", Med = ", fiveNumberSummary[3],
                    ", Q3 = ", fiveNumberSummary[4],
                    ", Max = ", fiveNumberSummary[5], sep = "");
  barplot(height = heights, width = widths, xlim = c(0, xAxisRightEnd),
          beside = TRUE, horiz = FALSE, main = mainTitle, xlab = xLabel,
          ylab = yLabel, space = 0, axisnames = TRUE, cex.names = 0.8, 
          names.arg = barLabels, 
          sub = subTitle
          );
}

length_of_status <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", 
          host="staging-db-slave.c45223.blueboxgrid.com", 
          port="5432",dbname="casebook2_staging");
  resource_types <- c("FosterFamily", "OutOfStateResource", 
                      "ResidentialResource");
  n_resource_types <- length(resource_types); 
  #Number of license states
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
    filename <- paste("n_states_", resource_types[i], ".png", sep = "");
    png(filename);
    par(mfrow=c(1, 2));

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
    n_states <- tapply(data$state, data$resource_id, length);
    #cat(n_states);
    edges <- c(0, 2, 4, 6, 8, 10, 12, 16);
    histogram <- hist(n_states, breaks = edges, plot = FALSE);
    #cat(paste(length(resource_types)));
    #cat(paste("resource_types[i] = ", as.character(resource_types[i]), "\n", sep = "")); 
    customHistogram(histogram = histogram, 
         mainTitle = paste("#states, ", 
                           nrow(n_states), resource_types[i], 
                           "rsrcs", sep = " "),
         xLabel = "#states", yLabel = "Fraction of resources",
         fivenum(n_states));
    boxplot(x = n_states, outline = FALSE);
    dev.off();
   }  #end if (rows_fetched > 0)
  } #end for (i in 1:n_resource_types)
  dbDisconnect(con);
  #dev.off();
}

