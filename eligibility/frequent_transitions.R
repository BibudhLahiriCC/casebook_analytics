convert_state_to_index <- function(all_states, n_distinct_states, state)
{
  for (r in 1:n_distinct_states)
  {
    if (all_states[r, "state"] == state)
    {
      return(r);
    }
  }
  return(-1);
}


frequent_transitions <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="bibudh", 
          host="staging-db-slave.c45223.blueboxgrid.com", 
          port="5432",dbname="casebook2_staging");
  resource_types <- c("FosterFamily", "OutOfStateResource", 
                      "ResidentialResource");
  n_resource_types <- length(resource_types); 
  #Frquency of transition among different license states. First, get the states.
  statement <- "select distinct(state) from paper_licenses order by state";
  res <- dbSendQuery(con, statement);
  all_states <- fetch(res, n = -1);
  n_distinct_states <- nrow(all_states);
  #Initializa a square matrix with number of rows = #states
  transition_matrix <- mat.or.vec(n_distinct_states, n_distinct_states);
  all_state_as_vector <- as.vector(as.matrix(all_states));
  rownames(transition_matrix) <- all_state_as_vector;
  colnames(transition_matrix) <- all_state_as_vector;

  for (i in 1:n_resource_types)
  {
   statement <- paste("select resource_id, paper_licenses.state state ",
                   "from paper_licenses, resources ",
                   "where paper_licenses.license_type = 'License' ",
                   "and paper_licenses.resource_id = resources.id ",
                   "and resources.type = '", resource_types[i], "' ",
                   "order by resource_id, date(paper_licenses.created_at) ",
                   "limit 500",
                   sep = "");
   cat(paste(statement, "\n", sep = ""));
   res <- dbSendQuery(con, statement);
   raw_data <- fetch(res, n = -1);
   rows_fetched <- nrow(raw_data);
   
   if (rows_fetched > 0)
   {
    cat(paste("rows_fetched = ", rows_fetched, "\n", sep = ""));
    filename <- paste("frequent_transitions_", resource_types[i], ".txt", sep = "");
    
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
    #Go through all state transtions and update the transition matrix
    data_size <- nrow(data);
    resource_id <- 0

    for (k in 1:data_size)
    {
     if (data[k, "resource_id"] != resource_id)
     {
       #New resource. Does not count as a state change.
       resource_id <- data[k, "resource_id"];
     }
     else
     {
       #Continuing with same resouce. Note the change of state 
       #between previos row and this row.
       #prev_state <- convert_state_to_index(all_states, n_distinct_states, 
       #               data[k-1, "state"]);
       #current_state <- convert_state_to_index(all_states, n_distinct_states, 
       #               data[k, "state"]);
       transition_matrix[data[k-1, "state"], data[k, "state"]] <- 
         transition_matrix[data[k-1, "state"], data[k, "state"]] + 1;  
     }
    } #for (k in 1:data_size)
    sink(filename);
    print(transition_matrix);
    #Print the k most frequent transitions
    k <- 5;
    top_transitions <- mat.or.vec(k, 1);
    n_elements <- 0; #number of non-zero elements in top_transitions 
    for (l in 1:n_distinct_states)
    {
      for (m in 1:n_distinct_states)
      {
        if (transition_matrix[l, m] > 0)
        {
          if (n_elements < k)
          {
            top_transitions[n_elements + 1] <- transition_matrix[l, m];
            n_elements <- n_elements + 1;
            top_transitions <- sort(top_transitions, decreasing = TRUE);
            #cat(paste("Added", transition_matrix[l, m], "at",  n_elements,
            #          sep = " "));
          }
          else if (transition_matrix[l, m] > top_transitions[k])
          {
            #Since top_transitions is reverse-sorted, the last element is minimum
            top_transitions[k] <- transition_matrix[l, m];
            #cat(paste("Added", transition_matrix[l, m], "at", k,
            #          sep = " "));
            top_transitions <- sort(top_transitions, decreasing = TRUE);
          }
        }
      }
    }
    cat(paste("The", k, "most frequent transitions for resource type",
               resource_types[i], "are", "\n", sep = " "));
    for (m in 1:k)
    {
      cat(paste(top_transitions[m], "\n", sep = ""));
    }
    sink();
   }  #end if (rows_fetched > 0)
  } #end for (i in 1:n_resource_types)
  dbDisconnect(con);
}

