n_placements_age_stacked_bar <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="indcs", host="sandbox.in.mycasebook.org", port="5432",dbname="casebook2_sandbox");
  png("n_placements_age_stacked_bar.png");
  bucket_size_age <- 5;
  bucket_size_n_placements <- 4;
  statement <- paste("select people.id, ",
    "(extract(year from age(fn_get_date_first_assessment(people.id), people.date_of_birth))::bigint)/",bucket_size_age,",", 
    " (count(distinct physical_location_records.id)::bigint)/", bucket_size_n_placements, 
                     " from people, physical_location_records",
                     " where people.id = physical_location_records.person_id",
                     " and physical_location_records.physical_location_type = 'PhysicalLocation::Placement'",
                     " and fn_get_date_first_assessment(people.id) is not NULL",
                     " and people.date_of_birth is not NULL",
                     " group by people.id, " ,
    "(extract(year from age(fn_get_date_first_assessment(people.id), people.date_of_birth))::bigint)/", bucket_size_age, 
                    sep = "");
  cat(paste(statement, "\n"));
  res <- dbSendQuery(con, statement);
  data <- fetch(res, n = -1);
  contingency_table <- table(data[,3], data[,2]); 
  #Note: #placements along X-axis, age along Y-axis in the plot.
  
  x_axis_labels <- c();
  n_age_groups <- nrow(contingency_table);
  for (i in 1:n_age_groups)
  {
    x_axis_labels[i] <- paste((i-1)*bucket_size_age, "-", i*bucket_size_age-1, 
                              sep = "");
  }
  str(x_axis_labels);
  str(contingency_table);
  barplot(contingency_table,  
          main = "Age group vs #placements histogram",
          xlab = "Number of placements", 
          col = colors()[c(461, 371, 290, 258, 138, 92, 32, 2)],
          axisnames = FALSE);
  axis(1, at=1:11, labels = x_axis_labels);
  dbDisconnect(con);
  dev.off();
  x_axis_labels;
}

