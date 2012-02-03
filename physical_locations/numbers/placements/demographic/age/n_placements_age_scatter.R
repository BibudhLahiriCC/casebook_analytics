n_placements_age_scatter <- function()
{
  library(RPostgreSQL);
  con <- dbConnect(PostgreSQL(), user="indcs", host="sandbox.in.mycasebook.org", port="5432",dbname="casebook2_sandbox");
  png("n_placements_age_scatter.png");
  statement <- paste("select people.id, ",
    "extract(year from age(assessments.created_at, people.date_of_birth)),",
    "count(distinct physical_location_records.id)",
    " from assessments, allegations, people, physical_location_records ",
    " where assessments.id = allegations.assessment_id",
    " and assessments.state = 'accepted'",
    " and allegations.victim_id = people.id", 
    " and people.id = physical_location_records.person_id",
    " and physical_location_records.physical_location_type = 'PhysicalLocation::Placement'",
    " and assessments.created_at is not NULL and people.date_of_birth is not NULL", 
    " group by people.id, extract(year from age(assessments.created_at, people.date_of_birth))", 
                       sep = "");
  res <- dbSendQuery(con, statement);
  data <- fetch(res, n = -1);
  plot(data[,2], data[,3], type = "p", 
          main = "Scatterplot for age (years) vs #placements",
          xlab = "Age (years) of creating assessment",
          ylab = "Number of placements");
  dbDisconnect(con);
  dev.off();
}

