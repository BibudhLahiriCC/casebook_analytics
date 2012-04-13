require 'dbi'

   begin
    # Connect to a database, old style
    dbh = DBI.connect('DBI:Pg:casebook2_staging:staging-db-slave.c45223.blueboxgrid.com',
                      'bibudh', 'lahiri2012')


    # Select all rows from simple01
=begin
       sth = dbh.prepare('select * from simple01')
    sth.execute

    # Print out each row
    while row=sth.fetch do
        p row
    end

    # Close the statement handle when done
    sth.finish
=end

    # And finally, disconnect
   rescue DBI::DatabaseError => e
     puts "An error occurred"
     puts "Error code: #{e.err}"
     puts "Error message: #{e.errstr}"
   ensure
     # disconnect from server
     dbh.disconnect if dbh
  end
