--Function to return the length of stay for a given child

CREATE FUNCTION lengthOfStay(childID int) RETURNS int AS $$
  
  DECLARE
    
    recordFromPlacement RECORD;
    recordFromTHV RECORD;
    nTotalDaysInRemovalEpisodes int;
    nDaysInRemovalEpisode int;
    nDaysInTHV int;
    vCurrentDate date; --Start date of the 669-day long sliding window
    vStartDate date;

    /*The physical_location_id in physical_location_placements can be the ID of a 
      placement, a THV, a temporary absence or a runaway. If matching ID is found in 
      physical_location_placements, then the location is a placement.*/

    cursForPlacement CURSOR (startDate date, today date) FOR 
     select physical_location_placements.id, date(start_date) placementStartDate, date(end_date) placementEndDate
     from physical_location_placements, physical_location_records
     where physical_location_records.physical_location_id = physical_location_placements.id
     and ((date(start_date) is not NULL and (date(end_date) between startDate and today))
           or (date(end_date) is not NULL and (date(start_date) between startDate and today)))
     and physical_location_records.person_id = childID;

    --Find trail home visits that start and/or end within a given placement. Max of start dates, min of end date in case of overlap.
    cursForTHV CURSOR(placementStartDate date, placementEndDate date) FOR
     select CASE WHEN date(start_date) >= placementStartDate THEN date(start_date) 
                 ELSE placementStartDate
            END AS THVStartDate, 
            CASE WHEN date(end_date) <= placementEndDate THEN date(end_date) 
		                 ELSE placementEndDate
		    END AS THVEndDate
     from physical_location_records, physical_location_trial_home_visits
     where physical_location_records.physical_location_id = physical_location_trial_home_visits.id
     and ((date(physical_location_trial_home_visits.start_date) between placementStartDate and placementEndDate)
           or (date(physical_location_trial_home_visits.end_date) between placementStartDate and placementEndDate))
     and physical_location_records.person_id = childID;
      
   BEGIN
    
    vCurrentDate := date(current_timestamp);
    vStartDate := vCurrentDate - 669;
    nTotalDaysInRemovalEpisodes := 0;

    FOR recordFromPlacement IN cursForPlacement(vStartDate, vCurrentDate) LOOP
      
      --Count number of days in each removal episode.
      IF recordFromPlacement.placementEndDate IS NULL THEN
        nDaysInRemovalEpisode := vCurrentDate - recordFromPlacement.placementStartDate;
      ELSE
        nDaysInRemovalEpisode := recordFromPlacement.placementEndDate - recordFromPlacement.placementStartDate;
      END IF;

      nTotalDaysInRemovalEpisodes := nTotalDaysInRemovalEpisodes + nDaysInRemovalEpisode;

      /*Find trail home visits that start and/or end within a given placement, and subtract the number of days in such trial visits 
        from the days in placement.*/
      FOR recordFromTHV IN cursForTHV(recordFromPlacement.placementStartDate, recordFromPlacement.placementEndDate) LOOP
         nDaysInTHV := recordFromTHV - 
        
      END LOOP;
     
    END LOOP;
    
  END;
$$ LANGUAGE plpgsql;