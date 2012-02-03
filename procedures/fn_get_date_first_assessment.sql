CREATE FUNCTION fn_get_date_first_assessment(child_id integer) RETURNS timestamp AS $$

DECLARE 
   date_first_assessment date;

BEGIN

 select min(date(assessments.created_at)) INTO date_first_assessment
 from allegations, assessments
 where assessments.id = allegations.assessment_id
 and allegations.victim_id = child_id
 and assessments.state = 'accepted'
 and assessments.created_at is not NULL;

 RETURN date_first_assessment;
END;
$$ LANGUAGE plpgsql;
