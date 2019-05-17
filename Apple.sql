-- Databricks notebook source
SELECT * FROM voter_file_csv;


-- COMMAND ----------

SELECT * FROM known_supporters_csv;


-- COMMAND ----------

SELECT * from known_opponents_csv;


-- COMMAND ----------

SELECT AVG(age) AS mean_age, STDDEV(age)AS Standard_deviation_age FROM voter_file_csv;

-- COMMAND ----------

SELECT AVG(age) AS mean_age_supporters, STDDEV(age)AS Standard_deviation_age_supporters, AVG(avgincome) AS mean_income_supporters, STDDEV(avgincome) As standard_deviation_income_supporters, AVG(model_probabilities_csv.ideology__conservative) AS mean_conserv, AVG(model_probabilities_csv.ideology__liberal) AS mean_lib, AVG(model_probabilities_csv.ideology__moderate) AS mean_moderate, AVG(model_probabilities_csv.ideology__progressive) AS mean_progressive
FROM  voter_file_csv 
Inner Join known_supporters_csv 
  ON  voter_file_csv.uid = known_supporters_csv.PRIMARYID
Inner Join model_probabilities_csv
  ON  known_supporters_csv.PRIMARYID = model_probabilities_csv.uid

-- COMMAND ----------

SELECT age, avgincome
FROM  voter_file_csv 
Inner Join known_supporters_csv 
  ON  voter_file_csv.uid = known_supporters_csv.PRIMARYID
Inner Join model_probabilities_csv
  ON  known_supporters_csv.PRIMARYID = model_probabilities_csv.uid

-- COMMAND ----------

SELECT AVG(age) AS mean_age_supporters, STDDEV(age)AS Standard_deviation_age_supporters, AVG(avgincome) AS mean_income_supporters, STDDEV(avgincome) As standard_deviation_income_supporters, AVG(model_probabilities_csv.ideology__conservative) AS mean_conserv, AVG(model_probabilities_csv.ideology__liberal) AS mean_lib, AVG(model_probabilities_csv.ideology__moderate) AS mean_moderate, party, COUNT(voter_file_csv.party) AS party_supporters
FROM  voter_file_csv 
Inner Join known_supporters_csv 
  ON  voter_file_csv.uid = known_supporters_csv.PRIMARYID
Inner Join model_probabilities_csv
  ON  known_supporters_csv.PRIMARYID = model_probabilities_csv.uid
GROUP BY party;

-- COMMAND ----------

SELECT AVG(voter_file_csv.age) AS mean_age_opposers, STDDEV(voter_file_csv.age) AS Standard_deviation_age_opposers, AVG(voter_file_csv.avgincome) AS mean_income_opposers,  STDDEV(voter_file_csv.avgincome) As standard_deviation_income_opposers , party, COUNT(voter_file_csv.party) AS party_opposers
FROM voter_file_csv
Inner Join  known_opponents_csv ON voter_file_csv.uid = known_opponents_csv.uid
GROUP BY party;

-- COMMAND ----------

SELECT AVG(voter_file_csv.age) AS mean_age_opposers, STDDEV(voter_file_csv.age) AS Standard_deviation_age_opposers, AVG(voter_file_csv.avgincome) AS mean_income_opposers,  STDDEV(voter_file_csv.avgincome) As standard_deviation_income_opposers , AVG(model_probabilities_csv.ideology__conservative) AS mean_conserv, AVG(model_probabilities_csv.ideology__liberal) AS mean_lib, AVG(model_probabilities_csv.ideology__moderate) AS mean_moderate, AVG(model_probabilities_csv.ideology__progressive) AS mean_progressive
FROM voter_file_csv
Inner Join  known_opponents_csv  
  ON voter_file_csv.uid = known_opponents_csv .uid 
Inner Join model_probabilities_csv
  ON  known_opponents_csv.uid = model_probabilities_csv.uid

-- COMMAND ----------

SELECT voter_file_csv.age , voter_file_csv.avgincome
FROM voter_file_csv
Inner Join  known_opponents_csv  
  ON voter_file_csv.uid = known_opponents_csv .uid 
Inner Join model_probabilities_csv
  ON  known_opponents_csv.uid = model_probabilities_csv.uid

-- COMMAND ----------

SELECT AVG(voter_file_csv.age) AS mean_age_opposers, STDDEV(voter_file_csv.age) AS Standard_deviation_age_opposers, AVG(voter_file_csv.avgincome) AS mean_income_opposers,  STDDEV(voter_file_csv.avgincome) As standard_deviation_income_opposers , AVG(model_probabilities_csv.ideology__conservative) AS mean_conserv, AVG(model_probabilities_csv.ideology__liberal) AS mean_lib, AVG(model_probabilities_csv.ideology__moderate) AS mean_moderate, party, COUNT(voter_file_csv.party) AS party_opposers
FROM voter_file_csv
Inner Join  known_opponents_csv  
  ON voter_file_csv.uid = known_opponents_csv .uid 
Inner Join model_probabilities_csv
  ON  known_opponents_csv.uid = model_probabilities_csv.uid
  GROUP BY party;

-- COMMAND ----------

SELECT AVG(ideology__conservative), AVG(ideology__liberal), AVG(ideology__moderate), AVG(ideology__progressive)
from 
FROM voter_file_csv
Inner Join  known_opponents_csv ON voter_file_csv.uid = known_opponents_csv.uid



-- COMMAND ----------

-- DBTITLE 1,comment party
/*
D DEMOCRAT
DS DECLINE TO STATE
GR GREEN
PF PEACE AND FREEDOM
NL NATURAL LAW
R REPUBLICAN
LI LIBERTARIAN
AI AMERICAN INDEPENDENT
RM REFORM
YY OTHER 
*/

-- COMMAND ----------

SELECT DISTINCT uid_to_household_key2.household_key , model_probabilities_csv.uid, model_probabilities_csv.persuadability__score probPursuade, model_probabilities_csv.turnout AS probVOte
FROM model_probabilities_csv
Left Join  known_supporters_csv ON model_probabilities_csv.uid =  known_supporters_csv.PRIMARYID
INNER JOIN uid_to_household_key2 ON known_supporters_csv.PRIMARYID = uid_to_household_key2.uid

-- COMMAND ----------

SELECT DISTINCT uid_to_household_key2.household_key , model_probabilities_csv.uid, model_probabilities_csv.local_schools__poor, model_probabilities_csv.issueB__no_concerns, model_probabilities_csv.turnout 
FROM model_probabilities_csv
Left Join  known_supporters_csv ON model_probabilities_csv.uid =  known_supporters_csv.PRIMARYID
LEFT JOIN uid_to_household_key2 ON known_supporters_csv.PRIMARYID = uid_to_household_key2.uid


-- COMMAND ----------

SELECT DISTINCT uid_to_household_key2.household_key , model_probabilities_csv.uid, model_probabilities_csv.local_schools__poor, model_probabilities_csv.issueB__no_concerns, model_probabilities_csv.turnout 
FROM model_probabilities_csv
Left Join  known_supporters_csv ON model_probabilities_csv.uid =  known_supporters_csv.PRIMARYID
LEFT JOIN uid_to_household_key2 ON known_supporters_csv.PRIMARYID = uid_to_household_key2.uid
WHERE model_probabilities_csv.turnout  >= 0.45 AND model_probabilities_csv.issueB__no_concerns < 0.45 AND  model_probabilities_csv.issueB__no_concerns <0.45 AND model_probabilities_csv.local_schools__poor  >= 0.45 ;

-- COMMAND ----------

/*
I would choose the second list because it targets people likely to vote, likely to be pursuaded, and who are likely to be unhappy with the schools. 
*/

-- COMMAND ----------





-- COMMAND ----------

SELECT known_opponents_csv.uid , uid_to_household_key2.household_key
FROM known_opponents_csv
Left JOIN uid_to_household_key2
  ON  known_opponents_csv.uid = uid_to_household_key2.uid;

-- COMMAND ----------

/*households that are no contact
*/

-- COMMAND ----------

SELECT  uid_to_household_key2.uid, uid_to_household_key2.household_key
FROM known_opponents_csv
Right JOIN uid_to_household_key2
  ON  known_opponents_csv.uid = uid_to_household_key2.uid

-- COMMAND ----------

SELECT known_opponents_csv.uid, voter_file_csv.
FROM known_opponents_csv
Left JOIN uid_to_household_key2
  ON  known_opponents_csv.uid = voter_file_csv.uid

-- COMMAND ----------

/*individuals that are no contact
*/

-- COMMAND ----------

Select * from phone_bank_csv2

-- COMMAND ----------

SELECT Date, Round(Agree/ Connects, 2) AS agree_per_connect, Round(Connects/ Calls, 2 ) AS connects_per_call
FROM phone_bank_csv2

-- COMMAND ----------

SELECT Date, Round(Agree/ Calls, 2) AS agree_per_call, Round(Connects/ Calls, 2 ) AS connects_per_call 
FROM phone_bank_csv2

-- COMMAND ----------

SELECT Date, ROUND(Undecided/Calls, 2) AS undecided_per_call, Round(Connects/ Calls, 2 ) AS connects_per_call 
FROM phone_bank_csv2

-- COMMAND ----------

SELECT Date, ROUND(Volunteer/Calls, 2) AS volunteers_per_call, Round(Connects/ Calls, 2 ) AS connects_per_call 
FROM phone_bank_csv2


-- COMMAND ----------

SELECT Date, Round(Agree/ Calls, 1) AS agree_per_call, ROUND(Undecided/Calls, 1) AS undecided_per_call, Round(Connects/ Calls, 1 ) AS connects_per_call 
FROM  phone_bank_csv2


-- COMMAND ----------

SELECT MIN(Date), MAX(Date)
FROM phone_bank_csv2
