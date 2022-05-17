{DEFAULT @primary_key = ''}

DROP TABLE IF EXISTS @results_schema.@table;

--Table @table
--HINT DISTRIBUTE ON RANDOM
CREATE TABLE @results_schema.@table (
    @columns
    {@primary_key != ''}?{
      , PRIMARY KEY(@primary_key)
    }
);

