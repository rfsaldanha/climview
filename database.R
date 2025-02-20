# Packages
library(tidyverse)
library(DBI)
library(duckdb)
library(zendown)
library(glue)

# Definitions
repo1 <- 13934888 # Normals and indicators
repo2 <- 13906834 # Zonal data

# Parquet files
eto_indi <- zen_file(repo1, "eto_indi.parquet")
eto_normal <- zen_file(repo1, "eto_normal.parquet")
eto <- zen_file(repo2, "ETo_3.2.3.parquet")
pr_indi <- zen_file(repo1, "pr_indi.parquet")
pr_normal <- zen_file(repo1, "pr_normal.parquet")
pr <- zen_file(repo2, "pr_3.2.3.parquet")
rh_indi <- zen_file(repo1, "rh_indi.parquet")
rh_normal <- zen_file(repo1, "rh_normal.parquet")
rh <- zen_file(repo2, "RH_3.2.3.parquet")
rs_indi <- zen_file(repo1, "rs_indi.parquet")
rs_normal <- zen_file(repo1, "rs_normal.parquet")
rs <- zen_file(repo2, "Rs_3.2.3.parquet")
tmax_indi <- zen_file(repo1, "tmax_indi.parquet")
tmax_normal <- zen_file(repo1, "tmax_normal.parquet")
tmax <- zen_file(repo2, "Tmax_3.2.3.parquet")
tmin_indi <- zen_file(repo1, "tmin_indi.parquet")
tmin_normal <- zen_file(repo1, "tmin_normal.parquet")
tmin <- zen_file(repo2, "Tmin_3.2.3.parquet")
u2_indi <- zen_file(repo1, "u2_indi.parquet")
u2_normal <- zen_file(repo1, "u2_normal.parquet")
u2 <- zen_file(repo2, "u2_3.2.3.parquet")

# Database connection
con <- dbConnect(duckdb(), "climindi.duckdb")

# Import files to database
dbExecute(con, glue("CREATE TABLE eto_indi AS SELECT * FROM '{eto_indi}'"))
dbExecute(con, glue("CREATE TABLE eto_normal AS SELECT * FROM '{eto_normal}'"))
dbExecute(con, glue("CREATE TABLE eto AS SELECT * FROM '{eto}'"))

dbExecute(con, glue("CREATE TABLE pr_indi AS SELECT * FROM '{pr_indi}'"))
dbExecute(con, glue("CREATE TABLE pr_normal AS SELECT * FROM '{pr_normal}'"))
dbExecute(con, glue("CREATE TABLE pr AS SELECT * FROM '{pr}'"))

dbExecute(con, glue("CREATE TABLE rh_indi AS SELECT * FROM '{rh_indi}'"))
dbExecute(con, glue("CREATE TABLE rh_normal AS SELECT * FROM '{rh_normal}'"))
dbExecute(con, glue("CREATE TABLE rh AS SELECT * FROM '{rh}'"))

dbExecute(con, glue("CREATE TABLE rs_indi AS SELECT * FROM '{rs_indi}'"))
dbExecute(con, glue("CREATE TABLE rs_normal AS SELECT * FROM '{rs_normal}'"))
dbExecute(con, glue("CREATE TABLE rs AS SELECT * FROM '{rs}'"))

dbExecute(con, glue("CREATE TABLE tmax_indi AS SELECT * FROM '{tmax_indi}'"))
dbExecute(con, glue("CREATE TABLE tmax_normal AS SELECT * FROM '{tmax_normal}'"))
dbExecute(con, glue("CREATE TABLE tmax AS SELECT * FROM '{tmax}'"))

dbExecute(con, glue("CREATE TABLE tmin_indi AS SELECT * FROM '{tmin_indi}'"))
dbExecute(con, glue("CREATE TABLE tmin_normal AS SELECT * FROM '{tmin_normal}'"))
dbExecute(con, glue("CREATE TABLE tmin AS SELECT * FROM '{tmin}'"))

dbExecute(con, glue("CREATE TABLE u2_indi AS SELECT * FROM '{u2_indi}'"))
dbExecute(con, glue("CREATE TABLE u2_normal AS SELECT * FROM '{u2_normal}'"))
dbExecute(con, glue("CREATE TABLE u2 AS SELECT * FROM '{u2}'"))






## Tests
dbListTables(con)

tbl(con, "pr_normal") |> head(100) |> collect() |> View()
