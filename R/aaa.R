
##Hack to make data.table functions work with devtools::load_all
#http://stackoverflow.com/questions/23252231/r-data-table-breaks-in-exported-functions
#http://r.789695.n4.nabble.com/Import-problem-with-data-table-in-packages-td4665958.html
assign(".datatable.aware", TRUE)

#Avoid false positives in R CMD CHECK:

utils::globalVariables(
  c(".fitted", ".resid", "method", "id", "yhat",
    "ymax", "yavg", "ymin", "metric", "metricSD"))
