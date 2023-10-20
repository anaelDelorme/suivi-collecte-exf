populate_table <- function(bucket, object){

  # Get data from MinIO
  df <- aws.s3::s3read_using(
    FUN = arrow::read_parquet,
    object = object,
    bucket = bucket,
    opts = list("region" = "")
  )
  return(df)

}
