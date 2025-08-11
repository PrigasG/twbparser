test_that("extract_named_connections parses basics", {
  xml <- xml2::read_xml(
    '<workbook><named-connection name="c1" caption="Athena">
       <connection class="athena" server="athena.us-east-1.amazonaws.com" dbname="AwsDataCatalog" schema="public"/>
     </named-connection></workbook>'
  )
  out <- extract_named_connections(xml)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("connection_id","connection_class","location_named") %in% names(out)))
  expect_equal(out$connection_class, "athena")
  expect_match(out$location_named, "Athena")
})
