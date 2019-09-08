context('vars list')

test_that("list_vars()", {
  expect_output(test_ret <- list_vars(xpdb_ex_pk))
  expect_equal(
    test_ret,
    invisible(list(
      `1` =
        list(
          id = "ID",
          dv = "DV",
          idv = "TIME",
          amt = "AMT", 
          evid = "EVID",
          pred = "PRED",
          ipred = "IPRED",
          param = c("KA", "CL", "V", "ALAG1"),
          eta = c("ETA1", "ETA2", "ETA3"),
          res = c("CWRES", "IWRES", "RES", "WRES"),
          catcov = c("SEX", "MED1", "MED2"),
          contcov = c("CLCR", "AGE", "WT"),
          a = c("A1", "A2"),
          na = c("DOSE", "SS", "II", "TAD", "CPRED")
        ),
      `2` =
        list(
          id = "ID",
          dv = "DV",
          idv = "TIME",
          amt = "AMT",
          evid = "EVID",
          ipred = "IPRED",
          na = c("DOSE", "TAD", "SEX", "CLCR", "AGE", "WT"))
    ))
  )
})
