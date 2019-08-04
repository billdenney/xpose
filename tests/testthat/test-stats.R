context("base stats helpers")

test_that("logLik", {
  expect_equal(
    logLik(xpose::xpdb_ex_pk),
    structure(
      701.9525,
      nobs=476L,
      nind=74L,
      df=10L,
      class="logLik"
    )
  )
  expect_equal(
    logLik(xpose::xpdb_ex_pk, .problem=1),
    structure(
      701.9525,
      nobs=476L,
      nind=74L,
      df=10L,
      class="logLik"
    )
  )
})
