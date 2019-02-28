context("Lookup")

test_that("Lookups", {
  
  expect_equal(
    look_up(airquality, Month = 8, Day = 1, value = 'Wind'),
    6.9
  )
  
  expect_equal(
    look_up(airquality, Month = 6, Day = c(1,2,3), value = 'Wind'),
    c(8.6, 9.7, 16.1)
  )
  
  expect_equal(
    look_up(airquality, Month = 6, Day = 1.2, value = 'Temp', bin = T),
    78
  )
  
  expect_equal(
    look_up(CO2, Plant = "Qn1", conc = 95, value = 'uptake'),
    16
  )
  
})
