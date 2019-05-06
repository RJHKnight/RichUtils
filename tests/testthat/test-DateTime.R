test_that("next weekday", {

  # Start on a Monday
  thisDate <- ymd("20190506")
  thisDate <- getNextWeekday(thisDate)
  # Tues
  expect_equal(thisDate, ymd("20190507"))

  thisDate <- getNextWeekday(thisDate)
  # Wed
  expect_equal(thisDate, ymd("20190508"))

  thisDate <- getNextWeekday(thisDate)
  # Thurs
  expect_equal(thisDate, ymd("20190509"))

  thisDate <- getNextWeekday(thisDate)
  # Fri
  expect_equal(thisDate, ymd("20190510"))

  thisDate <- getNextWeekday(thisDate)
  # Mon
  expect_equal(thisDate, ymd("20190513"))

  thisDate <- getNextWeekday(ymd("20190511"))
  # Mon
  expect_equal(thisDate, ymd("20190513"))

  thisDate <- getNextWeekday(ymd("20190512"))
  # Mon
  expect_equal(thisDate, ymd("20190513"))
})


test_that("previous weekday", {

  # Start on a Saturday
  thisDate <- ymd("20190511")
  thisDate <- getPreviousWeekday(thisDate)
  expect_equal(thisDate, ymd("20190510"))

  thisDate <- getPreviousWeekday(thisDate)
  expect_equal(thisDate, ymd("20190509"))

  thisDate <- getPreviousWeekday(thisDate)
  expect_equal(thisDate, ymd("20190508"))

  thisDate <- getPreviousWeekday(thisDate)
  expect_equal(thisDate, ymd("20190507"))

  thisDate <- getPreviousWeekday(thisDate)
  expect_equal(thisDate, ymd("20190506"))

  thisDate <- getPreviousWeekday(ymd("20190506"))
  expect_equal(thisDate, ymd("20190503"))

  thisDate <- getPreviousWeekday(ymd("20190505"))
  expect_equal(thisDate, ymd("20190503"))
})
