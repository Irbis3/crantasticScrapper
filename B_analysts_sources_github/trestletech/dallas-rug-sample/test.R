
test_that("Performance is good", {
  time <- system.time({
    for (i in 1:1000000){
      square(2)
    }
  })
  expect_true(
    time[3] < 0.8
  )
})

test_that("Positive Integers Work", {
  expect_equal(square(5), 25)
  
  expect_equal(square(3), 9)
})

test_that("Negatives work", {
  expect_equal(square(-2), 4)
})
test_that("Fractions work",{
  expect_equal(square(1.2), 1.44)
})