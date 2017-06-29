# Tests for duplicate functions

library(knightr)
context("duplicate identification")

library(dplyr)
test_df <- data.frame(a = c(1,3,3,3,5),
                      b = c("a", "c","c", "e", "c"),
                      c = c(NA, NA, NaN, NaN, "a"), stringsAsFactors = FALSE)

test_that("Correct combinations of duplicates are found", {
  expect_equal(get_dups(test_df, a), test_df[2:4, ])
  expect_equal(get_dups(test_df, b), test_df[c(2:3,5), ])
  expect_equal(get_dups(test_df, c), test_df[1:4, ])

  expect_message(check_unique(test_df, a), "Unique constraint is violated 2 times in test_df. Example rows: c(3, 4)", fixed=T)
  expect_message(check_unique(test_df, b), "Unique constraint is violated 2 times in test_df. Example rows: c(3, 5)\n", fixed=T)
  expect_message(check_unique(test_df, c), "Unique constraint is violated 2 times in test_df. Example rows: c(2, 4)\n", fixed=T)

  expect_error(assert_unique(test_df, a))
  expect_error(assert_unique(test_df, b))
  expect_error(assert_unique(test_df, c))

  expect_false(check_unique(test_df, a, b))
  expect_error(assert_unique(test_df, a, b))
  })


no_dupes <- data.frame(a = 1, stringsAsFactors = FALSE)

test_that("instances of no dupes return correct values", {
  expect_equal(no_dupes %>% get_dups(a), numeric(0))
  expect_true(check_unique(no_dupes, a))
  expect_equal(assert_unique(no_dupes, a), no_dupes)
})

test_that("works on variables with irregular names", {
  badname_df <- mtcars %>% mutate(`bad name!` = mpg * 1000)
  expect_equal(badname_df %>% get_dups(`bad name!`, cyl) %>% dim,
               c(10, 12)) # does it return the right-sized result?
  expect_is(badname_df %>% get_dups(), "data.frame") # test for success, i.e., produces a data.frame (with 0 rows)
  expect_false(check_unique(badname_df, `bad name!`, cyl))
  expect_error(assert_unique(badname_df, `bad name!`, cyl))
})
