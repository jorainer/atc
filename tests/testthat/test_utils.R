test_that("atcLevel works", {
    expect_identical(atcLevel(c("A", "A01")), c(1, 2))
    expect_identical(atcLevel("AA"), NA_real_)
})

test_that("toAtcLevel works", {
    expect_identical(unname(toAtcLevel("A01")), "A")
    toAtcLevel("A01C", level = c(1, 2))

    expect_identical(unname(toAtcLevel(c("A01CD", "A01CD04"), level = 3)),
                     c("A01C", "A01C"))

    toAtcLevel(c("A01CD", "A01CD04"), level = c(1, 3))
    ## Errors
    expect_error(toAtcLevel("A01", level = 7))
    expect_error(toAtcLevel("A0", level = 1))
})
