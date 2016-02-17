
test_levelFromAtc <- function(){
    checkEquals(levelFromAtc(c("A", "B", "C")), c(1, 1, 1))
    checkEquals(levelFromAtc(c("A01", "A")), c(2, 1))
    checkEquals(levelFromAtc(c("A01B")), 3)
    checkEquals(levelFromAtc(c("A01BA")), 4)
    checkEquals(levelFromAtc(c("A01BA02")), 5)
    checkEquals(levelFromAtc(c("A01dfdffdf")), as.numeric(NA))
}


