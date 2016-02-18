
test_levelFromAtc <- function(){
    checkEquals(atc:::levelFromAtc(c("A", "B", "C")), c(1, 1, 1))
    checkEquals(atc:::levelFromAtc(c("A01", "A")), c(2, 1))
    checkEquals(atc:::levelFromAtc(c("A01B")), 3)
    checkEquals(atc:::levelFromAtc(c("A01BA")), 4)
    checkEquals(atc:::levelFromAtc(c("A01BA02")), 5)
    checkEquals(atc:::levelFromAtc(c("A01dfdffdf")), as.numeric(NA))
}


