
test_KeyFilter_constructor <- function(){
    kf <- KeyFilter("R01")
    checkEquals(atc:::where(kf), "= 'R01'")
    checkEquals(atc:::where(kf, atc), "atc.key = 'R01'")
    checkEquals(atc:::where(kf, atc, with.tables="ddd"), "ddd.key = 'R01'")

    checkEquals(atc:::column(kf), "key")
    checkEquals(atc:::column(kf, atc), "atc.key")
}

