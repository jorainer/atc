
test_KeyFilter_constructor <- function(){
    kf <- KeyFilter("R01")
    checkEquals(where(kf), "= 'R01'")
    checkEquals(where(kf, atc), "atc.key = 'R01'")
    checkEquals(where(kf, atc, with.tables="ddd"), "ddd.key = 'R01'")

    checkEquals(column(kf), "key")
    checkEquals(column(kf, atc), "atc.key")
}

