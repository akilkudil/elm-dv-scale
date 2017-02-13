module SqrtScale exposing (sqrtScale, domain, range, setLookup, lookupRange, lookupDomain)

import PowerScale exposing (powerScale, domain, range, setLookup, lookupRange, lookupDomain)


sqrtScale =
    powerScale 0.5


domain =
    PowerScale.domain


range =
    PowerScale.range


setLookup =
    PowerScale.setLookup


lookupRange =
    PowerScale.lookupRange


lookupDomain =
    PowerScale.lookupDomain


transform =
    PowerScale.transform
