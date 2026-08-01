run scene width="" height="" samples="" path="":
    cabal run raytracer -- {{ scene }} {{ width }} {{ height }} {{ samples }} {{path}} +RTS -N10 -A64m

runHalf scene width="" height="" samples="" path="":
    cabal run raytracer -- {{ scene }} {{ width }} {{ height }} {{ samples }} {{path}} +RTS -N6 -A32m
