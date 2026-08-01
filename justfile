run scene width="" height="" samples="":
    cabal run raytracer -- {{ scene }} {{ width }} {{ height }} {{ samples }} +RTS -N10 -A64m

runHalf scene width="" height="" samples="":
    cabal run raytracer -- {{ scene }} {{ width }} {{ height }} {{ samples }} +RTS -N6 -A32m
