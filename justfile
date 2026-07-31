run scene:
    cabal run raytracer -- {{ scene }} +RTS -N10 -A64m

runHalf scene:
    cabal run raytracer -- {{ scene }} +RTS -N6 -A32m
