module Geometry.Scene where

import Math (Point3, Resolution, TextureCoord, Vector3 (Vector3), (+.), (/.))

type ViewportResolution = (Float, Float)

createViewportResolutionFromHeight :: Float -> Resolution -> ViewportResolution
createViewportResolutionFromHeight viewH (w, h) =
    (viewH * ((fromInteger w) / (fromInteger h)), viewH)

data Camera = Camera
    { focalLength :: Float
    , viewportResolution :: ViewportResolution
    , cameraCenter :: Point3
    , resolution :: Resolution
    }

calculateUV :: Camera -> (Vector3, Vector3)
calculateUV cam =
    ( (Vector3 (fst (viewportResolution cam)) 0 0)
    , (Vector3 0 (-snd (viewportResolution cam)) 0)
    )

calculateDeltaUV :: Camera -> (Vector3, Vector3)
calculateDeltaUV cam =
    let (viewportU, viewportV) = calculateUV cam
        (width, height) = resolution cam
     in ( viewportU /. fromInteger width
        , viewportV /. fromInteger height
        )

calculateTopLeftPos :: Camera -> Vector3
calculateTopLeftPos cam =
    let camCenter = cameraCenter cam
        (viewportU, viewportV) = calculateUV cam
        (deltaU, deltaV) = calculateDeltaUV cam
        focalLengthVector = (Vector3 0 0 (focalLength cam))
        viewportUpperLeft = camCenter - focalLengthVector - (viewportU /. 2) - (viewportV /. 2)
     in viewportUpperLeft +. 0.5 * (deltaU + deltaV)
