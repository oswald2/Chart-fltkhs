{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , BinaryLiterals
    , NumericUnderscores
    , FlexibleInstances
    , GADTs
    , ExistentialQuantification
#-}
module Graphics.Rendering.Chart.Backend.FLTKHS
(
    renderToWidget
    , renderToWidgetEC
    , runBackend
    , FLTKHSEnv
    , defaultEnv
    , withFlClip


    , flStrokePath
    , flFillPath
    , flTextSize
    , flDrawText
    , flWithFontStyle
)
where

import Control.Monad.IO.Class
import Control.Monad.Operational
import Control.Monad (void)

import qualified Data.Text as T
import Data.Char (chr)
import Data.Colour
import Data.Colour.SRGB
import Data.Bits
import Data.Default.Class

import qualified Graphics.UI.FLTK.LowLevel.FL  as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Types
import           Graphics.UI.FLTK.LowLevel.FLTKHS as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import Graphics.Rendering.Chart.Backend as G
import Graphics.Rendering.Chart.Backend.Impl
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Drawing as G
import Graphics.Rendering.Chart.Geometry as G
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.SparkLine
import Graphics.Rendering.Chart.State(EC, execEC)


data FLTKHSEnv = FLTKHSEnv {
    flAlignmentFns :: AlignmentFns
    , flFontColor :: Color
    , flPathColor :: Color
    , flFillColor :: Color
    , flCurrentMatrix :: Matrix
    }


defaultEnv :: AlignmentFns -> FLTKHSEnv
defaultEnv alignFns = FLTKHSEnv {
    flAlignmentFns = alignFns
    , flFontColor = blackColor
    , flPathColor = blackColor
    , flFillColor = whiteColor
    , flCurrentMatrix = Matrix 1.0 0.0 0.0 1.0 0.0 0.0
}

{-# INLINABLE renderToWidget #-}
renderToWidget :: Ref Widget -> Renderable a -> IO (PickFn a)
renderToWidget widget r = do
    rectangle' <- getRectangle widget
    let coords@(x', y', w', h') = fromRectangle rectangle'
        cr = render r (fromIntegral w', fromIntegral h')
    runBackend (defaultEnv bitmapAlignmentFns) cr

{-# INLINABLE renderToWidgetEC #-}
renderToWidgetEC :: (Default r, ToRenderable r) => Ref Widget -> EC r () -> IO ()
renderToWidgetEC widget ec = void $ renderToWidget widget (toRenderable (execEC ec))


{-# INLINABLE runBackend #-}
runBackend :: FLTKHSEnv -> BackendProgram a -> IO a
runBackend env m = eval env (view m)
    where
        eval :: FLTKHSEnv -> ProgramView ChartBackendInstr a -> IO a
        eval env (Return v) = return v
        eval env (StrokePath p :>>= f) = flStrokePath env p >>= step env f
        eval env (FillPath p :>>= f) = flFillPath env p >>= step env f
        eval env (GetTextSize s :>>= f) = flTextSize s >>= step env f
        eval env (DrawText p s :>>= f) = flDrawText env p s >>= step env f
        eval env (GetAlignments :>>= f) = step env f (flAlignmentFns env)
        eval env (WithTransform m p :>>= f) = flWithTransform env m p >>= step env f
        eval env (WithFontStyle font p :>>= f) = flWithFontStyle env font p >>= step env f
        eval env (WithFillStyle fs p :>>= f) = flWithFillStyle env fs p >>= step env f
        eval env (WithLineStyle ls p :>>= f) = flWithLineStyle env ls p >>= step env f
        eval env (WithClipRegion r p :>>= f) = flWithClipRegion env r p >>= step env f

        step :: FLTKHSEnv -> (v -> BackendProgram a) -> v -> IO a
        step env f v = runBackend env (f v)



{-# INLINABLE withColor #-}
withColor :: IO a -> IO a
withColor action = do
    col <- flcColor
    a <- action
    flcSetColor col
    pure a

{-# INLINABLE isClosed #-}
isClosed :: Path -> Bool
isClosed G.Close = True
isClosed End = False
isClosed (MoveTo _ p) = isClosed p
isClosed (LineTo _ p) = isClosed p
isClosed (Arc _ _ _ _ p) = isClosed p
isClosed (ArcNeg _ _ _ _ p) = isClosed p


{-# INLINABLE radToDegree #-}
radToDegree :: Double -> Double
radToDegree !theta = theta * 180 / pi

{-# INLINABLE pointToPrecisePosition #-}
pointToPrecisePosition :: Point -> PrecisePosition
pointToPrecisePosition p = PrecisePosition (PreciseX (p_x p)) (PreciseY (p_y p))

{-# INLINABLE pointToPosition #-}
pointToPosition :: Point -> Position
pointToPosition p = Position (X x) (Y y)
    where
        x = Prelude.round (p_x p)
        y = Prelude.round (p_y p)


instance Show Path where
    show (MoveTo p path) = "MoveTo " <> show p <> " " <> show path
    show (LineTo p path) = "LineTo " <> show p <> " " <> show path
    show (Arc p rad a1 a2 path) = "Arc " <> show p <> " " <> show rad <> " " <> show a1 <> " " <> show a2 <> " " <> show path
    show (ArcNeg p rad a1 a2 path) = "ArcNeg " <> show p <> " " <> show rad <> " " <> show a1 <> " " <> show a2 <> " " <> show path
    show End = "End"
    show G.Close = "Close"


{-# INLINABLE checkDouble #-}
checkDouble :: Double -> Double
checkDouble d = if isNaN d then 0 else d


flStrokePath :: FLTKHSEnv -> Path -> IO ()
flStrokePath env p = do
    withColor $ do
        flcSetColor (flPathColor env)
        let closed = isClosed p
        if closed
            then flcBeginLoop
            else flcBeginLine
        go p closed
    where
        go (MoveTo p path) closed = do
            if closed
                then do
                    flcEndLoop
                    flcBeginLoop
                else do
                    flcEndLine
                    flcBeginLine
            flcVertex (PrecisePosition (PreciseX (p_x p))
                (PreciseY (p_y p)))
            go path closed
        go (LineTo p path) closed = do
            flcVertex (PrecisePosition (PreciseX (p_x p))
                (PreciseY (p_y p)))
            go path closed
        go (Arc p r a1 a2 path) closed = do
            flcArcByRadius pt (checkDouble r) a1t a2t
            go path closed
            where
                pt = pointToPrecisePosition p
                !a1t = PreciseAngle (360 - radToDegree a1)
                !a2t = PreciseAngle (360 - radToDegree a2)
        go (ArcNeg p r a1 a2 path) closed = do
            flcArcByRadius pt (checkDouble r) a1t a2t
            go path closed
            where
                pt = pointToPrecisePosition p
                !a1t = PreciseAngle (radToDegree a1)
                !a2t = PreciseAngle (radToDegree a2)
        go End closed =
            if closed
                then flcEndLoop
                else flcEndLine
        go G.Close closed =
            if closed
                then flcEndLoop
                else flcEndLine



flFillPath :: FLTKHSEnv -> Path -> IO ()
flFillPath env p = do
    withColor $ do
        flcSetColor (flFillColor env)
        flcBeginComplexPolygon
        go p
    where
        go (MoveTo p path) = do
            flcGap
            flcVertex (PrecisePosition (PreciseX (p_x p))
                (PreciseY (p_y p)))
            go path
        go (LineTo p path) = do
            flcVertex (PrecisePosition (PreciseX (p_x p))
                (PreciseY (p_y p)))
            go path
        go (Arc p r a1 a2 path) = do
            flcArcByRadius pt (checkDouble r) a1t a2t
            go path
            where
                pt = pointToPrecisePosition p
                !a1t = PreciseAngle (360 - radToDegree a1)
                !a2t = PreciseAngle (360 - radToDegree a2)
        go (ArcNeg p r a1 a2 path) = do
            flcArcByRadius pt (checkDouble r) a1t a2t
            go path
            where
                pt = pointToPrecisePosition p
                !a1t = PreciseAngle (radToDegree a1)
                !a2t = PreciseAngle (radToDegree a2)
        go End =
            flcEndComplexPolygon
        go G.Close =
            flcEndComplexPolygon


flTextSize :: String -> IO TextSize
flTextSize text = do
    FL.Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h)) <- flcTextExtents (T.pack text)
    descent <- flcDescent
    let res = TextSize {
        textSizeWidth = fromIntegral w
        , textSizeHeight = fromIntegral h
        , textSizeDescent = fromIntegral descent
        , textSizeAscent = fromIntegral (h - descent)
        , textSizeYBearing = 0
        }
    pure res

{-# INLINABLE apply #-}
apply :: Matrix -> Point -> Point
apply (Matrix a1 a2 b1 b2 c1 c2) (Point x y) =
    let new_x = a1 * x + b1 * y + c1
        new_y = a2 * x + b2 * y + c2
    in Point new_x new_y


{-# INLINABLE flDrawText #-}
flDrawText :: FLTKHSEnv -> Point -> String -> IO ()
flDrawText env p text = withColor $ do
    flcSetColor (flFontColor env)
    flcDraw (T.pack text) (pointToPosition (apply (flCurrentMatrix env) p))




withSavedLineStyle :: IO a -> IO a
withSavedLineStyle action = do
    col <- flcColor
    a <- action
    flcLineStyle (LineDrawStyle Nothing Nothing Nothing) Nothing Nothing
    flcSetColor col
    return a

{-# INLINABLE clampI #-}
clampI :: Int -> Int
clampI x | x < 0 = 0
    | x > 255 = 255
    | otherwise = x

flWithLineStyle :: FLTKHSEnv -> G.LineStyle -> BackendProgram a -> IO a
flWithLineStyle env ls p = do
    withSavedLineStyle $ do
        let width = Prelude.round (_line_width ls)
            capStyle = convCapStyle (_line_cap ls)
            joinStyle = convJoinStyle (_line_join ls)
            style = LineDrawStyle Nothing (Just capStyle) (Just joinStyle)

            dashes = T.pack . map conv $ _line_dashes ls

            conv :: Double -> Char
            conv = chr . clampI . Prelude.round

            col = convColor (_line_color ls)

        flcLineStyle style (Just (Width width)) (Just dashes)
        runBackend env {flPathColor = col} p

flWithFillStyle :: FLTKHSEnv -> FillStyle -> BackendProgram a -> IO a
flWithFillStyle env fs p = runBackend env {flFillColor = convColor (_fill_color fs)} p


{-# INLINABLE withFlClip #-}
withFlClip :: FL.Rectangle -> IO a -> IO a
withFlClip rect action = do
    flcPushClip rect
    a <- action
    flcPopClip
    pure a


flWithClipRegion :: FLTKHSEnv -> Rect -> BackendProgram a -> IO a
flWithClipRegion env r@(Rect p1@(Point x1' y1') p2@(Point x2' y2')) p = do
    let mat = flCurrentMatrix env
        Point x1 y1 = apply mat p1
        Point x2 y2 = apply mat p2 

        rect = FL.Rectangle (Position
            (X (Prelude.round minx)) (Y (Prelude.round miny)))
            (Size (Width (Prelude.round w)) (Height (Prelude.round h)))
        minx = min x1 x2
        miny = min y1 y2
        maxx = max x1 x2
        maxy = max y1 y2
        w = maxx - minx
        h = maxy - miny
    withFlClip rect (runBackend env p)


{-# INLINABLE withMatrix #-}
withMatrix :: IO a -> IO a
withMatrix action = do
    flcPushMatrix
    a <- action
    flcPopMatrix
    pure a


flWithTransform :: FLTKHSEnv -> Matrix -> BackendProgram a -> IO a
flWithTransform env mat@(Matrix xx yx xy yy x0 y0) p = withMatrix $ do
    flcMultMatrix xx yx xy yy (ByXY (ByX x0) (ByY y0))
    runBackend env {flCurrentMatrix = flCurrentMatrix env * mat} p


{-# INLINABLE withFlFont #-}
withFlFont :: IO a -> IO a
withFlFont action = do
    font <- flcFont
    size <- flcSize
    a <- action
    flcSetFont font size
    pure a


flWithFontStyle :: FLTKHSEnv -> FontStyle -> BackendProgram a -> IO a
flWithFontStyle env font p = withFlFont $ do
    let fontSize = FontSize (Prelude.round (_font_size font))
        flfont = selectFont font
    flcSetFont flfont fontSize
    runBackend env {flFontColor = convColor (_font_color font)} p


{-# INLINABLE selectFont #-}
selectFont :: FontStyle -> Font
selectFont fs =
    case (_font_name fs, _font_slant fs, _font_weight fs) of
        ("serif", FontSlantNormal , FontWeightNormal) -> times
        ("serif", FontSlantNormal , FontWeightBold  ) -> timesBold
        ("serif", FontSlantItalic , FontWeightNormal) -> timesItalic
        ("serif", FontSlantOblique, FontWeightNormal) -> timesItalic
        ("serif", FontSlantItalic , FontWeightBold  ) -> timesBoldItalic
        ("serif", FontSlantOblique, FontWeightBold  ) -> timesBoldItalic

        ("sans-serif", FontSlantNormal , FontWeightNormal) -> helvetica
        ("sans-serif", FontSlantNormal , FontWeightBold  ) -> helveticaBold
        ("sans-serif", FontSlantItalic , FontWeightNormal) -> helveticaItalic
        ("sans-serif", FontSlantOblique, FontWeightNormal) -> helveticaItalic
        ("sans-serif", FontSlantItalic , FontWeightBold  ) -> helveticaBoldItalic
        ("sans-serif", FontSlantOblique, FontWeightBold  ) -> helveticaBoldItalic

        ("monospace", FontSlantNormal , FontWeightNormal) -> courier
        ("monospace", FontSlantNormal , FontWeightBold  ) -> courierBold
        ("monospace", FontSlantItalic , FontWeightNormal) -> courierItalic
        ("monospace", FontSlantOblique, FontWeightNormal) -> courierItalic
        ("monospace", FontSlantItalic , FontWeightBold  ) -> courierBoldItalic
        ("monospace", FontSlantOblique, FontWeightBold  ) -> courierBoldItalic

        (_, FontSlantNormal , FontWeightNormal) -> helvetica
        (_, FontSlantNormal , FontWeightBold  ) -> helveticaBold
        (_, FontSlantItalic , FontWeightNormal) -> helveticaItalic
        (_, FontSlantOblique, FontWeightNormal) -> helveticaItalic
        (_, FontSlantItalic , FontWeightBold  ) -> helveticaBoldItalic
        (_, FontSlantOblique, FontWeightBold  ) -> helveticaBoldItalic




{-# INLINABLE convCapStyle #-}
convCapStyle :: LineCap -> CapStyle
convCapStyle LineCapButt = CapStyleFlat
convCapStyle LineCapRound = CapStyleRound
convCapStyle LineCapSquare = CapStyleSquare

{-# INLINABLE convJoinStyle #-}
convJoinStyle :: LineJoin -> JoinStyle
convJoinStyle LineJoinMiter = JoinStyleMiter
convJoinStyle LineJoinRound = JoinStyleRound
convJoinStyle LineJoinBevel = JoinStyleBevel

{-# INLINABLE pureColour #-}
pureColour :: AlphaColour Double -> Colour Double
pureColour ac = darken (recip a) (ac `over` black)
    where
        a = alphaChannel ac

{-# INLINABLE convColor #-}
convColor :: AlphaColour Double -> Color
convColor color =
    let (RGB r g b) = toSRGB24 (pureColour color)
        col = Color (fromIntegral r `shiftL` 24
                .|. fromIntegral g `shiftL` 16
                .|. fromIntegral b `shiftL` 8)
    in
    col
