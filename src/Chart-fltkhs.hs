{-# LANGUAGE OverloadedStrings
  , BangPatterns
  , MultiWayIf
  , LambdaCase
  , TemplateHaskell
#-}
module Main where


import           Control.Monad                  ( void )

import qualified Graphics.UI.FLTK.LowLevel.FL  as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Types
import           Graphics.UI.FLTK.LowLevel.FLTKHS as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Array
import           Data.IORef
import           Data.Thyme.Clock
import           Data.Thyme.Time         hiding ( Vector )

import           Control.Lens

import           Graphics.Rendering.Chart.Easy as Ch
import           Graphics.Rendering.Chart.Backend.FLTKHS
import           Graphics.Rendering.Chart.Backend as CB


signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * sin (x*3.14159/5)) | x <- xs ]

setLinesBlue :: PlotLines a b -> PlotLines a b
setLinesBlue = plot_lines_style  . line_color .~ opaque blue

chart = toRenderable layout
    where
        am :: Double -> Double
        am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

        sinusoid1 = plot_lines_values .~ [[ (x,(am x)) | x <- [0,(0.5)..400]]]
                $ plot_lines_style  . line_color .~ opaque blue
                $ plot_lines_title .~ "am"
                $ def

        sinusoid2 = plot_points_style .~ filledCircles 2 (opaque red)
                $ plot_points_values .~ [ (x,(am x)) | x <- [0,7..400]]
                $ plot_points_title .~ "am points"
                $ def

        layout = layout_title .~ "Amplitude Modulation"
            $ layout_plots .~ [toPlot sinusoid1,
                                toPlot sinusoid2]
            $ def






values :: [ (String,Double,Bool) ]
values = [ ("Mexico City",19.2,False), ("Mumbai",12.9,False), ("Sydney",4.3,False), ("London",8.3,False), ("New York",8.2,True) ]

pitem (s,v,o) = pitem_value .~ v
              $ pitem_label .~ s
              $ pitem_offset .~ (if o then 25 else 0)
              $ def


circle :: [(Double,Double)]
circle = [ (r a * sin (a*dr),r a * cos (a*dr)) | a <- [0,0.5..360::Double] ]
    where
        dr = 2 * pi / 360
        r a = 0.8 * cos (a * 20 * pi /360)



r' x y z = sqrt $ x^2 + y^2 + z^2
efield sign x y = ( sign*x/r,sign*y/r) where r = r' x y 10
bfield sign x y = (-sign*y/r^2,sign*x/r^2) where r = r' x y 10
square a s = [(x,y) | x <- range, y <- range] where range = [-a,-a+s..a] :: [Double]
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

ef (x,y) = efield 1 (x-20) y `Main.add` efield (-1) (x+20) y
bf (x,y) = bfield 1 (x-20) y `Main.add` bfield (-1) (x+20) y
grid = square 30 3

vectorField title f grid = fmap plotVectorField $ liftEC $ do
    c <- takeColor
    plot_vectors_mapf .= f
    plot_vectors_grid .= grid
    plot_vectors_style . vector_line_style . line_color .= c
    plot_vectors_style . vector_head_style . point_color .= c
    plot_vectors_title .= title


drawScene :: SceneStateRef -> Ref Widget -> IO ()
drawScene ref widget = do
    now <- getCurrentTime
    let fElapsedTime :: Double
        fElapsedTime = toSeconds (utctDayTime (unUTCTime now))
    scState    <- readIORef ref
    rectangle' <- getRectangle widget
    let coords@(x', y', w', h') = fromRectangle rectangle'
    withFlClip rectangle' $ do
        void $ renderToWidget widget chart

        -- renderToWidgetEC widget $ do
        --     layout_title .= "Amplitude Modulation"
        --     setColors [opaque blue, opaque red]
        --     plot (line "am" [signal [0,(0.5)..400]])
        --     plot (points "am points" (signal [0,7..400]))


        -- renderToWidgetEC widget $ do
        --     pie_title .= "Relative Population"
        --     pie_plot . pie_data .= map pitem values

        -- renderToWidgetEC widget $ do
        --     setColors [opaque black, opaque blue]

        --     layout_title .= "Positive and Negative Charges"
        --     plot $ vectorField "Electric Field" ef grid
        --     plot $ vectorField "B-field" bf grid
        --     pure ()

        -- let path = MoveTo (Point 100 100)
        --         $ LineTo (Point 150 110)
        --         $ LineTo (Point 200 200)
        --         $ Arc (Point 250 250) 60 (degToRad 0) (degToRad 75) Ch.Close
        --     degToRad x = x * pi / 180

        --     testPath = MoveTo (Point 10 10 )
        --         $ LineTo (Point 790 590)
        --         $ MoveTo (Point 790 10)
        --         $ LineTo (Point 10 590) End


        -- flFillPath (defaultEnv bitmapAlignmentFns) path
        -- flStrokePath (defaultEnv bitmapAlignmentFns) path
        -- let msg = "Test Text"
        --     fontStyle = FontStyle {_font_name = "sans-serif", _font_size = 100.0, _font_slant = FontSlantNormal, _font_weight = FontWeightNormal, _font_color = black `withOpacity` 1.0}

        --     mat = Matrix 0.5 0.0 0.0 0.5 100.0 50.0

        --     program =
        --         withFontStyle fontStyle $ do
        --         withTransform mat $ do
        --             CB.drawText (Point 100 100) msg
        --             textSize msg
        --             fillPath path
        --             strokePath path
        --             strokePath testPath

        -- res <- runBackend (defaultEnv bitmapAlignmentFns) program
        -- putStrLn $ "res: " <> show res


data SceneState = SceneState {
    scWidth :: Width
    , scHeight :: Height
    , scTheta :: Double
    }


type SceneStateRef = IORef SceneState


main :: IO ()
main = do
    let width  = 800
        height = 600
        fAspectRatio :: Double
        fAspectRatio = fromIntegral height / fromIntegral width

    ref     <- newIORef (SceneState (Width width) (Height height) 0.0)

    window' <- doubleWindowNew (Size (Width width) (Height height))
                               Nothing
                               Nothing
    begin window'
    widget' <- widgetCustom
        (FL.Rectangle (Position (X 0) (Y 0)) (Size (Width width) (Height height)))
        Nothing
        (drawScene ref)
        defaultCustomWidgetFuncs
    end window'
    showWidget window'
    void FL.run

