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

import           Control.Lens

import           Graphics.Rendering.Chart.Easy as Ch
import           Graphics.Rendering.Chart.Backend.FLTKHS


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



drawChart :: Ref Widget -> IO ()
drawChart widget = do
    rectangle' <- getRectangle widget
    withFlClip rectangle' $
        -- Draw either with this:
        -- void $ renderToWidget widget chart

        -- Or with the API from Graphics.Rendering.Chart.Easy:
        renderToWidgetEC widget $ do
            layout_title .= "Amplitude Modulation"
            setColors [opaque blue, opaque red]
            plot (line "am" [signal [0,(0.5)..400]])
            plot (points "am points" (signal [0,7..400]))


main :: IO ()
main = do
    let width  = 800
        height = 600

    window' <- doubleWindowNew (Size (Width width) (Height height))
                               Nothing
                               Nothing
    begin window'
    widget' <- widgetCustom
        (FL.Rectangle (Position (X 0) (Y 0)) (Size (Width width) (Height height)))
        Nothing
        drawChart
        defaultCustomWidgetFuncs
    end window'
    showWidget window'
    void FL.run

