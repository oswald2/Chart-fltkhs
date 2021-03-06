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

import           Control.Lens

import           Graphics.Rendering.Chart.Easy as Ch
import           Graphics.Rendering.Chart.Backend.FLTKHS


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



drawChart ::  Ref Widget -> IO ()
drawChart widget = do
    rectangle' <- getRectangle widget
    withFlClip rectangle' $
        renderToWidgetEC widget $ do
            setColors [opaque black, opaque blue]

            layout_title .= "Positive and Negative Charges"
            plot $ vectorField "Electric Field" ef grid
            plot $ vectorField "B-field" bf grid




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

