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
import           Graphics.UI.FLTK.LowLevel.FLTKHS
                                               as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import           Data.Time.LocalTime
import           Data.Time.Calendar

import           Control.Lens

import           Graphics.Rendering.Chart.Layout
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Easy as Ch
import           Graphics.Rendering.Chart.Backend.FLTKHS

import           Prices                         ( prices1 )
import           System.Random



values :: [(LocalTime, Double, Int, Int)]
values = [ (d, v, z, t) | ((d, v, _), z, t) <- zip3 prices1 zs ts ]
  where
    zs = randoms $ mkStdGen 0
    ts = randomRs (-2, 27) $ mkStdGen 1





drawChart :: Ref Widget -> IO ()
drawChart widget = do
    rectangle' <- getRectangle widget
    let coords@(x', y', w', h') = fromRectangle rectangle'
    withFlClip rectangle' $ do
        renderToWidgetEC widget $ do
            layout_title .= "Price History"
            layout_background .= solidFillStyle (opaque white)
            layout_foreground .= (opaque black)
            layout_left_axis_visibility . axis_show_ticks .= False

            plot (line "price 1" [[ (d, v) | (d, v, _) <- prices1 ]])

            plot $ liftEC $ do
                area_spots_4d_title .= "random value"
                area_spots_4d_max_radius .= 20
                area_spots_4d_values .= values


main :: IO ()
main = do
    let width  = 800
        height = 600

    window' <- doubleWindowNew (Size (Width width) (Height height))
                               Nothing
                               Nothing
    begin window'
    widget' <- widgetCustom
        (FL.Rectangle (Position (X 0) (Y 0))
                      (Size (Width width) (Height height))
        )
        Nothing
        drawChart
        defaultCustomWidgetFuncs
    end window'
    showWidget window'
    void FL.run

