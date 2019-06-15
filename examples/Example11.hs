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
import           Data.IORef

import           Control.Lens

import           Graphics.Rendering.Chart.Easy as Ch
import           Graphics.Rendering.Chart.Backend.FLTKHS
import           Graphics.Rendering.Chart.Backend as CB


titles = ["Cash","Equity"]

values :: [ (String,[Double]) ]
values =
  [ ("Jun", [20,45])
  , ("Jul", [45,30])
  , ("Aug", [30,20])
  , ("Sep", [10,40])
  , ("Oct", [20,50])
  ]


chart borders = toRenderable layout
  where
   layout =
         layout_title .~ "Sample Bars" ++ btitle
       $ layout_title_style . font_size .~ 10
       $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
       $ layout_y_axis . laxis_override .~ axisGridHide
       $ layout_left_axis_visibility . axis_show_ticks .~ False
       $ layout_plots .~ [ plotBars bars2 ]
       $ def :: Layout PlotIndex Double

   bars2 = plot_bars_titles .~ ["Cash","Equity"]
       $ plot_bars_values .~ addIndexes [[20,45],[45,30],[30,20],[70,25]]
       $ plot_bars_style .~ BarsClustered
       $ plot_bars_spacing .~ BarsFixGap 30 5
       $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
       $ def

   alabels = [ "Jun", "Jul", "Aug", "Sep", "Oct" ]

   btitle = if borders then "" else " (no borders)"
   bstyle = if borders then Just (solidLine 1.0 $ opaque black) else Nothing
   mkstyle c = (solidFillStyle c, bstyle)



drawScene :: SceneStateRef -> Ref Widget -> IO ()
drawScene ref widget = do
    rectangle' <- getRectangle widget
    let coords@(x', y', w', h') = fromRectangle rectangle'
    withFlClip rectangle' $
        renderToWidgetEC widget $ do
            layout_title .= "Sample Bars"
            layout_title_style . font_size .= 10
            layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
            plot $ fmap plotBars $ bars titles (addIndexes (map snd values))



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

