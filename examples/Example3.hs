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
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Time.LocalTime
import           Data.Time.Calendar

import           Control.Lens

import           Graphics.Rendering.Chart.Easy as Ch
import           Graphics.Rendering.Chart.Backend.FLTKHS

import           Prices                         ( prices1 )

fillBetween title vs = liftEC $ do
    plot_fillbetween_title .= title
    color <- takeColor
    plot_fillbetween_style .= solidFillStyle color
    plot_fillbetween_values .= vs


green1 = opaque $ sRGB 0.5 1 0.5
blue1 = opaque $ sRGB 0.5 0.5 1

chart = toRenderable layout
  where
    price1 =
        plot_fillbetween_style
            .~ solidFillStyle green1
            $  plot_fillbetween_values
            .~ [ (d, (0, v2)) | (d, v1, v2) <- prices1 ]
            $  plot_fillbetween_title
            .~ "price 1"
            $  def

    price2 =
        plot_fillbetween_style
            .~ solidFillStyle blue1
            $  plot_fillbetween_values
            .~ [ (d, (0, v1)) | (d, v1, v2) <- prices1 ]
            $  plot_fillbetween_title
            .~ "price 2"
            $  def

    layout =
        layout_title
            .~ "Price History"
            $  layout_grid_last
            .~ True
            $  layout_plots
            .~ [toPlot price1, toPlot price2]
            $  def


drawChart :: Ref Widget -> IO ()
drawChart widget = do
    rectangle' <- getRectangle widget
    withFlClip rectangle' $
        renderToWidgetEC widget $ do
            layout_title .= "Price History"
            plot
                (fillBetween "price 1" [ (d, (0, v2)) | (d, v1, v2) <- prices1 ]
                )
            plot
                (fillBetween "price 2" [ (d, (0, v1)) | (d, v1, v2) <- prices1 ]
                )


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

