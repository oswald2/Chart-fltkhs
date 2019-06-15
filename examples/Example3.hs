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

import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.IORef
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Time.LocalTime
import           Data.Time.Calendar

import           Control.Lens

import           Graphics.Rendering.Chart.Easy as Ch
import           Graphics.Rendering.Chart.Backend.FLTKHS
import           Graphics.Rendering.Chart.Backend
                                               as CB

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


drawScene :: SceneStateRef -> Ref Widget -> IO ()
drawScene ref widget = do
    rectangle' <- getRectangle widget
    let coords@(x', y', w', h') = fromRectangle rectangle'
    withFlClip rectangle' $ do
        renderToWidgetEC widget $ do
            layout_title .= "Price History"
            plot
                (fillBetween "price 1" [ (d, (0, v2)) | (d, v1, v2) <- prices1 ]
                )
            plot
                (fillBetween "price 2" [ (d, (0, v1)) | (d, v1, v2) <- prices1 ]
                )



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
        (FL.Rectangle (Position (X 0) (Y 0))
                      (Size (Width width) (Height height))
        )
        Nothing
        (drawScene ref)
        defaultCustomWidgetFuncs
    end window'
    showWidget window'
    void FL.run

