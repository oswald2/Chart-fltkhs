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

import           Control.Lens

import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Grid
import           Graphics.Rendering.Chart.Easy as Ch
import           Graphics.Rendering.Chart.Backend.FLTKHS
import           Graphics.Rendering.Chart.Backend
                                               as CB


-- haskell black scholes (see http://www.espenhaug.com/black_scholes.html)

blackScholesCall :: Double -> Double -> Double -> Double -> Double -> Double
blackScholesCall s x t r v = s * normcdf d1 - x * exp (-r * t) * normcdf d2
  where
    d1 = (log (s / x) + (r + v * v / 2) * t) / (v * sqrt t)
    d2 = d1 - v * sqrt t

normcdf :: Double -> Double
normcdf x | x < 0     = 1 - w
          | otherwise = w
  where
    w =
        1.0
            - 1.0
            / sqrt (2.0 * pi)
            * exp (-l * l / 2.0)
            * ( a1
              * k
              + a2
              * (k ** 2)
              + a3
              * (k ** 3)
              + a4
              * (k ** 4)
              + a5
              * (k ** 5)
              )
    k  = 1.0 / (1.0 + 0.2316419 * l)
    l  = abs x
    a1 = 0.31938153
    a2 = -0.356563782
    a3 = 1.781477937
    a4 = -1.821255978
    a5 = 1.330274429


-- Construct a single chart for the grid
bsChart :: Double -> Double -> Double -> Layout Double Double
bsChart t r v = execEC $ do
    layout_y_axis . laxis_generate .= scaledAxis def (-10, 80)
    plot $ line "" [[ (s, blackScholesCall s 100 0.001 r v) | s <- ss ]]
    plot $ line lbl [[ (s, blackScholesCall s 100 t r v) | s <- ss ]]
  where
    ss  = [50, 51 .. 150]
    lbl = "t = " ++ show t ++ ", r = " ++ show r

-- Construct a grid of charts, with a single title accross the top
grid = title `wideAbove` aboveN
    [ besideN [ layoutToGrid (bsChart t r v) | t <- ts ] | r <- rs ]
  where
    ts    = [1, 2, 5]
    rs    = [0.05, 0.10, 0.20]
    v     = 0.10
    title = setPickFn nullPickFn
        $ label ls HTA_Centre VTA_Centre "Black Scholes Option Values"
    ls = def { _font_size = 15, _font_weight = FontWeightBold }





drawScene :: SceneStateRef -> Ref Widget -> IO ()
drawScene ref widget = do
    rectangle' <- getRectangle widget
    let coords@(x', y', w', h') = fromRectangle rectangle'
    withFlClip rectangle'
        $ void $ renderToWidget widget
        $ fillBackground def
        $ gridToRenderable
        $ grid

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

