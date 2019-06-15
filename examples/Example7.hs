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

import           Graphics.Rendering.Chart.Easy as Ch
import           Graphics.Rendering.Chart.Backend.FLTKHS
import           Graphics.Rendering.Chart.Backend
                                               as CB


circle :: [(Double,Double)]
circle = [ (r a * sin (a*dr),r a * cos (a*dr)) | a <- [0,0.5..360::Double] ]
  where
    dr = 2 * pi / 360
    r a = 0.8 * cos (a * 20 * pi /360)


chart = toRenderable layout
    where
        circle = [ (r a * sin (a*dr),r a * cos (a*dr)) | a <- [0,0.5..360::Double] ]
            where
                dr = 2 * pi / 360
                r a = 0.8 * cos (a * 20 * pi /360)

        circleP = plot_lines_values .~ [circle]
                $ plot_lines_style .~ solidLine 1.0 (opaque blue)
                $ def

        layout = layout_title .~ "Parametric Plot"
                $ layout_plots .~ [toPlot circleP]
                $ def








drawScene :: SceneStateRef -> Ref Widget -> IO ()
drawScene ref widget = do
    rectangle' <- getRectangle widget
    let coords@(x', y', w', h') = fromRectangle rectangle'
    withFlClip rectangle' $ do
          renderToWidgetEC widget $ do
            layout_title .= "Parametric Plot"
            plot (line "" [circle])



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

