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


values :: [ (String,Double,Bool) ]
values = [ ("Mexico City",19.2,False), ("Mumbai",12.9,False), ("Sydney",4.3,False), ("London",8.3,False), ("New York",8.2,True) ]

pitem (s,v,o) = pitem_value .~ v
              $ pitem_label .~ s
              $ pitem_offset .~ (if o then 25 else 0)
              $ def

chart = toRenderable layout
    where
        values = [ ("Mexico City",19.2,e), ("Mumbai",12.9,e), ("Sydney",4.3,e), ("London",8.3,e), ("New York",8.2,e1) ]
        e = 0
        e1 = 25
        pitem (s,v,o) = pitem_value .~ v
                        $ pitem_label .~ s
                        $ pitem_offset .~ o
                        $ def

        layout = pie_title .~ "Relative Population"
                $ pie_plot . pie_data .~ map pitem values
                $ def



drawScene :: SceneStateRef -> Ref Widget -> IO ()
drawScene ref widget = do
    rectangle' <- getRectangle widget
    let coords@(x', y', w', h') = fromRectangle rectangle'
    withFlClip rectangle' $
        renderToWidgetEC widget $ do
            pie_title .= "Relative Population"
            pie_plot . pie_data .= map pitem values



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

