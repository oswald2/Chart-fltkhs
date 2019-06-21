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

import           Control.Lens

import           Graphics.Rendering.Chart.Easy as Ch
import           Graphics.Rendering.Chart.Backend.FLTKHS

import           System.Random


trial frac = scanl (*) 1 (map f bits)
  where
    b = 0.1

    f True = (1+frac*(1+b))
    f False = (1-frac)
    bits = randoms $ mkStdGen 0

vals :: Double -> [ (Double,LogValue) ]
vals frac = [(fromIntegral x, LogValue y) | (x,y) <- filter (\(x,_)-> x `mod` (m+1)==0) $ take n $ zip [0..] (trial frac)]
  where
    n = 1001
    m = 0


chart :: Double -> Renderable ()
chart lwidth = toRenderable (layout 1001 (trial bits) :: Layout Double LogValue)
    where
        bits = randoms $ mkStdGen 0

        layout n t = layout_title .~ "Simulation of betting on a biased coin"
                    $ layout_plots .~ [
                            toPlot (plot "f=0.05" s1 n 0 (t 0.05)),
                            toPlot (plot "f=0.1" s2 n 0 (t 0.1))
                            ]
                    $ def

        plot tt s n m t = plot_lines_style .~ s
                        $ plot_lines_values .~
                            [[(fromIntegral x, LogValue y) | (x,y) <-
                                filter (\(x,_)-> x `mod` (m+1)==0) $ take n $ zip [0..] t]]
                        $ plot_lines_title .~ tt
                        $ def

        b = 0.1

        trial bits frac = scanl (*) 1 (map f bits)
            where
            f True = (1+frac*(1+b))
            f False = (1-frac)

        s1 = solidLine lwidth $ opaque green
        s2 = solidLine lwidth $ opaque blue

drawChart :: Ref Widget -> IO ()
drawChart widget = do
    rectangle' <- getRectangle widget
    withFlClip rectangle' $
          renderToWidgetEC widget $ do
            layout_title .= "Simulation of betting on a biased coin"
            plot (line "f=0.05" [vals 0.05 ])
            plot (line "f=0.1" [vals 0.1])


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

