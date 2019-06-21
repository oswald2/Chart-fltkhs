# Chart-fltkhs

FLTKHS backend for the Chart Haskell library.

Builds the library and (fi specified) the examples, which resemble the examples from https://github.com/timbod7/haskell-chart/wiki.

Transparency is not supported by FLTK, so e.g. Examples 8, 9 and 10 from the Chart library will not look correctly. This is a limitation of FLTK itself

To render a Chart to a widget, it is best to create a custom widget and override it's draw method.

## Building ##

Just use stack build. If you specify the flag 'examples', also the examples will be built:

> stack build
> stack build --flag Chart-fltksh:examples

### A usage example: ###

```haskell
widget' <- widgetCustom
    (FL.Rectangle (Position (X 0) (Y 0)) (Size (Width width) (Height height)))
    Nothing
    drawChart
    defaultCustomWidgetFuncs
```


Here, 'drawChart' is the provided draw method for the widget. A possible implementation
could be this:

```haskell
-- The char itself, to be used here with 'Graphics.Rendering.Chart.Easy'
signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * sin (x*3.14159/5)) | x <- xs ]

-- the overloaded drawing function
drawChart :: Ref Widget -> IO ()
drawChart widget = do
    -- determine a clipping area for the whole widget first
    rectangle' <- getRectangle widget

    -- with this clipping area, we draw the graph. This graph is taken from Example1
    -- from the Chart library
    withFlClip rectangle' $
        renderToWidgetEC widget $ do
            layout_title .= "Amplitude Modulation"
            setColors [opaque blue, opaque red]
            plot (line "am" [signal [0,(0.5)..400]])
            plot (points "am points" (signal [0,7..400]))
```

For more detailed examples, look into the [examples](https://github.com/oswald2/Chart-fltkhs/tree/master/examples) directory.

