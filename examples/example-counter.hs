{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import Control.Wire
import Prelude hiding ((.), id)
import UniqSupply

import WidgetWires
import HWebUI


main :: IO ()
main = do
 supply <- mkSplitUniqSupply 'i'
 let namedM = do
     WidgetWire wButton1 button1W <- wwButton "Button1"        
     WidgetWire wButton2 button2W <- wwButton "Button2"        
     WidgetWire wOut outW <- wwHtml
    
    -- settings 
     let port = 8080        
        
    -- create gui elements and layout
     let guiLayout = do    
         wInitGUI port
        
         -- buttons
         [whamlet|
              <H1>HWebUI - Counter Example
              The following buttons increase and decrease the counter:
                    |]
         wButton1
         wButton2

        -- finally the output text as html
         [whamlet|
              <p>And here the output value: 
              <p>
         |]
         wOut

    -- create netwire gui elements
     let theWire = do
        
         up <- button1W 
         down <- button2W
         output <- outW 
        
         -- build the FRP wire, we need a counter, which increases a value at each up event and decreases it at each down event
    
         -- this wire counts from 0, part of prefab netwire Wires
         let cnt = countFrom (0::Int)

         -- this wire adds one on button up, substracts one on button down, return id on no button press
         let w1 = cnt . ( up . pure 1 <|> down . pure (-1)  <|> pure 0 )

         -- stringify the output result (applicative style)
         let strw1 = (Just . show ) <$> w1
        
         -- set the output on change only
         return $ output . changed . strw1 
    
     -- run the webserver, the netwire loop and wait for termination   
     return $ runHWebUI port guiLayout theWire
     
     -- return ()
 initUs_ supply namedM    

 