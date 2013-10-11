{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import Control.Wire
import Prelude hiding ((.), id)

import HWebUI

guiDefinition = do
         -- define GUI Elements 
         buttonUp <- hwuButton
         buttonDown <- hwuButton
         outHtml <- hwuHtml
     
         -- define layout
         let guiLayout = do    
             let buttonUpW = hwuLayout buttonUp [label := "Counter Up"]
                 buttonDownW = hwuLayout buttonDown [label := "Counter Down"]
                 outHtmlW = hwuLayout outHtml []
             -- buttons
             [whamlet|
                   <H1>HWebUI - Counter Example
                   The following buttons increase and decrease the counter:
                         |]
             (buttonUpW)
             (buttonDownW)
     
             -- finally the output text as html
             [whamlet|
                   <p>And here the output value: 
                   <p>
             |]
             outHtmlW
     
         -- define functionality
--         let guiWire = do
             
         let up = hwuWire buttonUp 
         let down = hwuWire buttonDown 
         let output = hwuWire outHtml 
             
         -- build the FRP wire, we need a counter, which increases a value at each up event and decreases it at each down event
         
         -- this wire counts from 0, part of prefab netwire Wires
         let cnt = countFrom (0::Int)
         -- this wire adds one on button up, substracts one on button down, return id on no button press
         let w1 = cnt . ( up . pure 1 <|> down . pure (-1)  <|> pure 0 )
         -- stringify the output result (applicative style)
         let strw1 = (Just . show ) <$> w1
             
         -- set the output on change only
         let guiWire = output . changed . strw1 
        
         return (guiLayout, guiWire) 
          
main :: IO ()
main = do
         -- settings 
         let port = 8080
         runHWebUI port guiDefinition
