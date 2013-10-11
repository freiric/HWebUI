{-# LANGUAGE Rank2Types, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, ScopedTypeVariables #-}
module WidgetWires where

import Server
import Wires
import Widgets
import Properties

import Control.Monad.State (StateT, put, get)
import Control.Monad.Trans (liftIO)
import Data.Map (fromList)


data WidgetWire a b c= WidgetWire {
        hwuLayout :: [Property c] -> Widget,
        hwuWire :: GUIWire a b 
        }

data GuiState = GuiState {
        channelMap :: GSChannelMap,
        idCounter :: Int }
        
type WWMonad a = StateT GuiState IO a

initGuiState :: GuiState
initGuiState = GuiState ( (fromList [])::GSChannelMap ) 0

freshId :: String -> WWMonad String
freshId prefix = do
        gs <- get
        let idc = idCounter gs
        let cm = channelMap gs
        put (GuiState cm (idc + 1))
        return $ prefix ++ (show idc)

getCMap :: WWMonad GSChannelMap
getCMap = do
        gs <- get
        return (channelMap gs)
        
putCMap :: GSChannelMap -> WWMonad ()
putCMap cmap = do
        gs <- get
        put $ GuiState cmap (idCounter gs)
        return ()

updateState :: (String -> [Property c] -> Widget)
               -> (String -> GSChannelMap -> IO (GUIWire a b, GSChannelMap))
               -> WWMonad (WidgetWire a b c)
updateState widget wire = do
        elid <- freshId "hwuId"
        cmap <- getCMap
        let l = widget elid 
        (w, nmap) <- liftIO $ wire elid cmap
        putCMap nmap
        return $ WidgetWire l w

hwuButton :: WWMonad (WidgetWire a a Button)
hwuButton = updateState wButton buttonW

hwuHtml :: WWMonad (WidgetWire (Maybe String) String HtmlText)
hwuHtml = updateState wHtml htmlW

hwuTextBox :: WWMonad (WidgetWire (Maybe String) String TextBox)
hwuTextBox = updateState wTextBox textBoxW

hwuTextarea :: WWMonad (WidgetWire (Maybe String) String Textarea)
hwuTextarea = updateState wTextarea textareaW

hwuRadioButton :: WWMonad (WidgetWire (Maybe Bool) Bool RadioButton)
hwuRadioButton = updateState wRadioButton radioButtonW

hwuMultiSelect :: WWMonad (WidgetWire (Maybe [(String, Bool, a)]) [a] MultiSelect)
hwuMultiSelect = updateState wMultiSelect multiSelectW
