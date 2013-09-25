module WidgetWires where
import Server
import Wires
import UniqSupply
import Unique
import HWebUI

data WidgetWire a b = WidgetWire Widget (ChannelStateGUIWire a b)

type NamedWidgetWire a b = UniqSM (WidgetWire a b)

giveAnId :: (String -> Widget) -> (String -> ChannelStateGUIWire a b) -> NamedWidgetWire a b
giveAnId wWidget wireW = do
        ident <- getUniqueM
        return $ WidgetWire (wWidget (show . getKey $ ident)) (wireW (show . getKey $ ident))

wwButton :: String -> NamedWidgetWire a a   
wwButton name = giveAnId (\ident -> wButton ident name) buttonW

wwHtml :: NamedWidgetWire (Maybe String) String
wwHtml = giveAnId wHtml htmlW
