module Draw.Window where

import Control.Monad
import Data.Dequeue
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras


-- XFCE covers the root window with a "Desktop" one
-- on which the wallpaper /
findWindowByName :: String -> Display -> Window -> IO (Maybe Window)
findWindowByName needle dpy = go . fromList . (:[]) where

  go :: BankersDequeue Window -> IO (Maybe Window)
  go q = case popFront q of
    Nothing       -> return Nothing
    Just (hd, tl) -> do
      name <- fetchName dpy hd
      if name == Just needle
      then return $ Just hd
      else do
          (_, _, next) <- queryTree dpy hd
          go $ foldl pushBack tl next


findDesktop :: Display -> Window -> IO (Maybe Window)
findDesktop = findWindowByName "Desktop"

getSize :: Display -> Drawable -> IO (Dimension, Dimension)
getSize dpy win = select <$> getGeometry dpy win
  where select (_,_,_,x,y,_,_) = (x, y)

-- ALIGNING WINDOWS

data RelativePosition = 
    NearestCorner  Dimension
  | Middle
  | OppositeCorner Dimension

data Alignment = Alignment { forX :: RelativePosition
                           , forY :: RelativePosition }

drawAligned :: Display -> Pixmap -> Window -> GC -> Alignment -> IO Bool
drawAligned dpy pixmap win gc alg = do
  (x, y) <- getSize dpy pixmap
  (a, b) <- getSize dpy win
  let mTopLeftX = corner (forX alg) x a
  let mTopLeftY = corner (forY alg) y b
  case (mTopLeftX, mTopLeftY) of
    (Just topLeftX, Just topLeftY) -> do
      copyArea dpy pixmap win gc 0 0 x y topLeftX topLeftY
      return True
    (_ , _) -> return False

  where

    corner :: RelativePosition -> Dimension -> Dimension -> Maybe Position
    corner rpos sizeSource sizeTarget = fromIntegral <$> do
      guard (sizeSource < sizeTarget)
      let diff = sizeTarget - sizeSource
      case rpos of
        NearestCorner bd  -> guard (bd < diff) >> return bd
        Middle            -> return $ diff `quot` 2
        OppositeCorner bd -> guard (bd < diff) >> return (diff - bd)

rightJustified :: Display -> Pixmap -> Window -> GC -> Dimension -> Dimension -> IO (Bool)
rightJustified dpy pix win gc bdX bdY =
    drawAligned dpy pix win gc
  $ Alignment { forX = OppositeCorner bdX
              , forY = NearestCorner  bdY }
     
