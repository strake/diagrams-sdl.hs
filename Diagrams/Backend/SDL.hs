{-# LANGUAGE ViewPatterns #-}

module Diagrams.Backend.SDL (SDL (..), Render (..), Options (..)) where

import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Monad;
import Data.AffineSpace;
import Data.Function (on);
import Data.Maybe;
import Data.Monoid;
import Data.VectorSpace;
import Data.Word;
import Diagrams.Attributes as Diagrams;
import Diagrams.Core.Style;
import Diagrams.Core.Types;
import Diagrams.Core.V;
import Diagrams.Located;
import Diagrams.Path;
import Diagrams.Points;
import Diagrams.Segment;
import Diagrams.Transform;
import Diagrams.TwoD.Image;
import Diagrams.TwoD.Size;
import Diagrams.TwoD.Text;
import Diagrams.TwoD.Transform;
import Diagrams.TwoD.Types;
import Diagrams.TwoD.Vector;
import Graphics.UI.SDL.Color as SDL;
import Graphics.UI.SDL.Primitives;
import Graphics.UI.SDL.Rect  as SDL;
import Graphics.UI.SDL.Types as SDL;
import Graphics.UI.SDL.Video as SDL hiding (flip);
import Graphics.UI.SDL.Image as SDL;
import Graphics.UI.SDL.Rotozoomer;
import Graphics.UI.SDL.TTF;
import System.IO.Unsafe (unsafePerformIO);
import Util;

data SDL = SDL;

instance Backend SDL R2 where {
  data Render SDL R2 = SDLRender (Style R2 -> Surface -> IO ());
  type Result SDL R2 = IO ();
  data Options SDL R2 = SDLOpts {
    sdlSurface :: Surface
  };
  withStyle SDL style _ (SDLRender f) = SDLRender $ f ∘ (<> style);
  adjustDia SDL (SDLOpts s) = (,) (SDLOpts s) ∘ sized (Dims (fromIntegral $ surfaceGetWidth s) (fromIntegral $ surfaceGetHeight s)) ∘ join (maybe id (translateX ∘ negate ∘ fst) ∘ extentX) ∘ join (maybe id (translateY ∘ negate ∘ fst) ∘ extentY);
  doRender SDL (SDLOpts s) (SDLRender f) = f mempty s;
};

instance Monoid (Render SDL R2) where {
  mempty = SDLRender $ const ∘ const $ return ();
  SDLRender f `mappend` SDLRender g = SDLRender $ (liftA2 ∘ liftA2) (>>) f g;
};

instance Renderable (Path R2) SDL where {
  render SDL (Path ps) = SDLRender $ \ style s ->
    mapM_ (viewLoc & uncurry (flip pathFromTrailAt) & fixPath & concat &
           mapM_ (\ seg ->
                  uncurry4 (mapRGBA (surfaceGetPixelFormat s) `onnn` round ∘ (* 0xFF)) ((fromMaybe (1, 1, 1, 1) ∘ fmap colorToSRGBA) (getAttr style :: Maybe LineColor)) >>=
                  case seg of {
                    FLinear (unp2 -> (x1, y1)) (unp2 -> (x2, y2)) -> line s (round x1) (round y1) (round x2) (round y2);
                    FCubic (unp2 -> p1) (unp2 -> p2) (unp2 -> p3) (unp2 -> p4) -> bezier s (join (***) round <$> [p1, p2, p3, p4]) 5;
                  })) ps;
};

instance Renderable Text SDL where {
  render SDL (Text tf a xs) = SDLRender $ \ style s -> (() <$) $
    openFont (maybe (error "No font") getFont $ getAttr style) (round $ maybe 1 getFontSize $ getAttr style) >>= \ font ->
    renderUTF8Blended font xs ((fromMaybe (SDL.Color 0xFF 0xFF 0xFF) ∘ fmap sDLizeColor) (getAttr style :: Maybe LineColor)) >>=
    flip blitFullLocatedSurface s ∘ transform tf ∘ flip at (case a of {
                                                              BaselineText -> p2 (0, fromIntegral $ unsafePerformIO $ fontAscent font);
                                                              BoxAlignedText x y ->
                                                                 join (***) (negate ∘ fromIntegral) >>> (x*) *** (y*) >>> p2 $
                                                                 unsafePerformIO $ utf8Size font xs;
                                                            });
};

type instance V Surface = R2;

instance Transformable Surface where {
  transform tf = uncurry (saneRotozoom True) (apply tf >>> getRad ∘ direction &&& magnitude $ r2 (1, 0));
};

blitFullLocatedSurface :: Located Surface -> Surface -> IO Bool;
blitFullLocatedSurface = viewLoc >>> unp2 & join (***) round *** id >>> uncurry blitFullSurfaceAt;

blitFullSurfaceAt :: (Int, Int) -> Surface -> Surface -> IO Bool;
blitFullSurfaceAt (x, y) s t = blitSurface s empty t (Just $ Rect x y 0 0);

saneRotozoom :: Bool -> Double -> Double -> Surface -> Surface;
saneRotozoom smooth angle zoom s = unsafePerformIO $ rotozoom s (180*angle/pi) zoom smooth;

sDLizeColor :: Diagrams.Color c => c -> SDL.Color;
sDLizeColor = let { denorm = round ∘ (* fromIntegral (maxBound :: Word8)) } in uncurry3 (SDL.Color `onn` denorm) ∘ (\ (r, g, b, a) -> (r, g, b)) ∘ colorToSRGBA;
