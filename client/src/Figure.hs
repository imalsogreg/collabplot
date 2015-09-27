{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards   #-}

module Figure where

import           Control.Lens
import           Data.Fixed  (mod')
import           Data.Foldable
import           Data.Traversable
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe  (catMaybes)
import           Data.Monoid
import qualified Data.Text   as T
import           Data.UUID
import           Lucid.Svg
import           Reflex.Dom
import           CollabTypes
import           Utils
import           Primitives
import           Shadow

data FigOpts = FigOpts {
    piMinWedgeWidth    :: Double
  , piWedgeSpacing     :: Double
  , thrustWedgeSpacing :: Double
  , piRadiusMin        :: Double
  , piRadiusMax        :: Double
  , piColor            :: T.Text
  , thrustRadiusMin    :: Double
  , thrustRadiusMax    :: Double
  , thrustThetaOffset  :: Double
  , collabRadius       :: Double
  }

figOpts :: FigOpts
figOpts = FigOpts {
    piMinWedgeWidth    = 0.1
  , piWedgeSpacing     = 0.05
  , thrustWedgeSpacing = 0.1
  , piRadiusMin        = 100
  , piRadiusMax        = 250
  , piColor            = "hsl(100,50%,50%)"
  , thrustRadiusMin    = 255
  , thrustRadiusMax    = 320
  , thrustThetaOffset  = 0
  , collabRadius       = 250
  }


------------------------------------------------------------------------------
thrustAngleRanges :: [Thrust] -> Map Thrust AngleRange
thrustAngleRanges thrusts = Map.fromList $ zipWith f [0..] thrusts
  where f i thrustName = (thrustName, (theta0 i, theta1 i))
        n              = fromIntegral (length thrusts) :: Double
        wedgeInterval  = (2*pi) / n :: Double
        theta0 i       = fromIntegral i * wedgeInterval
        theta1 i       = theta0 i + wedgeInterval
                         - thrustWedgeSpacing figOpts

------------------------------------------------------------------------------
-- Make a map from PI name to
-- angle in the circle. This gets a bit messy due to the need
-- to build up intermediate maps for the invididual thrusts
piRanges :: Model -> (Map PI AngleRange)
piRanges m@Model{..} =
  Map.unions
  . flip map _modelThrusts $ \t@Thrust{..} ->

  let

      (thrustT0,thrustT1) = maybe (error "piAndOrphan impossible case") id
                            (Map.lookup t (thrustAngleRanges _modelThrusts) )

      n          = fromIntegral $ length _thrustPIs :: Double

      thrustRngs = thrustAngleRanges _modelThrusts
      angRange   = (thrustT0,thrustT1)

      piWedgeWid = (angleDiff angRange - ((n-1) * piWedgeSpacing figOpts)) / n

      piStarts i = i*(piWedgeWid + piWedgeSpacing figOpts) + thrustT0
      piStops  i = piStarts i + piWedgeWid
      piRanges   = map ((\i -> (piStarts i, piStops i)) . fromIntegral)
                   [0.. (length _thrustPIs) - 1]

      piMap      = Map.fromList $ zip _thrustPIs piRanges

  in  piMap


-- ------------------------------------------------------------------------------
-- -- Map from PI name to names of children
-- allPiChildren :: Model -> Map T.Text [T.Text]
-- allPiChildren m@Model{..} =
--   let piChildren p = map _memberName $ _piMembers p
--   in  Map.fromList $ map (\p -> (_piName p, piChildren p))
--       (concatMap _thrustPIs _modelThrusts)

------------------------------------------------------------------------------
-- Map from PI to (Map from child to angle)
allPiMemberAngles :: Model -> Map PI (Map Member Double)
allPiMemberAngles m@Model{..} = Map.fromList $ map (\p -> (p, aux p)) (concatMap _thrustPIs _modelThrusts)
  where aux p    = onePiMemberAngles m (piRanges m) p

onePiMemberAngles :: Model -> Map PI AngleRange -> PI -> Map Member Double
onePiMemberAngles m@Model{..} piRanges p@PI{..} =
  let range = maybe (error "onePiMemberAngles impossible case")
              id (Map.lookup p piRanges)
      cs    = _piMembers
      dt    = 1/ fromIntegral (length cs)
      fracs = take (length cs) [dt/2, 3*dt/2 ..]
  in  Map.fromList $ zipWith (\i c -> (c, angleFrac range i))
      fracs cs

------------------------------------------------------------------------------
memberAngles :: Model -> Map Member Double
memberAngles m@Model{..} =
  let piRngs = piRanges m
  in Map.unions (Map.elems (allPiMemberAngles m))


piWedge :: MonadWidget t m
        => Dynamic t [UUID]
        -> PI
        -> Dynamic t (AngleRange, Map Member Double)
        -> m (Event t (Model -> Model))
piWedge dynBrights p@PI{..} dynChildAngs = do
  let r0 = piRadiusMin figOpts
      r1 = piRadiusMax figOpts
  tws <- forDyn dynChildAngs $ \(rng@(th0,th1), childAngs) ->
           TaurusWedgeSpec 0 0 (piRadiusMin figOpts) (piRadiusMax figOpts)
                           (angleFrac rng 0.5) (angleDiff rng)
  (g,_) <- svgEl' "g" $ do
    taurusWedge tws False (constDyn $ "fill"   =: "hsl(100,50%,50%)"
                                    <> "stroke" =: "none")
    dynAng <- forDyn dynChildAngs $ \(rng,_) -> angleFrac rng 0.5
    textOnCircle (constDyn $ T.unpack _piName) (constDyn $ "font-size" =: "10pt")
                  (constDyn (r0/2 + r1/2)) dynAng (constDyn $ Just (r1 - r0))
  let c = domEvent Click g
  return $ (\m -> m & set modelSelections [_piID]
                    & set modelFocus (Just p)) <$ c

foldThroughMap :: Map a (Model -> Model) -> (Model -> Model)
foldThroughMap m = flip (foldr ($)) . Map.elems $ m

piWedges :: MonadWidget t m => Dynamic t Model -> m (Event t (Model -> Model))
piWedges m = do
  piRngs <- mapDyn piRanges m
  brights <- mapDyn _modelSelections m
  piMemberAngs <- mapDyn allPiMemberAngles m
  piInfo <- combineDyn (Map.intersectionWith (\a b -> (a,b)))
            piRngs piMemberAngs
  e <- listViewWithKey piInfo (piWedge brights)
  return $ fmap foldThroughMap e


thrustWedge :: MonadWidget t m
            => Thrust
            -> Dynamic t AngleRange
            -> m (Event t ())
thrustWedge thr dynRng = do
  let r0 = thrustRadiusMin figOpts
      r1 = thrustRadiusMax figOpts
      twsAttrs = "fill" =: "hsl(100,50%,75%)" <> "stroke" =: "none"
  dWidth   <- forDyn dynRng angleDiff
  dAng     <- forDyn dynRng $ \r -> angleFrac r 0.5

  tws      <- forDyn dynRng $ \r ->
                TaurusWedgeSpec 0 0 r0 r1 (angleFrac r 0.5) (angleDiff r)
  taurusWedge tws False (constDyn twsAttrs)

  textOnCircle (constDyn (T.unpack $ _thrustName thr))
               (constDyn $ "font-size" =: "6pt")
               (constDyn $ r0/2 + r1/2) dAng (constDyn Nothing)
  return never



thrustWedges :: MonadWidget t m => Dynamic t Model -> m ()
thrustWedges m = do
  angMap <- mapDyn (thrustAngleRanges . _modelThrusts) m
  listViewWithKey angMap thrustWedge
  return ()

collabLines :: MonadWidget t m
            => Dynamic t Model
            -> m (Event t (Model -> Model))
collabLines m = do
  angs  <- mapDyn memberAngles m
  projs <- mapDyn _modelProjects m
  projsAngs <- combineDyn projAngles projs angs
  events <- listViewWithKey projsAngs projectLine
  return $ fmap (flip (foldr ($)) . Map.elems) events

projAngles :: [Project] -> Map Member Double -> Map Project [(Member,Double)]
projAngles projs memberAngles =
  Map.fromList $ flip map projs $ \p@Project{..} ->
    let projAngs = Map.toList $ Map.filterWithKey
                                (\m _ -> m `elem` _projectMembers)
                                memberAngles
    in (p,projAngs)
------------------------------------------------------------------------------
projectLine :: MonadWidget t m
            => Project
            -> Dynamic t [(Member,Double)]
            -> m (Event t (Model -> Model))
projectLine proj dynAngs = do
  let findAngs angs p = catMaybes $ map (flip Map.lookup angs) (_projectMembers p)
  thisAngs     <- forDyn dynAngs (map snd)
  thisMembers  <- forDyn dynAngs (map fst)
  angPairs     <- forDyn thisAngs $ \xs -> [(x,y) | x <- xs , y <- xs , y > x]
  let visLineAttrs = "fill"         =: "none"
                  <> "stroke"       =: "yellow"
                  <> "stroke-width" =: "2"
      hidLineAttrs = "fill"         =: "none"
                  <> "stroke"       =: "rgba(0,0,0,0)"
                  <> "stroke-width" =: "5"

  let gAttrs = st "id"    =: T.unpack (textEncode (_projectName proj))
            <>    "class" =: "collabLine"

  (g,_) <- svgElDynAttr' "g" (constDyn gAttrs) $ simpleList angPairs $ \dynPair -> do
    let glowParams = ShadowParams (constDyn 0) (constDyn 0) (constDyn 3)
                     (constDyn "rgba(255,255,0,1)")
    elShadow glowParams $ highLine dynPair (constDyn $ piRadiusMin figOpts) (constDyn 100) (constDyn visLineAttrs)
    highLine dynPair (constDyn $ piRadiusMin figOpts) (constDyn 100) (constDyn hidLineAttrs)
  let c = domEvent Click g
  return $ set modelSelections [_projectID proj] <$ c


modelSvg :: MonadWidget t m => Dynamic t Model -> m (Event t (Model -> Model))
modelSvg m = do

 elShadow defShadowParams $ thrustWedges m
 piEvents <- elShadow defShadowParams $ piWedges m
 liEvents <- collabLines m

 return $ leftmost [piEvents, liEvents]

------------------------------------------------------------------------------
data TextWedge = TextWedge {
    twText       :: T.Text
  , twTheta      :: Double
  , twWidth      :: Double
  , twInner      :: Double
  , twOuter      :: Double
  , twTextAttrs  :: [Attribute]
  , twBkgndAttrs :: [Attribute]
  } deriving (Eq, Show)


-- ------------------------------------------------------------------------------
-- textWedge :: TextWedge -> Svg ()
-- textWedge TextWedge{..} = g_ $ do
--   (with $ taurusWedge
--    (TaurusWedgeSpec 0 0
--     twInner twOuter twTheta twWidth) False)
--     twBkgndAttrs
--   textOnCircle twText twTextAttrs (twInner/2 + twOuter/2) twTheta Nothing
--
-- textWedge' :: TextWedge -> Svg ()
-- textWedge' TextWedge{..} = g_ $ do
--   (with $ taurusWedge
--    (TaurusWedgeSpec 0 0
--     twInner twOuter twTheta twWidth) True)
--     twBkgndAttrs
--   textOnCircle twText twTextAttrs (twOuter/2 + twInner/2)
--     twTheta (Just $ twOuter - twInner)
