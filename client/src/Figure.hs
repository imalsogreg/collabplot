{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards   #-}

module Figure where

import           Data.Fixed  (mod')
import           Data.Foldable
import           Data.Traversable
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe  (catMaybes)
import           Data.Monoid
import qualified Data.Text   as T
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
    piMinWedgeWidth = 0.1
  , piWedgeSpacing = 0.05
  , thrustWedgeSpacing = 0.1
  , piRadiusMin = 100
  , piRadiusMax = 250
  , piColor     = "hsl(100,50%,50%)"
  , thrustRadiusMin = 255
  , thrustRadiusMax = 320
  , thrustThetaOffset = 0
  , collabRadius = 250
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
piRanges :: Model -> (Map T.Text AngleRange)
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

      piMap      = Map.fromList $ zip (map _piName _thrustPIs) piRanges

  in  piMap


------------------------------------------------------------------------------
-- Map from PI name to names of children
allPiChildren :: Model -> Map T.Text [T.Text]
allPiChildren m@Model{..} =
  let piChildren p = map _memberName $ _piMembers p
  in  Map.fromList $ map (\p -> (_piName p, piChildren p))
      (concatMap _thrustPIs _modelThrusts)

------------------------------------------------------------------------------
-- Map from PI name to (Map from child name to angle)
allPiMemberAngles :: Model -> Map T.Text (Map T.Text Double)
allPiMemberAngles m@Model{..} = Map.fromList $ map (\p -> (_piName p, aux p)) (concatMap _thrustPIs _modelThrusts)
  where aux p    = onePiMemberAngles m (piRanges m) p

onePiMemberAngles :: Model -> Map T.Text AngleRange -> PI -> Map T.Text Double
onePiMemberAngles m@Model{..} piRanges p@PI{..} =
  let range = maybe (error "onePiMemberAngles impossible case")
              id (Map.lookup _piName piRanges)
      --allPi = concatMap _thrustPIs _modelThrusts
      --allMs = concatMap _piMembers allPi
      cs    = _piMembers
      dt    = 1/ fromIntegral (length cs)
      fracs = take (length cs) [dt/2, 3*dt/2 ..]
  in  Map.fromList $ zipWith (\i c -> (_memberName c, angleFrac range i))
      fracs cs

------------------------------------------------------------------------------
memberAngles :: Model -> Map T.Text Double
memberAngles m@Model{..} =
  let piRngs = piRanges m
  in Map.unions (Map.elems (allPiMemberAngles m))

piWedge :: T.Text -> (AngleRange, Map T.Text Double) -> Svg ()
piWedge pName (rng@(th0,th1), childAngs) =
  let width = angleDiff rng
      ang   = angleFrac    rng 0.5
      r0    = piRadiusMin figOpts
      r1    = piRadiusMax figOpts
  in do
    (with $ taurusWedge (TaurusWedgeSpec 0 0 r0 r1 ang width) False)
      [fill_ "hsl(100,50%,50%)", stroke_ "none"]
    textOnCircle pName [font_size_ "18"] (r0/2 + r1/2) ang (Just $ r1 - r0)

piWedge' :: MonadWidget t m
         => T.Text
         -> Dynamic t (AngleRange, Map T.Text Double)
         -> m (Event t ())
piWedge' nm dynChildAngs = do
  let r0 = piRadiusMin figOpts
      r1 = piRadiusMax figOpts
  tws <- forDyn dynChildAngs $ \(rng@(th0,th1), childAngs) ->
           TaurusWedgeSpec 0 0 (piRadiusMin figOpts) (piRadiusMax figOpts)
                           (angleFrac rng 0.5) (angleDiff rng)
  taurusWedge' tws False (constDyn $ "fill"   =: "hsl(100,50%,50%)"
                                  <> "stroke" =: "none")
  dynAng <- forDyn dynChildAngs $ \(rng,_) -> angleFrac rng 0.5
  textOnCircle' (constDyn $ T.unpack nm) (constDyn $ "font-size" =: "18pt")
                (constDyn (r0/2 + r1/2)) dynAng (constDyn $ Just (r1 - r0))
  return never

piWedges :: Model -> Svg ()
piWedges m = let piRngs   = piRanges m :: Map T.Text AngleRange
                 piMemberAngs = allPiMemberAngles m
                 piInfo = Map.intersectionWith (\a b -> (a,b)) piRngs piMemberAngs
             in mconcat . Map.elems $ Map.mapWithKey piWedge piInfo

piWedges' :: MonadWidget t m => Dynamic t Model -> m ()
piWedges' m = do
  piRngs <- mapDyn piRanges m
  piMemberAngs <- mapDyn allPiMemberAngles m
  piInfo <- combineDyn (Map.intersectionWith (\a b -> (a,b)))
            piRngs piMemberAngs
  listViewWithKey piInfo piWedge'
  return ()

thrustWedge :: (Thrust, AngleRange) -> Svg ()
thrustWedge (Thrust{..}, (th0, th1)) = do
  (with $ taurusWedge (TaurusWedgeSpec 0 0 r0 r1 ang width) False)
    [fill_ "hsl(100,50%,75%)", stroke_ "node"]
  textOnCircle _thrustName [font_size_ "18"] (r0/2 + r1/2) ang Nothing
    where width = angleDiff (th0,th1)
          r0    = thrustRadiusMin figOpts
          r1    = thrustRadiusMax figOpts
          ang   = angleFrac (th0,th1) 0.5

thrustWedge' :: MonadWidget t m
             => Thrust
             -> Dynamic t AngleRange
             -> m (Event t ())
thrustWedge' thr dynRng = do
  let r0 = thrustRadiusMin figOpts
      r1 = thrustRadiusMax figOpts
      twsAttrs = "fill" =: "hsl(100,50%,75%)" <> "stroke" =: "none"
  dWidth   <- forDyn dynRng angleDiff
  dAng     <- forDyn dynRng $ \r -> angleFrac r 0.5

  tws      <- forDyn dynRng $ \r ->
                TaurusWedgeSpec 0 0 r0 r1 (angleFrac r 0.5) (angleDiff r)
  taurusWedge' tws False (constDyn twsAttrs)

  textOnCircle' (constDyn (T.unpack $ _thrustName thr))
                (constDyn $ "font-size" =: "18pt")
                (constDyn $ r0/2 + r1/2) dAng (constDyn Nothing)
  return never


thrustWedges :: Model -> Svg ()
thrustWedges Model{..} =
  let angMap = thrustAngleRanges _modelThrusts
  in  forM_  (Map.toList angMap) thrustWedge

thrustWedges' :: MonadWidget t m => Dynamic t Model -> m ()
thrustWedges' m = do
  angMap <- mapDyn (thrustAngleRanges . _modelThrusts) m
  listViewWithKey angMap thrustWedge'
  return ()

memberDot :: (T.Text, Double) -> Svg ()
memberDot (mName, mAng) =
  let x = collabRadius figOpts * cos mAng
      y = collabRadius figOpts * sin mAng
  in do
    circle_ [ cx_ (f x)
            , cy_ (f y)
            , r_ "10", fill_ (piColor figOpts)]

memberDots :: Model -> Svg ()
memberDots m = mconcat . map memberDot . Map.toList . memberAngles $ m

collabLines :: Model -> Svg ()
collabLines m@Model{..} =
  let angs = memberAngles m
  in forM_ _modelProjects (projectLine angs)

collabLines' :: MonadWidget t m => Dynamic t Model -> m ()
collabLines' m = do
  angs <- mapDyn memberAngles m
  projs <- mapDyn _modelProjects m
  simpleList projs (projectLine' angs)
  return ()

projectLine :: Map T.Text Double -> Project -> Svg ()
projectLine angMap p@Project{..} =
  let thisAngs       = catMaybes $
                       map (flip Map.lookup angMap)
                       (map _memberName _projectMembers)
      visLineAttrs   = [fill_ "none", stroke_ "yellow"
                       , stroke_width_ "2"]
      hidLineAttrs   = [fill_ "none", stroke_ "rgba(0,0,0,0)"
                       , stroke_width_ "5"]
      lineGroupAttrs = [id_ (textEncode _projectName)
                       ,class_ "collabLine"]
      lineBase a0 a1 = highLine a0 a1 (piRadiusMin figOpts) 100
      collabLine a0 a1 = with (term "g")  lineGroupAttrs $ do
        dropShadow 0 0 2 "yellow" $ with (lineBase a0 a1) visLineAttrs
        with (lineBase a0 a1) hidLineAttrs
  in mconcat [collabLine a0 a1 | a0 <- thisAngs
                               , a1 <- thisAngs
                               , a1 > a0]


------------------------------------------------------------------------------
projectLine' :: MonadWidget t m
             => Dynamic t (Map T.Text Double)
             -> Dynamic t Project
             -> m ()
projectLine' dynAngs dynProj = do
  let findAngs angs p = catMaybes $ map (flip Map.lookup angs) (map _memberName $ _projectMembers p)
  thisAngs     <- combineDyn findAngs dynAngs dynProj
  angPairs     <- forDyn thisAngs $ \xs -> [(x,y) | x <- xs , y <- xs , y > x]
  let visLineAttrs = "fill"         =: "none"
                  <> "stroke"       =: "yellow"
                  <> "stroke-width" =: "2"
      hidLineAttrs = "fill"         =: "none"
                  <> "stroke"       =: "rgba(0,0,0,0)"
                  <> "stroke-width" =: "5"

  gAttrs <- forDyn dynProj $ \p -> "id"    =: T.unpack (textEncode (_projectName p))
                                <> "class" =: "collabLine"

  simpleList angPairs $ \dynPair -> do
    svgElDynAttr "g" gAttrs $ do
      -- TODO shadow params
      elShadow' defShadowParams $ highLine' dynPair (constDyn $ piRadiusMin figOpts) (constDyn 100) (constDyn visLineAttrs)
      highLine' dynPair (constDyn $ piRadiusMin figOpts) (constDyn 100) (constDyn hidLineAttrs)
  return ()


------------------------------------------------------------------------------
modelSvg :: Model -> Svg ()
modelSvg m@Model{..} = do

  dropShadow 2 2 2 "black" $ g_ $ thrustWedges m
  dropShadow 2 2 4 "black" $ g_ $ do
    piWedges m
    memberDots m
  collabLines m

modelSvg' :: MonadWidget t m => Dynamic t Model -> m ()
modelSvg' m = do

 elShadow' defShadowParams $ thrustWedges' m
 elShadow' defShadowParams $ piWedges' m
 collabLines' m

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


------------------------------------------------------------------------------
textWedge :: TextWedge -> Svg ()
textWedge TextWedge{..} = g_ $ do
  (with $ taurusWedge
   (TaurusWedgeSpec 0 0
    twInner twOuter twTheta twWidth) False)
    twBkgndAttrs
  textOnCircle twText twTextAttrs (twInner/2 + twOuter/2) twTheta Nothing

textWedge' :: TextWedge -> Svg ()
textWedge' TextWedge{..} = g_ $ do
  (with $ taurusWedge
   (TaurusWedgeSpec 0 0
    twInner twOuter twTheta twWidth) True)
    twBkgndAttrs
  textOnCircle twText twTextAttrs (twOuter/2 + twInner/2)
    twTheta (Just $ twOuter - twInner)
