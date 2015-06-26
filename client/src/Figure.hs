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
import           Model
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
-- Make a map from PI name and orphan (Member without a PI) to
-- angle in the circle. This gets a bit messy due to the need
-- to build up intermediate maps for the invididual thrusts
piAndOrphanRanges :: Model -> (Map T.Text AngleRange, Map T.Text Double)
piAndOrphanRanges m@Model{..} =
  (\(mapsPI, mapsOrphan) -> (Map.unions mapsPI, Map.unions mapsOrphan))
  . unzip . flip map thrusts $ \(Thrust thrust) ->

  let thisPis  = map piName $ filter ((== thrust) . piThrust) pis

      (thrustT0,thrustT1) = maybe (error "piAndOrphan impossible case") id
                            (Map.lookup (Thrust thrust) (thrustAngleRanges thrusts) )

      -- Keep the names of orphan members whose thrusts match this one
      orphMayName = \thrName mem -> case memberGroup mem of
        MemberPI      _     -> Nothing
        MemberThrust tName
          | tName == thrust -> Just (memberName mem)
          | otherwise       -> Nothing

      thisOrphs = catMaybes (map (orphMayName thrust) members)

      n          = fromIntegral $ length thisPis + length thisOrphs :: Double

      thrustRngs = thrustAngleRanges thrusts
      angRange   = (thrustT0,thrustT1)

      piWedgeWid = (angleDiff angRange - ((n-1) * piWedgeSpacing figOpts)) / n

      piStarts i = i*(piWedgeWid + piWedgeSpacing figOpts) + thrustT0
      piStops  i = piStarts i + piWedgeWid
      piRanges   = map ((\i -> (piStarts i, piStops i)) . fromIntegral)
                   [0.. (length thisPis) - 1]

      piMap      = Map.fromList $ zip thisPis piRanges

      orphRanges = map ((\i -> (piStarts i, piStops i)) . fromIntegral)
                   [length thisPis .. round n]
      orphThetas = map (\(t0,t1) -> (t1+t0)/2) orphRanges
      orphMap    = Map.fromList $ zip thisOrphs orphThetas

  in  (piMap, orphMap)


------------------------------------------------------------------------------
-- Map from PI name to names of children
allPiChildren :: Model -> Map T.Text [T.Text]
allPiChildren m@Model{..} =
  let piChildren p = map memberName $
                     filter ((== Just (piName p)) . memberPI) members
  in  Map.fromList $ map (\p -> (piName p, piChildren p)) pis

------------------------------------------------------------------------------
-- Map from PI name to (Map from child name to angle)
allPiMemberAngles :: Model -> Map T.Text (Map T.Text Double)
allPiMemberAngles m@Model{..} = Map.fromList $ map (\p -> (piName p, aux p)) pis
  where (piRanges, _) = piAndOrphanRanges m
        aux p         = onePiMemberAngles m piRanges p

onePiMemberAngles :: Model -> Map T.Text AngleRange -> PI -> Map T.Text Double
onePiMemberAngles m@Model{..} piRanges p@(PI pName _) =
  let range = maybe (error "onePiMemberAngles impossible case")
              id (Map.lookup pName piRanges)
      cs    = filter ( (== Just pName) . memberPI ) members
      dt    = 1/ fromIntegral (length cs)
      fracs = take (length cs) [dt/2, 3*dt/2 ..]
  in  Map.fromList $ zipWith (\i c -> (memberName c, angleFrac range i))
      fracs cs

------------------------------------------------------------------------------
memberAngles :: Model -> Map T.Text Double
memberAngles m@Model{..} =
  let (piRngs, orphAngs)    = piAndOrphanRanges m
--  in  orphAngs <> mconcat (map (\p -> onePiMemberAngles m piRngs p) pis)
  in orphAngs <> Map.unions (Map.elems (allPiMemberAngles m))


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

piWedges :: Model -> Svg ()
piWedges m = let piRngs   = fst $ piAndOrphanRanges m :: Map T.Text AngleRange
                 piMemberAngs = allPiMemberAngles m
                 piInfo = Map.intersectionWith (\a b -> (a,b)) piRngs piMemberAngs
             in mconcat . Map.elems $ Map.mapWithKey piWedge piInfo

thrustWedge :: (Thrust, AngleRange) -> Svg ()
thrustWedge (Thrust tName, (th0, th1)) = do
  (with $ taurusWedge (TaurusWedgeSpec 0 0 r0 r1 ang width) False)
    [fill_ "hsl(100,50%,75%)", stroke_ "node"]
  textOnCircle tName [font_size_ "18"] (r0/2 + r1/2) ang Nothing
    where width = angleDiff (th0,th1)
          r0    = thrustRadiusMin figOpts
          r1    = thrustRadiusMax figOpts
          ang   = angleFrac (th0,th1) 0.5

thrustWedges :: Model -> Svg ()
thrustWedges Model{..} =
  let angMap = thrustAngleRanges thrusts
  in  forM_  (Map.toList angMap) thrustWedge

memberDot :: (T.Text, Double) -> Svg ()
memberDot (mName, mAng) =
  let x = collabRadius figOpts * cos mAng
      y = collabRadius figOpts * sin mAng
  in do
    --with (text_ (toHtml mName)) [x_ (f x), y_ (f y)]
    circle_ [ cx_ (f x)
            , cy_ (f y)
            , r_ "10", fill_ (piColor figOpts)]

memberDots :: Model -> Svg ()
memberDots m = mconcat . map memberDot . Map.toList . memberAngles $ m

collabLines :: Model -> Svg ()
collabLines m@Model{..} =
  let angs = memberAngles m
  in forM_ projects (projectLine angs)

projectLine :: Map T.Text Double -> Project -> Svg ()
projectLine angMap p@Project{..} =
  let thisAngs       = catMaybes $
                       map (flip Map.lookup angMap) projectMembers
      visLineAttrs   = [fill_ "none", stroke_ "yellow"
                       , stroke_width_ "2"]
      hidLineAttrs   = [fill_ "none", stroke_ "rgba(0,0,0,0)"
                       , stroke_width_ "5"]
      lineGroupAttrs = [id_ (textEncode projectName)
                       ,class_ "collabLine"]
      lineBase a0 a1 = highLine a0 a1 (piRadiusMin figOpts) 100
      collabLine a0 a1 = with (term "g")  lineGroupAttrs $ do
        dropShadow 0 0 2 "yellow" $ with (lineBase a0 a1) visLineAttrs
        with (lineBase a0 a1) hidLineAttrs
--        with (highLine a0 a1 (piRadiusMin figOpts) 100)
--        [fill_ "none", stroke_ "yellow"
--        , stroke_width_ "2px"
--        , id_ (textEncode (projectName))
--        , class_ "collabLine"]
  in mconcat [collabLine a0 a1 | a0 <- thisAngs , a1 <- thisAngs , a1 > a0]


------------------------------------------------------------------------------
modelSvg :: Model -> Svg ()
modelSvg m@Model{..} = do

  dropShadow 2 2 2 "black" $ g_ $ thrustWedges m
  dropShadow 2 2 4 "black" $ g_ $ do
    piWedges m
    memberDots m
  collabLines m

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
