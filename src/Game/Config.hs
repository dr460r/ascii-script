module Game.Config
( 
  fireMxLvl
, fireSpreadLvl

, cropPrice
, cropActons
, baseCropYield

, housePop
, housePrice
, houseActions
)
where

fireMxLvl :: Int
fireSpreadLvl :: Int

cropPrice :: Int
cropActons :: Int
baseCropYield :: Int

housePop :: Int
housePrice :: Int
houseActions :: Int

{- Fire -}
fireMxLvl = 7       -- Must be at least 5
fireSpreadLvl = 3   -- Must be less-equal tnen fireMxLvl

{- Crop -}
cropPrice = 50
cropActons = 1
baseCropYield = 1

{- House -}
housePop = 5
housePrice = 500
houseActions = 1