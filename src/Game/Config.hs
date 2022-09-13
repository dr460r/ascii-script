module Game.Config
( 
  fireMxLvl
, fireSpreadLvl
, fireSpreadDistance

, cropPrice
, cropActons
, baseCropYield

, housePop
, housePrice
, houseActions

, backupActions
)
where

fireMxLvl :: Int
fireSpreadLvl :: Int
fireSpreadDistance :: Int

cropPrice :: Int
cropActons :: Int
baseCropYield :: Int

housePop :: Int
housePrice :: Int
houseActions :: Int

{- Fire -}
fireMxLvl = 7       -- Must be at least 5
fireSpreadLvl = 3   -- Must be less-equal tnen fireMxLvl
fireSpreadDistance = 3

{- Crop -}
cropPrice = 50
cropActons = 1
baseCropYield = 1

{- House -}
housePop = 5
housePrice = 500
houseActions = 1

{- General -}
backupActions :: Int
backupActions = 1