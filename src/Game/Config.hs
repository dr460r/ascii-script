module Game.Config
( 
  fireMxLvl
, fireSpreadLvl
, fireSpreadDistance
, fireSpawnChance

, cropPrice
, cropActons
, baseCropYield

, housePop
, housePrice
, houseActions

, backupActions
, fightCost
, fightActions
)
where

fireMxLvl :: Int
fireSpreadLvl :: Int
fireSpreadDistance :: Int
fireSpawnChance :: Int

cropPrice :: Int
cropActons :: Int
baseCropYield :: Int

housePop :: Int
housePrice :: Int
houseActions :: Int

fightCost :: Int
fightActions :: Int

backupActions :: Int

{- Fire -}
-- fire properties
fireMxLvl = 7       -- Must be at least 5
fireSpreadLvl = 3   -- Must be less-equal tnen fireMxLvl
fireSpreadDistance = 3
-- spawn properties
fireSpawnChance = 20  -- Percents (%)

{- Crop -}
cropPrice = 50
cropActons = 1
baseCropYield = 1

{- House -}
housePop = 5
housePrice = 500
houseActions = 1

{- Fight -}
fightCost = 30
fightActions = 1


{- General -}
backupActions = 1


