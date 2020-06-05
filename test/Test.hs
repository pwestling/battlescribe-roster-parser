{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}


import Test.Hspec
import Types
import RosterProcessing (processRoster)
import           Codec.Archive.Zip
import qualified Data.ByteString.Lazy                 as B
import qualified Data.ByteString.Lazy.Char8           as C8
import           Data.Aeson
import           Data.Data
import           Data.Generics


{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

unzipRoster :: String -> IO String
unzipRoster fileName = do
  roster <- B.readFile ("test_rosters/" ++ fileName ++ ".rosz")
  let zipEntries = zEntries $ toArchive roster
  let onlyEntry = head zipEntries
  return (C8.unpack $ fromEntry onlyEntry)

processUnitWithOptions ::  ScriptOptions -> String -> IO Unit
processUnitWithOptions options fileName  = do
   roster <- unzipRoster fileName
   units <- processRoster options roster "abc" 
   shouldSatisfy (fmap _unitName units) (\u -> length u == 1)
   return (head units)


processUnit :: String -> IO Unit
processUnit = processUnitWithOptions (ScriptOptions True Nothing Nothing False False False Nothing Nothing)

processUnits :: String -> Int ->  IO [Unit]
processUnits = processUnitsWithOptions (ScriptOptions True Nothing Nothing False False False Nothing Nothing)

processUnitsWithOptions :: ScriptOptions -> String -> Int ->  IO [Unit]
processUnitsWithOptions options fileName count = do
   roster <- unzipRoster fileName
   units <- processRoster options roster "abc" 
   shouldSatisfy (fmap _unitName units) (\u -> length u == count)
   return units

hasStat :: ModelGroup -> (Stats -> String) -> String -> IO ()
hasStat unit statGetter val = do
    let maybeStats =  _stats unit
    case maybeStats of
      Just stats -> statGetter stats `shouldBe` val
      Nothing -> expectationFailure "Model should have stats"

hasWeaponNamed :: ModelGroup -> String -> IO ()
hasWeaponNamed unit weaponName = do
    fmap _weaponName (_weapons unit) `shouldContain` [weaponName]

hasAbilityNamed :: ModelGroup -> String -> IO ()
hasAbilityNamed unit abilityName = do
    fmap _abilityName (_abilities unit) `shouldContain` [abilityName]

hasWeapons :: ModelGroup -> [String] -> IO ()
hasWeapons unit weapons = do
  print $ fmap _weaponName  (_weapons unit)
  mapM_ (hasWeaponNamed unit) weapons
  length (_weapons unit) `shouldBe` length weapons

isEquivalent :: ModelGroup -> ModelGroup -> IO ()
isEquivalent m1 m2 = do
  let cleanWeapons mg = mg { _weapons = fmap (\x -> x {_id = ""} :: Weapon) (_weapons mg)}
  let cleanUpgrades mg = mg {_upgrades = fmap (\x -> x {_id = ""} :: Upgrade) (_upgrades mg)}
  let cleanAbilities mg = mg {_abilities = fmap (\x -> x {_id = ""} :: Ability) (_abilities mg)}
  let clean = cleanWeapons . cleanAbilities . cleanUpgrades
  let m1clean = clean $ m1 {_modelGroupId = ""}
  let m2clean = clean $ m2 {_modelGroupId = ""}
  m1clean `shouldBe` m2clean

areEquivalent :: [ModelGroup] -> IO ()
areEquivalent (m : ms) = mapM_ (isEquivalent m) ms
areEquivalent _ = pure ()

hasAbilities :: ModelGroup -> [String] -> IO ()
hasAbilities unit abilities = do
  print $ fmap _abilityName  (_abilities unit)
  mapM_ (hasAbilityNamed unit) abilities
  length (_abilities unit) `shouldBe` length abilities

hasUnitAbilityNamed :: Unit -> String -> IO ()
hasUnitAbilityNamed unit abilityName = do
    fmap _abilityName (_unitAbilities unit) `shouldContain` [abilityName]

hasUnitAbilities :: Unit -> [String] -> IO ()
hasUnitAbilities unit abilities = do
  print $ fmap _abilityName  (_unitAbilities unit)
  mapM_ (hasUnitAbilityNamed unit) abilities
  length (_unitAbilities unit) `shouldBe` length abilities

hasCount :: ModelGroup -> Int -> IO ()
hasCount group count = do
  _modelCount group `shouldBe` count

hasGroups :: Unit -> Int -> IO()
hasGroups unit count = do
    print $ fmap _name (_subGroups unit)
    shouldSatisfy (fmap _name (_subGroups unit)) (\u -> length u == count)

printUnits :: [Unit] -> IO ()
printUnits units = putStrLn $ C8.unpack $ encode cleanUnits where
  cleanUnits = fmap cleanUnit units
  cleanUnit unit = unit { _script = ""}

main :: IO ()
main = hspec $ do
  describe "RosterProcessing" $ do
    describe "processRoster" $ do
      it "assigns Bolt Rifles to all Intercessors" $ do
        unit <- processUnit "BoltRifleIntercessors"
        unit `hasGroups` 2
        let [intercessor, sargeant] = _subGroups unit
        intercessor `hasCount` 4
        sargeant `hasCount` 1
        intercessor `hasWeapons` ["Bolt rifle", "Bolt pistol", "Frag grenade", "Krak grenade"]
        sargeant `hasWeapons` ["Bolt rifle", "Power sword", "Bolt pistol", "Frag grenade", "Krak grenade"]
      it "assigns Stalker Bolt Rifles to all Intercessors" $ do
        unit <- processUnit "StalkerBoltRifleIntercessors"
        unit `hasGroups` 2
        let [intercessor, sargeant] = _subGroups unit
        intercessor `hasCount` 4
        sargeant `hasCount` 1
        intercessor `hasWeapons` ["Stalker Bolt Rifle", "Bolt pistol", "Frag grenade", "Krak grenade"]
        sargeant `hasWeapons` ["Stalker Bolt Rifle", "Power sword", "Bolt pistol", "Frag grenade", "Krak grenade"]
      it "excludes grenades" $ do
        unit <- processUnitWithOptions (ScriptOptions True Nothing Nothing True False False Nothing Nothing) "AutoBoltRifleIntercessors"
        unit `hasGroups` 2
        let [intercessor, sargeant] = _subGroups unit
        intercessor `hasCount` 4
        sargeant `hasCount` 1
        intercessor `hasWeapons` ["Auto Bolt Rifle", "Bolt pistol"]
        sargeant `hasWeapons` ["Auto Bolt Rifle", "Power sword", "Bolt pistol"]
      it "excludes sidearms" $ do
        unit <- processUnitWithOptions (ScriptOptions True Nothing Nothing False True False Nothing Nothing) "AutoBoltRifleIntercessors"
        unit `hasGroups` 2
        let [intercessor, sargeant] = _subGroups unit
        intercessor `hasCount` 4
        sargeant `hasCount` 1
        intercessor `hasWeapons` ["Auto Bolt Rifle", "Frag grenade", "Krak grenade"]
        sargeant `hasWeapons` ["Auto Bolt Rifle", "Power sword", "Frag grenade", "Krak grenade"]
      it "excludes Abilities" $ do
        unit <- processUnitWithOptions (ScriptOptions True Nothing Nothing False False True Nothing Nothing) "AutoBoltRifleIntercessors"
        unit `hasGroups` 2
        let [intercessor, sargeant] = _subGroups unit
        intercessor `hasCount` 4
        sargeant `hasCount` 1
        intercessor `hasWeapons` ["Auto Bolt Rifle", "Bolt pistol", "Frag grenade", "Krak grenade"]
        sargeant `hasWeapons` ["Auto Bolt Rifle", "Power sword", "Bolt pistol", "Frag grenade", "Krak grenade"]
        intercessor `hasAbilities` []
        sargeant `hasAbilities` []
      it "assigns Auto Bolt Rifles to all Intercessors" $ do
        unit <- processUnit "AutoBoltRifleIntercessors"
        unit `hasGroups` 2
        let [intercessor, sargeant] = _subGroups unit
        intercessor `hasCount` 4
        sargeant `hasCount` 1
        intercessor `hasWeapons` ["Auto Bolt Rifle", "Bolt pistol", "Frag grenade", "Krak grenade"]
        sargeant `hasWeapons` ["Auto Bolt Rifle", "Power sword", "Bolt pistol", "Frag grenade", "Krak grenade"]
      it "creates Scarab Occult correctly" $ do
        unit <- processUnit "ScarabOccultTerminators"
        unit `hasGroups` 3
        let [terminators, heavyWeapon, sorcerer] = _subGroups unit
        terminators `hasCount` 3
        heavyWeapon `hasCount` 1
        sorcerer `hasCount` 1
        terminators `hasWeapons` ["Inferno Combi-bolter", "Power sword"]
        heavyWeapon `hasWeapons` ["Inferno Combi-bolter", "Power sword", "Hellfyre Missile Rack"]
        sorcerer `hasWeapons` ["Inferno Combi-bolter", "Force stave"]
      it "creates SM Tactical Squad correctly" $ do
        unit <- processUnit "TacticalSquad"
        unit `hasGroups` 4
        let [tacticals, sargeant, heavy, special] = _subGroups unit
        tacticals `hasCount` 5
        heavy `hasCount` 1
        special `hasCount` 1
        sargeant `hasCount` 1
        tacticals `hasWeapons` ["Boltgun", "Bolt pistol", "Frag grenade", "Krak grenade"]
        heavy `hasWeapons` ["Heavy bolter", "Bolt pistol", "Frag grenade", "Krak grenade"]
        special `hasWeapons` ["Meltagun", "Bolt pistol", "Frag grenade", "Krak grenade"]
        sargeant `hasWeapons` ["Boltgun", "Bolt pistol", "Frag grenade", "Krak grenade"]
      it "creates Hellblasters correctly" $ do
        unit <- processUnit "Hellblasters"
        unit `hasGroups` 2
        let [blasters, sargeant] = _subGroups unit
        blasters `hasCount` 4
        sargeant `hasCount` 1
        blasters `hasWeapons` ["Plasma incinerator, Standard","Plasma incinerator, Supercharge", "Bolt pistol", "Frag grenade", "Krak grenade"]
        sargeant `hasWeapons` ["Plasma incinerator, Standard","Plasma incinerator, Supercharge", "Bolt pistol", "Frag grenade", "Krak grenade"]
      it "creates Assault Hellblasters correctly" $ do
        unit <- processUnit "AssaultHellblasters"
        unit `hasGroups` 2
        let [blasters, sargeant] = _subGroups unit
        blasters `hasCount` 4
        sargeant `hasCount` 1
        blasters `hasWeapons` ["Assault Plasma Incinerator, Standard","Assault Plasma Incinerator, Supercharged", "Bolt pistol", "Frag grenade", "Krak grenade"]
        sargeant `hasWeapons` ["Assault Plasma Incinerator, Standard","Assault Plasma Incinerator, Supercharged", "Bolt pistol", "Frag grenade", "Krak grenade"]
      it "creates Heavy Hellblasters correctly" $ do
        unit <- processUnit "HeavyHellblasters"
        unit `hasGroups` 2
        let [blasters, sargeant] = _subGroups unit
        blasters `hasCount` 4
        sargeant `hasCount` 1
        blasters `hasWeapons` ["Heavy Plasma Incinerator, Standard","Heavy Plasma Incinerator, Supercharged", "Bolt pistol", "Frag grenade", "Krak grenade"]
        sargeant `hasWeapons` ["Heavy Plasma Incinerator, Standard","Heavy Plasma Incinerator, Supercharged", "Bolt pistol", "Frag grenade", "Krak grenade"]
      it "creates Genestealers correctly" $ do
        unit <- processUnit "Genestealers"
        unit `hasGroups` 2
        let [normal, maws] = _subGroups unit
        normal `hasCount` 6
        maws `hasCount` 4
        normal `hasWeapons` ["Rending Claws"]
        maws `hasWeapons` ["Rending Claws", "Acid Maw"]
      it "creates Broodlord correctly" $ do
        unit <- processUnit "Broodlord"
        unit `hasGroups` 1
        let [lord] = _subGroups unit
        lord `hasCount` 1
        lord `hasWeapons` ["Monstrous Rending Claws"]
      it "creates TalonMaster correctly" $ do
        unit <- processUnit "TalonMaster"
        unit `hasGroups` 1
        let [talonmaster] = _subGroups unit
        talonmaster `hasCount` 1
        talonmaster `hasWeapons` ["Power sword","Twin assault cannon","Twin heavy bolter"]
      it "creates Aggressors correctly" $ do
        unit <- processUnit "Aggressors"
        unit `hasGroups` 2
        let [squadies, sargeant] = _subGroups unit
        squadies `hasCount` 2
        squadies `hasWeapons` ["Auto Boltstorm Gauntlets (Shooting)", "Auto Boltstorm Gauntlets (Melee)","Fragstorm Grenade Launcher"]
        sargeant `hasCount` 1
        sargeant `hasWeapons` ["Auto Boltstorm Gauntlets (Shooting)", "Auto Boltstorm Gauntlets (Melee)","Fragstorm Grenade Launcher"]
      it "creates BigMeks correctly" $ do
        units <- processUnits "BigMeks" 5
        printUnits units
        let [oiler, noOiler, forceField, megaarmor, legends] = units
        forceField `hasGroups` 1
        let [forceFieldModel] = _subGroups forceField
        forceFieldModel `hasCount` 1
        forceFieldModel `hasWeapons` ["Choppa", "Slugga", "Stikkbomb"] 
        forceFieldModel `hasAbilities` ["Big Mekaniak","Kustom Force Field", "'Ere We Go!", "Mob Rule","Dakka Dakka Dakka"]
        noOiler `hasGroups` 1
        let [noOilerModel] = _subGroups noOiler
        noOilerModel `hasCount` 1
        noOilerModel `hasWeapons` ["Shokk Attack Gun", "Stikkbomb", "Killsaw"] 
        noOilerModel `hasAbilities` ["Big Mekaniak","'Ere We Go!", "Mob Rule","Dakka Dakka Dakka"]
        oiler `hasGroups` 2
        let [grot, mek] = _subGroups oiler
        mek `hasCount` 1
        mek `hasWeapons` ["Shokk Attack Gun", "Stikkbomb", "Killsaw"] 
        mek `hasAbilities` ["Big Mekaniak","'Ere We Go!", "Mob Rule","Dakka Dakka Dakka"]
        grot `hasCount` 1
        grot `hasWeapons` []
        grot `hasAbilities` ["Grot Oiler"]
        legends `hasGroups` 2
        let [grotL, mekL] = _subGroups legends
        mekL `hasCount` 1
        mekL `hasWeapons` ["Choppa", "Slugga", "Stikkbomb"] 
        mekL `hasAbilities` ["Big Mekaniak", "'Ere We Go!", "Mob Rule"]
        grotL `hasCount` 1
        grotL `hasWeapons` []
        grotL `hasAbilities` ["Grot Oiler"]
        megaarmor `hasGroups` 2
        let [grotA, mekA] = _subGroups megaarmor
        mekA `hasCount` 1
        mekA `hasWeapons` ["Kustom Mega-blasta", "Power Klaw"]
        mekA `hasAbilities` ["Big Mekaniak", "'Ere We Go!", "Mob Rule","Dakka Dakka Dakka"]
        grotA `hasCount` 1
        grotA `hasWeapons` []
        grotA `hasAbilities` ["Grot Oiler"]
      it "creates Painboy correctly" $ do
        unit <- processUnit "Painboy"
        unit `hasGroups` 2
        let [grot, painboy] = _subGroups unit
        grot `hasCount` 1
        grot `hasWeapons` []
        painboy `hasCount` 1
        painboy `hasWeapons` ["Power Klaw", "'Urty Syringe"]
      it "creates Thunderfire correctly" $ do
        unit <- processUnit "ThunderfireCannon"
        unit `hasGroups` 2
        let [cannon, techmarine] = _subGroups unit
        techmarine `hasCount` 1
        techmarine `hasWeapons` ["Bolt pistol", "Flamer", "Plasma cutter, Standard", "Plasma cutter, Supercharge", "Servo-arm"]
        hasStat techmarine _strength "4"
        cannon `hasCount` 1
        cannon `hasWeapons` ["Thunderfire Cannon"]
        hasStat cannon _strength "3"
      it "creates Ravenwing Bikes correctly" $ do
        unit <- processUnit "RavenwingBikeSquad"
        unit `hasGroups` 4
        let [biker1, biker2, attackbike, sargeant] = _subGroups unit
        sargeant `hasCount` 1
        sargeant `hasWeapons` ["Bolt pistol", "Frag grenade", "Krak grenade", "Twin boltgun"]
        hasStat sargeant _leadership "8"
        biker1 `isEquivalent` biker2
        biker1 `hasCount` 1
        biker1 `hasWeapons` ["Bolt pistol", "Frag grenade", "Krak grenade", "Twin boltgun"]
        hasStat biker1 _leadership "7"
        attackbike `hasCount` 1
        attackbike `hasWeapons` ["Heavy bolter", "Bolt pistol", "Twin boltgun"]
      it "creates Wulfen correctly" $ do
        unit <- processUnit "Wulfen"
        unit `hasGroups` 3
        let [frostaxes, thunderhammers, leader] = _subGroups unit
        frostaxes `hasCount` 2
        frostaxes `hasWeapons` ["Great frost axe"]
        thunderhammers `hasCount` 2
        thunderhammers `hasWeapons` ["Thunder hammer"]
        leader `hasCount` 1
        leader `hasWeapons` ["Frost claws"]
      it "creates Plague Marines correctly" $ do
        unit <- processUnit "PlagueMarines"
        unit `hasGroups` 9
        unit `hasUnitAbilities` ["Hateful Assault", "Death to the False Emperor", "Disgustingly Resilient", "Vectors of Death and Disease"]
        let [flail1, flail2, gun, special, knife, axe1, axe2, axe3, champ] = _subGroups unit
        areEquivalent [axe1,axe2,axe3]
        champ `hasCount` 1
        champ `hasWeapons` ["Plasma gun, Standard","Plasma gun, Supercharge", "Plaguesword", "Krak grenade", "Blight Grenade"]
        hasStat champ _leadership "8"
        flail1 `hasCount` 1
        flail1 `hasAbilityNamed` "Icon of Despair"
        flail1 `hasWeapons` ["Flail of Corruption", "Plague knife", "Krak grenade", "Blight Grenade"]
        flail2 `hasCount` 1
        flail2 `hasWeapons` ["Flail of Corruption", "Plague knife", "Krak grenade", "Blight Grenade"]
        axe1 `hasCount` 1
        axe1 `hasWeapons` ["Bubotic Axe", "Plague knife", "Krak grenade", "Blight Grenade"]
        knife `hasCount` 1
        knife `hasWeapons` ["Plague knife", "Krak grenade", "Blight Grenade"]
        gun `hasCount` 1
        gun `hasWeapons` ["Boltgun", "Plague knife", "Krak grenade", "Blight Grenade"]
        special `hasCount` 1
        special `hasWeapons` ["Plague Spewer", "Plague knife", "Krak grenade", "Blight Grenade"]
      it "creates Bloodletters correctly" $ do
        unit <- processUnit "Bloodletters"
        printUnits [unit]
        unit `hasGroups` 4
        unit `hasUnitAbilities` ["Daemonic", "Unstoppable Ferocity", "Murderous Tide","Daemonic Ritual"]
        let [icon, letters, instrument, leader] = _subGroups unit
        letters `hasCount` 7
        letters `hasWeapons` ["Hellblade"]
        letters `hasAbilities` []
        hasStat letters _attacks "1"
        icon `hasCount` 1
        icon `hasWeapons` ["Hellblade"]
        icon `hasAbilities` ["Daemonic Icon"]
        instrument `hasCount` 1
        instrument `hasWeapons` ["Hellblade"]
        instrument `hasAbilities` ["Instrument of Chaos"]
        leader `hasCount` 1        
        leader `hasWeapons` ["Hellblade"]
        hasStat leader _attacks "2"
      it "create Triarch Praetorians correctly" $ do
        units <- processUnits "TriarchPraetorians" 2
        printUnits units
        let [voidbladeUnit, rodUnit] = units
        let [voidblade] = _subGroups voidbladeUnit
        voidblade `hasCount` 5
        voidblade `hasWeapons` ["Voidblade", "Particle Caster"]
        let [rod] = _subGroups rodUnit
        rod `hasCount` 5
        rod `hasWeapons` ["Rod of Covenant (Melee)", "Rod of Covenant (Shooting)"]

