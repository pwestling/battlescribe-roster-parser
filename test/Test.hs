{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

import Test.Hspec
import Types
import RosterProcessing (processRoster)
import           Codec.Archive.Zip
import qualified Data.ByteString.Lazy                 as B
import qualified Data.ByteString.Lazy.Char8           as C8
import           Data.Aeson


{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

unzipRoster :: String -> IO String
unzipRoster fileName = do
  roster <- B.readFile ("test_rosters/" ++ fileName ++ ".rosz")
  let zipEntries = zEntries $ toArchive roster
  let onlyEntry = head zipEntries
  return (C8.unpack $ fromEntry onlyEntry)


processUnit :: String -> IO Unit
processUnit fileName = do
   roster <- unzipRoster fileName
   units <- processRoster (ScriptOptions True Nothing Nothing) roster "abc" 
   shouldSatisfy (fmap _unitName units) (\u -> length u == 1)
   return (head units)

processUnits :: String -> Int ->  IO [Unit]
processUnits fileName count = do
   roster <- unzipRoster fileName
   units <- processRoster (ScriptOptions True Nothing Nothing) roster "abc" 
   shouldSatisfy (fmap _unitName units) (\u -> length u == count)
   return units


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

hasAbilities :: ModelGroup -> [String] -> IO ()
hasAbilities unit abilities = do
  print $ fmap _abilityName  (_abilities unit)
  mapM_ (hasAbilityNamed unit) abilities
  length (_abilities unit) `shouldBe` length abilities

hasCount :: ModelGroup -> Int -> IO ()
hasCount group count = do
  _modelCount group `shouldBe` count

hasGroups :: Unit -> Int -> IO()
hasGroups unit count = do
    print $ fmap _name (_subGroups unit)
    shouldSatisfy (fmap _name (_subGroups unit)) (\u -> length u == count)

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
        let [heavyWeapon, terminators, sorcerer] = _subGroups unit
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
        unit <- processUnit "Hellblasters"
        unit `hasGroups` 2
        let [blasters, sargeant] = _subGroups unit
        blasters `hasCount` 4
        sargeant `hasCount` 1
        blasters `hasWeapons` ["Plasma incinerator, Standard","Plasma incinerator, Supercharge", "Bolt pistol", "Frag grenade", "Krak grenade"]
        sargeant `hasWeapons` ["Plasma incinerator, Standard","Plasma incinerator, Supercharge", "Bolt pistol", "Frag grenade", "Krak grenade"]
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
        let [maws, normal] = _subGroups unit
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
        units <- processUnits "BigMeks" 3
        let [oiler, noOiler, forceField] = units
        forceField `hasGroups` 1
        let [forceFieldModel] = _subGroups forceField
        forceFieldModel `hasCount` 1
        forceFieldModel `hasWeapons` ["Choppa", "Slugga", "Stikkbomb"] 
        forceFieldModel `hasAbilities` ["Big Mekaniak","Kustom Force Field"]
        noOiler `hasGroups` 1
        let [noOilerModel] = _subGroups noOiler
        noOilerModel `hasCount` 1
        noOilerModel `hasWeapons` ["Shokk Attack Gun", "Stikkbomb", "Killsaw"] 
        noOilerModel `hasAbilities` ["Big Mekaniak"]
        oiler `hasGroups` 2
        let [grot, mek] = _subGroups oiler
        mek `hasCount` 1
        mek `hasWeapons` ["Shokk Attack Gun", "Stikkbomb", "Killsaw"] 
        mek `hasAbilities` ["Big Mekaniak"]
        grot `hasCount` 1
        grot `hasWeapons` []
        grot `hasAbilities` ["Grot Oiler"]