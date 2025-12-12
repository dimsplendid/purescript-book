module Test.MySolutions
  ( findEntryByStreet
  , isInBook
  , removeDuplicates
  )
  where

import Prelude

import Data.AddressBook (Entry, AddressBook, findEntry)
import Data.List (filter, head, nubByEq)
import Data.Maybe (Maybe, isJust)

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet streetName = head <<< filter isStreet -- there's "streetName" I don't know how to remove
    where
        isStreet :: Entry -> Boolean
        isStreet =  eq streetName <<< _.address.street

-- isInBook :: String -> String -> AddressBook -> Boolean
-- isInBook firstName lastName addressBook = case findEntry firstName lastName addressBook of
--     Just _ -> true
--     Nothing -> false
    
isInBook :: String -> String -> AddressBook -> Boolean
isInBook = map (map isJust) <<< findEntry

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq isSameName
    where
        isSameName :: Entry -> Entry -> Boolean
        isSameName e1 e2 = 
            e1.firstName == e2.firstName &&
            e1.lastName == e2.lastName