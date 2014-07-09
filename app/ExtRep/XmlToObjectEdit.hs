{-# LANGUAGE UnicodeSyntax #-}
module ExtRep.XmlToObjectEdit (parseObjectEdit) where

import BaseImport
import Data.Maybe
import Data.Time
import Text.XML.HXT.Core

import Audit.EditAction
import Audit.ObjectHistory
import Util.EntityKey
import Util.Hxt
import Util.Time


-- TODO: this will work 100% as long as we get the expected input, so
--       ⋅ make XSD schema
--       ⋅ validate against schema

parseObjectEdit ∷ Text → Either Text ObjectEdit
parseObjectEdit = parseDocRoot $ toEither parser
    where
    parser = new <+> modified <+> deleted


new ∷ ArrowXml hom ⇒ hom XmlTree ObjectEdit
new = (findTag "newContent") `guards` 
      (metadata &&& state NewS ⋙ arr2 make)
    where
    make (usr, (tyme, key)) content = newContent usr tyme key content

deleted ∷ ArrowXml hom ⇒ hom XmlTree ObjectEdit
deleted = (findTag "deletedContent") `guards` 
          (metadata &&& state OldS ⋙ arr2 make)
    where
    make (usr, (tyme, key)) content = delContent usr tyme key content

modified ∷ ArrowXml hom ⇒ hom XmlTree ObjectEdit
modified = (findTag "modifiedContent") `guards` 
           (metadata &&& state OldS &&& state NewS ⋙ arr3 make)
    where
    make (usr, (tyme, key)) prev cur = modContent usr tyme key prev cur

user ∷ ArrowXml hom ⇒ hom XmlTree Text
user = textOf "userName" 

time ∷ ArrowXml hom ⇒ hom XmlTree  UTCTime
time = integerOf "timeOfChange" ⋙ arr millisFromEpoc

entityKey ∷ ArrowXml hom ⇒ hom XmlTree EntityKey
entityKey = findTag "entityKey" 
          ⋙ (deep $ textOf "className") &&& (deep $ integerOf "persistentId")
          ⋙ arr2 mkEntityKey
          >>. catMaybes

metadata ∷ ArrowXml hom ⇒ hom XmlTree (Text, (UTCTime, EntityKey))
metadata = deep user &&& deep time &&& deep entityKey

data WhichState = NewS | OldS

state ∷ ArrowXml hom ⇒ WhichState → hom XmlTree Text
state which = findTag (tag which) ⋙ innerXml
    where
    tag NewS = "newState"
    tag OldS = "oldState"
