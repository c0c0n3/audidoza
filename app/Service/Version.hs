{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes #-}
module Service.Version (getVersionR) where

import Prelude.Unicode
import Data.Acid.Advanced
import Data.Maybe
import Text.Blaze.Html (toHtml)
import Yesod

import Audit.EditAction
import Audit.ObjectHistory
import Audit.VersionedChange
import Db.AuditStore
import ExtRep.DiffTreeToHtml
import Service.AuditService
import Util.SequentialId
import Util.Time




getVersionR ∷ AuditId → Handlr Html
getVersionR auditId = do
                    mv ← findVersion auditId
                    case mv of
                         Just v  → return $ renderVersion v undefined
                         Nothing → return $ renderNothing undefined


findVersion ∷ AuditId → Handlr (Maybe VersionedObject)
findVersion auditId = do 
                    app ← getYesod
                    v   ← query' (db app) $ SelectVersions [auditId]
                    return ∘ listToMaybe $ v 

renderDiff ∷ VersionedObject → Html
renderDiff = toHtml ∘ describeChangeTree ∘ editAction

-- TODO: this should go in an external file.
-- TODO: make style sheet URL type-safe.
-- TODO: sort out style sheet generation from DiffTreeCss. 
renderVersion ∷ VersionedObject → ξ → Html
renderVersion v = [hamlet|
$doctype 5
<html>
  <head>
    <title>Diff Tree
    <link rel="stylesheet" type="text/css" href="diff-tree.css">
  <body>
    <div class="diffView">
      <div class="diffHeader">
        <span class="username" title="User who made the change.">#{username $ editAction v}
        <span class="timestamp" title="When changed.">#{showDateTime $ timeOfChange $ editAction v}
        <span class="version" title="Change version number.">#{toText $ version v}
      <div class="diffTree">
        #{renderDiff v}   
|]

renderNothing ∷ ξ → Html
renderNothing = [hamlet|
$doctype 5
<html>
  <head>
    <title>No Version Found
  <body>
    <p>No Version Found

|]
