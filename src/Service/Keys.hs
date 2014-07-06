{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes #-}
module Service.Keys (getKeysR) where

import Prelude.Unicode
import Data.Acid.Advanced
import Data.List
import Yesod

import Db.AuditStore
import Db.HistoryQueries (VersionsCount)
import Service.AuditService
import Service.Routes
import Util.EntityKey




getKeysR ∷ Handlr Html
getKeysR = do
         ksv ← listEntityKeys
         defaultLayout $ renderKeys ksv
         --return $ renderKeys ksv renderRoute


listEntityKeys ∷ Handlr [(EntityKey, VersionsCount)]
listEntityKeys = do 
               app ← getYesod
               ks  ← query' (db app) AllEntityKeys
               return ∘ sort $ ks

renderKeys ksv = [whamlet|
    <div class="keysListView">
      <ul class="keysList">
        $forall (k, count) <- ksv
          <li class="key">
            <a href=@{HistoryLineR k 100}
               title="View last 100 versions.">
               <span class="className">#{className k}
               <span class="entityId">ID: #{entityId k}
               <span class="versionsCount">(versions count: #{count})
|]

{-
renderKeys ∷ [(EntityKey, VersionsCount)] → (Route AuditService → [ξ] → Html) 
           → Html
renderKeys ksv = [hamlet|
$doctype 5
<html>
  <head>
    <title>Audited Objects
  <body>
    <div class="keysListView">
      <ul class="keysList">
        $forall (k, count) <- ksv
          <li class="key">
            <a href=@{HistoryLineR k 100}
               title="View last 100 versions.">
               <span class="className">#{className k}
               <span class="entityId">ID: #{entityId k}
               <span class="versionsCount">(versions count: #{count})
|]
-}
