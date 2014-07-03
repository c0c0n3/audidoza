{-# LANGUAGE UnicodeSyntax, QuasiQuotes #-}
module XmlData where

import Prelude.Unicode
--import Data.Text (Text)
import Data.Text.Lazy (Text, toStrict)
import Text.Heredoc
import Text.Shakespeare.Text


et1 = [here|
    <?xml version="1.0"?> <!-- parse error, doc begins with whitespace -->
    <root> 
        <id>1</id>
        <r1>r1</r1>
    </root>
|]

xt1 = [here|<?xml version="1.0"?>
    <root>
        <id>1</id>
        <r1>r1</r1>
        <X>
            <id>1</id>
            <x1>x1</x1>
            <x2>x2</x2>
            <W>
                <id>1</id>
            </W>
        </X>
        <Y>
            <id>1</id>
            <y1>y1</y1>
            <Z>
                <id>1</id>
                <z1>z1</z1>
                <z2>z2</z2>
            </Z>
        </Y>
    </root>
|]

xt2 = [here|
    <root>
        <id>1</id>
        <r1>r1 (mod)</r1>
        <!-- seqOfX -->
            <X>
                <id>1</id>
                <x1>x1</x1>
                <x2>x2 (mod)</x2>
                <x3>new</x3>
                <W>
                    <id>1</id>
                    <w1>new</w1>
                </W>
            </X>
        <!-- /seqOfX -->
        <seqOfY>
            <Y>
                <id>1</id>
                <y2>new</y2>
                <seqOfZ>
                    <Z>
                        <id>1</id>
                        <z1>z1</z1>
                        <z2>z2</z2>
                    </Z>
                </seqOfZ>
            </Y>
            <Y>
                <id>2</id>
                <y2>y2</y2>
            </Y>
        </seqOfY>
    </root>
|]


mkAuditXml ∷ String → Int → String → Int → Text → Text
mkAuditXml usr millis className entityId contentXml = [lt|<?xml version="1.0"?>
    <editAction>
        <userName>#{usr}</userName>
        <timeOfChange>#{millis}</timeOfChange> <!-- millis from epoc -->
        <entityKey>
            <className>#{className}</className>
            <persistentId>#{entityId}</persistentId>
        </entityKey>
        #{contentXml}
    </editAction>
|]

mkNewContentXml ∷ String → Text
mkNewContentXml x = [lt|
    <newContent>
        <newState>
            #{x}
        </newState>
    </newContent>
|]

mkModContentXml ∷ String → String → Text
mkModContentXml x x' = [lt|
    <modifiedContent>
        <oldState>
            #{x}
        </oldState>
        <newState>
            #{x'}
        </newState>
    </modifiedContent>
|]

mkDelContentXml ∷ String → Text
mkDelContentXml x = [lt|
    <deletedContent>
        <oldState>
            #{x}
        </oldState>
    </deletedContent>
|]

xuc1 = toStrict $ 
       mkAuditXml "u1" 10 "root" 1 $ mkNewContentXml (drop 22 xt1)  -- gets rid of xml decl in xt1
xuc2 = toStrict $
       mkAuditXml "u2" 20 "root" 1 $ mkModContentXml (drop 22 xt1) xt2
xuc3 = toStrict $
       mkAuditXml "u3" 30 "root" 1 $ mkDelContentXml xt2 

