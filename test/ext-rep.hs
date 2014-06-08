{-# LANGUAGE UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Test where

import Prelude.Unicode
import Control.Arrow.Unicode
import Text.Heredoc
import Text.XML.HXT.Core

import HxtUtil
import TreeUtil

import Diff.ObjectTree
import ExtRep.XmlToObjectTree



xt1 = [here|
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

showX = xrun this
showP = printNTree ∘ fromXml ∘ head ∘ run (hasName "root")

show_xt1 = showX xt1
show_parse_xt1 = showP xt1

show_xt2 = showX xt2
show_parse_xt2 = showP xt2
