{-# LANGUAGE UnicodeSyntax, QuasiQuotes #-}
module XmlData where

import Prelude.Unicode
import Text.Heredoc



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

