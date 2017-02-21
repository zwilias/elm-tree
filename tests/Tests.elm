module Tests exposing (..)

import Test exposing (..)
import Tree.AATest
import Tree.AVLTest
import Tree.TwoThreeTest
import Dict.AATest
import Dict.AVLTest
import Dict.TwoThreeTest
import Set.AATest
import Set.AVLTest
import Set.TwoThreeTest


all : Test
all =
    describe "All tests"
        [ Tree.AATest.all
        , Tree.AVLTest.all
        , Tree.TwoThreeTest.all
        , Dict.AATest.tests
        , Dict.AVLTest.tests
        , Dict.TwoThreeTest.tests
        , Set.AATest.tests
        , Set.AVLTest.tests
        , Set.TwoThreeTest.tests
        ]
