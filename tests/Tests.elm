module Tests exposing (..)

import Test exposing (..)
import Tree.AATest
import Tree.AVLTest
import Tree.TwoThreeTest


all : Test
all =
    describe "All tests"
        [ Tree.AATest.all
        , Tree.AVLTest.all
        , Tree.TwoThreeTest.all
        ]
