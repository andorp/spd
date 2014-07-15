module Main where

{-
4.6 Designing With Itemizations
-}

import SPD.Test

{-
Sample Problem:
The state of Tax Land has created a three-stage sales tax to cope with its budget deficit.
* Inexpensive items, those costing less than $1,000, are not taxed.
* Luxury items, with a price of more than $10,000, are taxed at the rate of eight percent (8.00%).
* Everything in between comes with a five percent (5.25%) price tag.

Design a function for a cash register that computes the sales tax for each item.
That is, design a function that, given the price of an item,
computes the amount of tax to be charged.
-}

-- The Tax price falls into on eof the three intervals:
-- * 0 through 1000
-- * 1000 through 10000
-- * 10000 and above

type Price = Double

-- * Constants

normalTax = 5.25 / 100
luxoryTax = 8.00 / 100

lowPrice    = 1000
luxoryPrice = 10000

{-
data Tax = Tax (Price, Price)
  deriving (Show, Eq)
-}

salesTax :: Price -> Double
-- ^ Compute the amount of tax charged for price p
salesTax x = (tax x) * x where
  tax p
    | and [(0 <= p),    (p < lowPrice)] = 0
    | and [(lowPrice <= p), (p < luxoryPrice)] = normalTax
    | (p >= luxoryPrice) = luxoryTax

salesTaxTests = eqPartitions salesTax [
    ("0.0"              ,0               ,0                            , "")
  , ("Low price - 1"    ,lowPrice - 1    ,0                            , "")
  , ("Low price"        ,lowPrice        ,normalTax * lowPrice         , "")
  , ("Low price + 1"    ,lowPrice + 1    ,normalTax * (lowPrice + 1)   , "")
  , ("Luxory price - 1" ,luxoryPrice - 1 ,normalTax * (luxoryPrice - 1), "")
  , ("Luxory price"     ,luxoryPrice     ,luxoryTax * luxoryPrice      , "")
  , ("Luxory price + 1" ,luxoryPrice + 1 ,luxoryTax * (luxoryPrice + 1), "")
  ]
