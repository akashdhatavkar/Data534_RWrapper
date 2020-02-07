[![Build Status](https://travis-ci.org/akashdhatavkar/Data534_RWrapper.svg?branch=master)](https://travis-ci.org/akashdhatavkar/Data534_RWrapper)

## fireballNASA

The objective of fireNASA is to provide the user with a clean consumable
data from NASA’s fireball API based on the users criteria

## Installation

`install.packages("fireNASA")`

## Added Features
The `fireball_data` function will also provide the users with the a "Country"
column, so that the users can use this information to views fireballs by 
country. In addition to this, the date column, has been split into seperate
Date and Time columns, so that the user doesn't have to worry about converting
the column in the right format and can instead focus on the analysis they had 
in mind

## Example
`fireNASA::fireball_data()`
`fireNASA::fireball_data(date_min = "2015-01-01", lim = 50)`
