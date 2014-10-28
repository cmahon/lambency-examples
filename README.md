# Lambency examples

Experimenting with the [Lambency](https://github.com/Mokosha/Lambency) library. Currently using a [modified version](https://github.com/cmahon/Lambency/tree/exportall) that exposes all modules.

## Setup 

Tested on GHC 7.8.3:

    git clone https://github.com/cmahon/Lambency.git
    cd Lambency
    git checkout -b exposeall
    cd ..
    git clone https://github.com/cmahon/lambency-examples.git
    cd lambency-examples
    cabal sandbox init
    cabal sandbox add-source ../Lambency
    cabal install --only-dependencies
    cabal configure
    cabal install

    cabal run asteroids

## Asteroids controls

* Turn ship: Left, Right
* Thrust: Up
* Fire: Space

## Asteroids to-do list

* Random polygons for asteroids
* Asteroids explode into smaller parts
* Particle system for explosions and ship engine
* Ship / asteroid collisions
* Sound



