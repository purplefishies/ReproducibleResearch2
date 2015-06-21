#!/bin/bash


function livedat() { 
    /bin/rm -f stormdata.csv
    ln -s actualdata.csv stormdata.csv
}


function dummydat() {
    /bin/rm -f stormdata.csv
    ln -s dummydata.csv stormdata.csv
}
