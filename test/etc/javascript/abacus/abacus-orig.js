#!/usr/bin/node
const path = require('path')

function line(number){
  switch(number){
  case "0": console.log('.......XXXXXXXXX'); break;
  case "1": console.log('X.......XXXXXXXX'); break;  
  case "2": console.log('XX.......XXXXXXX'); break;  
  case "3": console.log('XXX.......XXXXXX'); break;  
  case "4": console.log('XXXX.......XXXXX'); break;  
  case "5": console.log('XXXXX.......XXXX'); break;  
  case "6": console.log('XXXXXX.......XXX'); break;  
  case "7": console.log('XXXXXXX.......XX'); break;  
  case "8": console.log('XXXXXXXX.......X'); break;  
  case "9": console.log('XXXXXXXXX.......'); break;  
  }
}

function board(number){
  number.split("").reverse().forEach(function(digit){
    line(digit)
  })
}

number = process.argv[2]
if(number){
  board(number)
} else {
  console.log(`Usage: ${path.basename(process.argv[1])} NUMBER`)
}
