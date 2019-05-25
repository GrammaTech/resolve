#!/usr/bin/node
const path = require('path')

function line(number){
  switch(number){
  case "0": console.log('.......OOOOOOOOO'); break;
  case "1": console.log('O.......OOOOOOOO'); break;
  case "2": console.log('OO.......OOOOOOO'); break;
  case "3": console.log('OOO.......OOOOOO'); break;
  case "4": console.log('OOOO.......OOOOO'); break;
  case "5": console.log('OOOOO.......OOOO'); break;
  case "6": console.log('OOOOOO.......OOO'); break;
  case "7": console.log('OOOOOOO.......OO'); break;
  case "8": console.log('OOOOOOOO.......O'); break;
  case "9": console.log('OOOOOOOOO.......'); break;
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
