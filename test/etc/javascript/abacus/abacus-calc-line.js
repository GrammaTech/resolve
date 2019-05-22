#!/usr/bin/node
const path = require('path')

function line(number){
  let num = parseInt(number)
  for(var i=0; i < num; i++){
    process.stdout.write("X");
  }
  for(var i=0; i < 7; i++){
    process.stdout.write(".")
  }
  for(var i=0; i < (9 - num); i++){
    process.stdout.write("X");
  }
  console.log("")
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
