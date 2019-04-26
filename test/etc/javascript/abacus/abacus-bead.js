#!/usr/bin/node
const path = require('path')

function line(number){
  switch(number){
  case "0": console.log('.......∘∘∘∘∘∘∘∘∘'); break;
  case "1": console.log('∘.......∘∘∘∘∘∘∘∘'); break;  
  case "2": console.log('∘∘.......∘∘∘∘∘∘∘'); break;  
  case "3": console.log('∘∘∘.......∘∘∘∘∘∘'); break;  
  case "4": console.log('∘∘∘∘.......∘∘∘∘∘'); break;  
  case "5": console.log('∘∘∘∘∘.......∘∘∘∘'); break;  
  case "6": console.log('∘∘∘∘∘∘.......∘∘∘'); break;  
  case "7": console.log('∘∘∘∘∘∘∘.......∘∘'); break;  
  case "8": console.log('∘∘∘∘∘∘∘∘.......∘'); break;  
  case "9": console.log('∘∘∘∘∘∘∘∘∘.......'); break;  
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
